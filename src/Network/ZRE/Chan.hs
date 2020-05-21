{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Network.ZRE.Chan (
    zreChan
  , zreChan'
  , mapToGroup
  , mapToGroup'
  ) where

import Control.Concurrent.STM (TChan)
import Data.Serialize (Serialize)
import Data.ZRE (Group, KnownGroup, knownToGroup)
import Network.ZRE (ZRE)

import qualified Control.Concurrent.Async.Lifted
import qualified Control.Concurrent.STM
import qualified Control.Monad
import qualified Control.Monad.IO.Class
import qualified Data.Serialize

import qualified Network.ZRE

-- | Map function to deserialized data type received from one group
-- and send it encoded to another group. Basically a typed proxy between
-- two groups.
mapToGroup :: forall fromGroup toGroup from to .
            ( Serialize from
            , Show from
            , Serialize to
            , KnownGroup fromGroup
            , KnownGroup toGroup
            )
          => (from -> to)  -- ^ Conversion function
          -> ZRE ()
mapToGroup fn = mapToGroup'
  (knownToGroup @fromGroup)
  (knownToGroup @toGroup)
  fn

-- | Like `mapToGroup` but with non-symbolic groups
mapToGroup' :: (Show from, Serialize from, Serialize to)
           => Group         -- ^ Group to listen to and decode its messages
           -> Group         -- ^ Group to send encoded messages to
           -> (from -> to)  -- ^ Conversion function
           -> ZRE ()
mapToGroup' fromGroup toGroup fn = do
  Network.ZRE.zjoin fromGroup
  Network.ZRE.zjoin toGroup

  Network.ZRE.zrecvShoutsDecode fromGroup Data.Serialize.decode
    $ \(mdec :: Either String from) -> do
      case mdec of
        Left e -> do
          Network.ZRE.zfail
            $ "Unable to decode message from "
            ++ show fromGroup ++ " error was: " ++ e
        Right dec -> do
          Network.ZRE.zshout toGroup $ Data.Serialize.encode $ fn dec

-- | Typed ZRE channel using two groups
--
-- * @input -> outputGroup@ for transfering encoded data
-- * @inputGroup -> output@ for receiving decoded data
--
-- Unexpected data on channel will result in error.
zreChan :: forall input output inputGroup outputGroup .
        ( Serialize input
        , Serialize output
        , KnownGroup inputGroup
        , KnownGroup outputGroup
        )
        => IO ( TChan input
              , TChan output)
zreChan = zreChan'
  (knownToGroup @outputGroup)
  (knownToGroup @inputGroup)

-- | Like `zreChan` but with non-symbolic groups
zreChan' :: (Serialize input, Serialize output)
         => Group
         -> Group
         -> IO ( TChan input
               , TChan output)
zreChan' outputGroup inputGroup = do
  chanInput  <- Control.Concurrent.STM.newTChanIO
  chanOutput <- Control.Concurrent.STM.newTChanIO

  _ <- Control.Concurrent.Async.Lifted.async $ Network.ZRE.runZre $ do

    -- joining the outputGroup is not strictly needed for
    -- shouts to pass thru, for indication only
    Network.ZRE.zjoin outputGroup

    -- shout input to outputGroup
    Control.Monad.void
      $ Control.Concurrent.Async.Lifted.async
      $ Control.Monad.forever
      $ do
        out <-
            Control.Monad.IO.Class.liftIO
          $ Control.Concurrent.STM.atomically
          $ Control.Concurrent.STM.readTChan chanInput

        Network.ZRE.zshout outputGroup
          $ Data.Serialize.encode out

    -- receive on inputGroup and forward to output
    Network.ZRE.zjoin inputGroup
    Network.ZRE.zrecvShoutsDecode inputGroup Data.Serialize.decode
      $ either
        (\e -> Network.ZRE.zfail
          $ "zreChan: Unable to decode message from input "
          ++ show inputGroup
          ++ " error was: "
          ++ e
        )
        ( Control.Monad.IO.Class.liftIO
        . Control.Concurrent.STM.atomically
        . Control.Concurrent.STM.writeTChan chanOutput
        )

  return (chanInput, chanOutput)
