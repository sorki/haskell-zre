module Network.ZRE.Chan (
    zreChan
  ) where

import Control.Concurrent.STM (TChan)
import Data.Serialize (Serialize)
import Data.ZRE (Group)

import qualified Control.Concurrent
import qualified Control.Concurrent.Async.Lifted
import qualified Control.Concurrent.STM
import qualified Control.Monad
import qualified Control.Monad.IO.Class
import qualified Data.Serialize

import qualified Network.ZRE

-- | Typed ZRE channel using two groups
-- input -> outputGroup for transfering encoded data
-- inputGroup -> output for receiving decoded data
zreChan :: (Serialize input, Serialize output)
        => Group
        -> Group
        -> IO ( TChan input
              , TChan output)
zreChan outputGroup inputGroup = do
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
    Network.ZRE.zrecvShouts
      $ Network.ZRE.whenDecodes Data.Serialize.decode
      $ Control.Monad.IO.Class.liftIO
      . Control.Concurrent.STM.atomically
      . Control.Concurrent.STM.writeTChan chanOutput

  return (chanInput, chanOutput)
