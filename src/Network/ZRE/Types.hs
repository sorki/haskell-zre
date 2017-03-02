{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Network.ZRE.Types where

import Control.Monad.RWS hiding (state)
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import Data.UUID
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as B
import Data.Time.Clock

import System.IO (Handle)

import Data.ZRE
import System.ZMQ4.Endpoint

mCastPort = 5670 :: Port
mCastIP = "225.25.25.25"

--sec :: (Num a, Fractional b) => a -> b
sec  = (*1000000)
msec = (*1000)

-- send beacon every 1 second
--zreBeaconMs = 1000000

-- send beacon every 0.9 seconds
zreBeaconMs = 900000 :: Int

-- send hugz after x mseconds
-- agressive
--quietPeriod = (msec 200) / 1000000.0 :: NominalDiffTime
--deadPeriod = (msec 600) / 1000000.0 :: NominalDiffTime

-- lazy
quietPeriod = (sec 1) / 1000000.0 :: NominalDiffTime
deadPeriod = (sec 5)  / 1000000.0 :: NominalDiffTime

quietPingRate = round (sec 1) :: Int

-- send beacon every 1 ms (much aggressive, will kill networkz)
--zreBeaconMs = 1000 :: Int
--quietPeriod = 2000 / 100000.0 :: NominalDiffTime
--deadPeriod = 6000  / 100000.0 :: NominalDiffTime

data Event =
    New Peer
  | Ready Peer
  | GroupJoin Peer Group
  | GroupLeave Peer Group
  | Quit Peer
  | Message ZREMsg
  -- TODO: rewrite dbg to emit $ Debug msg
  deriving (Show)

data API =
    DoJoin Group
  | DoLeave Group
  | DoShout Group B.ByteString
  | DoShoutMulti Group [B.ByteString]
  | DoWhisper UUID B.ByteString
  | DoDiscover UUID Endpoint
  deriving (Show)

type Peers = M.Map UUID (TVar Peer)
type PeerGroups = M.Map Group Peers

type EventQueue = TBQueue Event
type APIQueue = TBQueue API


data ZREState = ZREState {
    zreUUID       :: UUID
  , zrePeers      :: Peers
  , zrePeerGroups :: PeerGroups
  , zreEndpoint   :: Endpoint
  , zreGroups     :: Groups
  , zreGroupSeq   :: GroupSeq
  , zreName       :: Name
  , zreHeaders    :: Headers
  , zreIn         :: EventQueue
  , zreOut        :: APIQueue
  }

data Peer = Peer {
    peerEndpoint  :: Endpoint
  , peerUUID      :: UUID
  , peerSeq       :: Seq
  , peerGroups    :: Groups
  , peerGroupSeq  :: GroupSeq
  , peerName      :: Maybe Name
  , peerAsync     :: Maybe (Async ())
  , peerAsyncPing :: Maybe (Async ())
  , peerQueue     :: TBQueue ZRECmd
  , peerLastHeard :: UTCTime
  }
  deriving (Show)

instance Show a => Show (TBQueue a) where
  show = pure "TBQueue"

instance Show a => Show (Async a) where
  show = pure "Async"

newtype ZRE a = Z {
  runZ' :: ReaderT (EventQueue, APIQueue) IO a
}
  deriving (Functor, Applicative, Monad, MonadIO,
    MonadBase IO,
--    MonadBaseControl IO,
    MonadReader (EventQueue, APIQueue))

--type instance StM ZRE a = a

instance MonadBaseControl IO ZRE where
  type StM ZRE a = a
  liftBaseWith f = Z $ liftBaseWith $ \q -> f (q . runZ')
  restoreM = Z . restoreM

runZ app events api = runReaderT (runZ' app) (events, api)

readZ :: ZRE (Event)
readZ = do
  (e, _) <- ask
  v <- liftIO $ atomically $ readTBQueue e
  return v

writeZ :: API -> ZRE ()
writeZ x = do
  (_, a) <- ask
  liftIO $ atomically $ writeTBQueue a x

readZreQueue inQ = atomically $ readTBQueue inQ
writeZreQueue outQ x = atomically $ writeTBQueue outQ x
concurrentZre recv act = runConcurrently $ Concurrently (recv) *> Concurrently (act)

zjoin = writeZ . DoJoin
zleave = writeZ . DoLeave
zshout group msg = writeZ $ DoShout group msg
zshout' group msgs = writeZ $ DoShoutMulti group msgs
zwhisper uuid msg = writeZ $ DoWhisper uuid msg

join = DoJoin
leave = DoLeave
shout = DoShout
shout' = DoShoutMulti
whisper = DoWhisper

maybeM err f value = value >>= maybe err f

newZREState name endpoint u inQ outQ = atomically $ newTVar $
  ZREState {
    zreUUID = u
    , zrePeers = M.empty
    , zrePeerGroups = M.empty
    , zreEndpoint = endpoint
    , zreGroups = Set.empty
    , zreGroupSeq = 0
    , zreName = name
    , zreHeaders = M.empty
    , zreIn = inQ
    , zreOut = outQ }
