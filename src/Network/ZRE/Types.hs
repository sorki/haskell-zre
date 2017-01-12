{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.ZRE.Types where

import Control.Monad.RWS hiding (state)
import Control.Monad.Identity
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import Data.UUID
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import Data.Time.Clock

import Data.ZRE
import System.ZMQ4.Endpoint

mCastPort = 5670 :: Port
mCastIP = "225.25.25.25"

-- send beacon every 1 second
--zreBeaconMs = 1000000

-- send beacon every 100 mseconds
zreBeaconMs = 100000 :: Int

-- send hugz after x mseconds
quietPeriod = 200000 / 1000000.0 :: NominalDiffTime
deadPeriod = 600000  / 1000000.0 :: NominalDiffTime

-- send beacon every 1 ms (much aggressive, will kill networkz)
--zreBeaconMs = 1000 :: Int
--quietPeriod = 2000 / 100000.0 :: NominalDiffTime
--deadPeriod = 6000  / 100000.0 :: NominalDiffTime

data Event =
    New Peer
  | Update Peer
  | GroupJoin Peer Group
  | GroupLeave Peer Group
  | Quit Peer
  | Message ZREMsg
--  deriving (Show)

data API =
    DoJoin Group
  | DoLeave Group
  | DoShout Group B.ByteString
  | DoWhisper Peer B.ByteString

type Peers = M.Map UUID (TVar Peer)
type PeerGroups = M.Map Group Peers

-- zreUS :: Peer ???
data ZREState = ZREState {
    zreUUID       :: UUID
  , zrePeers      :: Peers
  , zrePeerGroups :: PeerGroups
  , zreEndpoint   :: Endpoint
  , zreGroups     :: Groups
  , zreGroupSeq   :: GroupSeq
  , zreName       :: Name
  , zreHeaders    :: Headers
  , zreIn         :: TBQueue Event
  , zreOut        :: TBQueue API
  }

data Peer = Peer {
    peerEndpoint  :: Endpoint
  , peerUUID      :: UUID
  , peerSeq       :: Seq
  , peerGroups    :: Groups
  , peerGroupSeq  :: GroupSeq
  , peerName      :: Name
  , peerAsync     :: Maybe (Async ())
  , peerAsyncPing :: Maybe (Async ())
  , peerQueue     :: TBQueue ZRECmd
  , peerLastHeard :: UTCTime
  }

newtype ZRET m a = ZRE { runZRE :: RWST ZREReader [ZREWriter] ZREState m a }
  deriving (Monad,
            Functor,
            Applicative,
            MonadReader ZREReader,
            MonadWriter [ZREWriter],
            MonadState ZREState)

type ZRE = ZRET Identity

data ZREReader = ZREReader {
  readName :: B.ByteString
  }

data ZREWriter =
  Event ZREMsg
