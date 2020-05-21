{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.ZRE.Types where

import Control.Monad.Reader
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.UUID
import Data.Time.Clock
import Data.Default

import Data.ZRE hiding (Shout, Whisper) -- (Name, Seq, Group, Groups, GroupSeq, Headers, Content, ZRECmd, ZREMsg)
import System.ZMQ4.Endpoint


isec :: (Num a) => a -> a
isec  = (*1000000)
sec  :: (RealFrac a) => a -> Int
sec  = round . isec
msec :: (RealFrac a) => a -> Int
msec = round . (*1000)

-- send beacon every 0.9 seconds
zreBeaconMs :: Int
zreBeaconMs = 900000

-- send hugz after x mseconds
-- agressive
--quietPeriod = (msec 200) / 1000000.0 :: NominalDiffTime
--deadPeriod = (msec 600) / 1000000.0 :: NominalDiffTime

-- lazy
quietPeriod :: NominalDiffTime
quietPeriod = (fromIntegral $ sec (1.0 :: Float)) / 1000000.0

deadPeriod :: NominalDiffTime
deadPeriod = (fromIntegral $ sec (5.0 :: Float))  / 1000000.0

quietPingRate :: Int
quietPingRate = sec (1.0 :: Float)

-- send beacon every 1 ms (much aggressive, will kill networkz)
--zreBeaconMs = 1000 :: Int
--quietPeriod = 2000 / 100000.0 :: NominalDiffTime
--deadPeriod = 6000  / 100000.0 :: NominalDiffTime

data ZRECfg = ZRECfg {
    zreNamed        :: ByteString
  , zreQuietPeriod  :: Int
  , zreDeadPeriod   :: Int
  , zreBeaconPeriod :: Int
  , zreInterfaces   :: [ByteString]
  , zreMCast        :: Endpoint
  , zreZGossip      :: Maybe Endpoint
  , zreDbg          :: Bool
  } deriving (Show)

defMCastEndpoint :: Endpoint
defMCastEndpoint = newUDPEndpoint "225.25.25.25" 5670

defaultConf :: ZRECfg
defaultConf = ZRECfg {
    zreNamed        = "zre"
  , zreQuietPeriod  = sec (1.0 :: Float)
  , zreDeadPeriod   = sec (5.0 :: Float)
  , zreBeaconPeriod = sec (0.9 :: Float)
  , zreInterfaces   = []
  , zreZGossip      = Nothing
  , zreMCast        = defMCastEndpoint
  , zreDbg          = False
  }

instance Default ZRECfg where
  def = defaultConf

data Event =
    New UUID (Maybe Name) Groups Headers Endpoint
  | Ready UUID Name Groups Headers Endpoint
  | GroupJoin UUID Group
  | GroupLeave UUID Group
  | Quit UUID (Maybe Name)
  | Message ZREMsg
  | Shout UUID Group Content UTCTime
  | Whisper UUID Content UTCTime
  | Debug ByteString
  deriving (Show)

data API =
    DoJoin Group
  | DoLeave Group
  | DoShout Group ByteString
  | DoShoutMulti Group [ByteString]
  | DoWhisper UUID ByteString
  | DoDiscover UUID Endpoint
  | DoDebug Bool
  | DoQuit
  deriving (Show)

type Peers = Map UUID (TVar Peer)
type PeerGroups = Map Group Peers

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
  , zreDebug      :: Bool
  , zreIn         :: EventQueue
  , zreOut        :: APIQueue
  , zreIfaces     :: Map ByteString [Async ()]
  }

data Peer = Peer {
    peerEndpoint  :: Endpoint
  , peerUUID      :: UUID
  , peerSeq       :: Seq
  , peerGroups    :: Groups
  , peerGroupSeq  :: GroupSeq
  , peerName      :: Maybe Name
  , peerHeaders   :: Headers
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
  } deriving (
      Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadBase IO
    , MonadReader (EventQueue, APIQueue))

instance MonadBaseControl IO ZRE where
  type StM ZRE a = a
  liftBaseWith f = Z $ liftBaseWith $ \q -> f (q . runZ')
  restoreM = Z . restoreM

runZ :: ZRE a -> EventQueue -> APIQueue -> IO a
runZ app events api = runReaderT (runZ' app) (events, api)

readZ :: ZRE (Event)
readZ = do
  (e, _) <- ask
  v <- liftIO $ atomically $ readTBQueue e
  return v

unReadZ :: Event -> ZRE ()
unReadZ x = do
  (e, _) <- ask
  void $ liftIO $ atomically $ unGetTBQueue e x

writeZ :: API -> ZRE ()
writeZ x = do
  (_, a) <- ask
  liftIO $ atomically $ writeTBQueue a x

getEventQueue :: ZRE (EventQueue)
getEventQueue = ask >>= return . fst
getApiQueue :: ZRE (APIQueue)
getApiQueue = ask >>= return . snd

zjoin :: Group -> ZRE ()
zjoin = writeZ . DoJoin

zleave :: Group -> ZRE ()
zleave = writeZ . DoLeave

zshout :: Group -> ByteString -> ZRE ()
zshout group msg = writeZ $ DoShout group msg

zshout' :: Group -> [ByteString] -> ZRE ()
zshout' group msgs = writeZ $ DoShoutMulti group msgs

zwhisper :: UUID -> ByteString -> ZRE ()
zwhisper uuid msg = writeZ $ DoWhisper uuid msg

zdebug :: ZRE ()
zdebug = writeZ $ DoDebug True

znodebug :: ZRE ()
znodebug = writeZ $ DoDebug False

zquit :: ZRE ()
zquit = writeZ $ DoQuit

zfail :: String -> ZRE ()
zfail errorMsg = do
  liftIO $ putStrLn errorMsg
  writeZ $ DoQuit

zrecv :: ZRE (Event)
zrecv = readZ

maybeM :: Monad m => m b -> (a -> m b) -> m (Maybe a) -> m b
maybeM err f value = value >>= maybe err f

newZREState :: Name
            -> Endpoint
            -> UUID
            -> EventQueue
            -> APIQueue
            -> Bool
            -> IO (TVar ZREState)
newZREState name endpoint u inQ outQ dbg = atomically $ newTVar $
  ZREState {
    zreUUID = u
    , zrePeers = mempty
    , zrePeerGroups = mempty
    , zreEndpoint = endpoint
    , zreGroups = mempty
    , zreGroupSeq = 0
    , zreName = name
    , zreHeaders = mempty
    , zreDebug = dbg
    , zreIn = inQ
    , zreOut = outQ
    , zreIfaces = mempty
    }
