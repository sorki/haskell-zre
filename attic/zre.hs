{-# LANGUAGE OverloadedStrings #-}
module Main where
import Prelude hiding (putStrLn, take)
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import Network.Multicast
import qualified System.ZMQ4.Monadic as ZMQ
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

import qualified Data.List.NonEmpty as NE -- (NonEmpty((:|)))  --as NE

import Data.Attoparsec.ByteString.Char8

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import Data.UUID
import Data.UUID.V1
import Data.Maybe

import GHC.Word


import Data.Binary.Strict.Get as G
import Data.Binary.Strict.BitGet as BG
import Data.Binary.Put
--import Data.Binary.BitPut hiding (putByteString)

import Data.Map as M

port = "5670"
mCast = "225.25.25.25"

rPort = "1234"
zmqPort = 1337

zreVer = 2 :: Int
zreSig = 0xAAA1 :: Word16


type Port = Int

data Peer = Peer {
    peerHost  :: B.ByteString
  , peerUUID  :: UUID
  , peerPort  :: Port
  , peerAsync :: Async ()
  }
--  } deriving (Show)

data Event =
  NewPeer B.ByteString UUID Port
  deriving (Show)

--type Peers = M.Map SockAddr Peer
type Peers = M.Map UUID Peer

data ZRE = ZRE {
    zreUUID  :: UUID
  , zrePeers :: Peers
--    conn     :: 
  }

uuidByteString = BL.toStrict . toByteString

main = do
    u <- nextUUID
    let uuid = uuidByteString $ fromJust u -- BL.toStrict $ toByteString $ fromJust u
    print u
    (addr:_) <- getAddrInfo Nothing (Just mCast) (Just port)
    q <- atomically $ newTBQueue $ 10
    s <- atomically $ newTVar $ ZRE (fromJust u) M.empty

    runConcurrently $ Concurrently (beaconRecv q) *>
                      Concurrently (beacon addr uuid zmqPort) *>
                      Concurrently (router uuid zmqPort) *>
                      Concurrently (peerPool q s)
    return ()

dealer endpoint port ourUUID = ZMQ.runZMQ $ do
  d <- ZMQ.socket ZMQ.Dealer
  ZMQ.setLinger (ZMQ.restrict 1) d
  -- The sender MAY set a high-water mark (HWM) of, for example, 100 messages per second (if the timeout period is 30 second, this means a HWM of 3,000 messages).
  ZMQ.setSendHighWM (ZMQ.restrict $ 30 * 100) d
  ZMQ.setSendTimeout (ZMQ.restrict 0) d
  ZMQ.setIdentity (ZMQ.restrict $ uuidByteString ourUUID) d
  ZMQ.connect d $ concat ["tcp://", B.unpack endpoint, ":", show port]
  forever $ do
     --ZMQ.send d [] "OHAI"
     --ZMQ.sendMulti d $ "" :| "OHAI"
     ZMQ.sendMulti d $ (NE.fromList ["OHAI"] :: NE.NonEmpty B.ByteString)
     liftIO $ threadDelay 1000000

router uuid port = ZMQ.runZMQ $ do
  r <- ZMQ.socket ZMQ.Router
  -- probably don't need to set identity here
  ZMQ.setIdentity (ZMQ.restrict uuid) r
  ZMQ.bind r $ concat ["tcp://", "*:", show port]
  forever $ do
     input <- ZMQ.receiveMulti r
     liftIO $ print input

peerPool q s = forever $ do
  msg <- atomically $ readTBQueue q
  case msg of
    (NewPeer addr uuid port) -> do
      st <- atomically $ readTVar s

      if uuid == zreUUID st
        then return () -- our own message
        else do
          case M.lookup uuid $ zrePeers st of
            Just _ -> return ()
            Nothing -> do
              B.putStrLn $ B.concat ["New peer ", B.pack $ show uuid, " (", addr, ":", B.pack $ show port , ")"]
              asnc <- async $ dealer addr port (zreUUID st)
              atomically $ modifyTVar s $
                \x -> x { zrePeers = f addr uuid port asnc (zrePeers x) }


  --print msg
  where f addr uuid port asnc = M.insert uuid $ Peer addr uuid port asnc

--newPeer addr = Peer addr

beaconRecv q = do
    sock <- multicastReceiver mCast 5670
    forever $ do
        (msg, addr) <- recvFrom sock 22
        -- print (msg, addr)
        case G.runGet parseBeacon msg of
          (Left err, remainder) -> print err
          (Right (lead, ver, uuid, port), _) -> do
            case addr of
              (SockAddrInet _hisport h) -> do
                host <- fmap B.pack $ inet_ntoa h
                atomically $ writeTBQueue q $ NewPeer host uuid port
              -- FIXME: handle SockAddrInet6 and _, lead/ver mismatch

beacon addr uuid port = do
    withSocketsDo $ do
      bracket (getSocket addr) sClose (talk (addrAddress addr))

  where
    getSocket addr = do
      s <- socket (addrFamily addr) Datagram defaultProtocol
      mapM_ (\x -> setSocketOption s x 1) [Broadcast, ReuseAddr, ReusePort]
      bind s (addrAddress addr)
      return s
    talk addr s = forever $ do
      --putStrLn "send"
      sendTo s (zreBeacon uuid port) addr
      threadDelay 1000000

zreBeacon uuid port = BL.toStrict $ runPut $ do
  putByteString "ZRE"
  putInt8 $ fromIntegral zreVer
  putByteString uuid
  putWord16be port

parseBeacon = do
  lead <- G.getByteString 3
  ver <- fromIntegral <$> G.getWord8
  uuid <- fromJust . fromByteString . BL.fromStrict <$> G.getByteString 16
  port <- fromIntegral <$> G.getWord16be
  return (lead, ver, uuid, port)

getInt8 :: Get Int
getInt8  = fromIntegral <$> G.getWord8
getInt16 = fromIntegral <$> G.getWord16be
getInt32 = fromIntegral <$> G.getWord32be

type Seq = Int
type Endpoint = B.ByteString
type Group = B.ByteString
type Groups = [Group]
type Status = Int
type Name = B.ByteString
type Headers = M.Map B.ByteString B.ByteString
type Content = [B.ByteString]

data ZREMsg = ZREMsg {
    msgSeq :: Seq
  , msgCmd :: ZRECmd
  } deriving (Show, Eq, Ord)

data ZRECmd =
    Hello Endpoint Groups Status Name Headers
  | Whisper --Content
  | Shout Group --Content
  | Join Group Status
  | Leave Group Status
  | Ping
  | PingOk
  deriving (Show, Eq, Ord)

putByteStringLen x = do
  putInt8 $ fromIntegral $ B.length x
  putByteString x

putLongByteStringLen x = do
  putInt16be $ fromIntegral $ B.length x
  putByteString x

putByteStrings x = do
  putInt32be $ fromIntegral $ length x
  mapM_ putLongByteStringLen x
  where lx = length x

putKV (k, v) = do
  putByteStringLen k
  putLongByteStringLen v

putMap map = do
  count <- putInt32be $ fromIntegral $ length ml
  mapM_ putKV ml
  where ml = M.toList map

genZre = BL.toStrict $ runPut $ do
  putWord16be zreSig
  putWord8 0xd1 -- cmd
  putInt8 $ fromIntegral zreVer
  putWord16be 1337
  putByteStringLen "endpoint:1337"
  putByteStrings ["group1", "group2"]
  putInt8 7
  putByteStringLen "name"
  putMap (M.fromList [("abc", "val1"), ("qqq", "val2")])

parseString = do
  len <- getInt8
  st <- getByteString len
  return st

parseLongString = do
  len <- getInt16
  st <- getByteString len
  return st

parseStrings = do
  count <- getInt32
  res <- sequence $ replicate count parseLongString
  return res

parseKV = do
  key <- parseString
  value <- parseLongString
  return (key, value)

parseMap = do
  count <- getInt32
  res <- sequence $ replicate count parseKV
  return $ M.fromList res

parseHello = Hello
  <$> parseString
  <*> parseStrings
  <*> getInt8
  <*> parseString
  <*> parseMap

parseWhisper = Whisper
parseShout = Shout <$> parseString
parseJoin = Join <$> parseString <*> getInt8
parseLeave = Leave <$> parseString <*> getInt8

parseCmd = do
    cmd <- getInt8
    ver <- getInt8
    seq <- getInt16

    if ver /= zreVer
      then fail "Protocol version mismatch"
      else do

        cmd <- case cmd of
          0xd1 -> parseHello
          0xd2 -> pure Whisper -- parseWhisper
          0xd3 -> parseShout
          0xd4 -> parseJoin
          0xd5 -> parseLeave
          0xd6 -> pure Ping
          0xd7 -> pure PingOk
          _    -> fail "Unknown command"

        return $ ZREMsg seq cmd

parseZre = do
  sig <- G.getWord16be
  if sig /= zreSig
    then fail "Signature mismatch"
    else do
      res <- parseCmd
      return res
