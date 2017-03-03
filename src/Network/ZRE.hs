{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.ZRE (runZre) where

import Prelude hiding (putStrLn, take)
import Control.Exception
import Control.Monad hiding (join)
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Network (withSocketsDo)
import Network.BSD (getHostName)
import Network.Socket hiding (accept, send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.Multicast
import Network.SockAddr
import Network.Info
import qualified Data.ByteString.Char8 as B


import qualified Data.Set as Set
import Data.UUID
import Data.UUID.V1
import Data.Maybe
import Data.Time.Clock

import qualified Data.Map as M

import Data.ZRE
import qualified Data.ZGossip as ZGS
import Network.ZRE.Utils
import Network.ZRE.Peer
import Network.ZRE.ZMQ
import Network.ZRE.Types
import System.ZMQ4.Endpoint

import Network.ZGossip

dbg x = return ()
--dbg x = x

gossipPort = 31337

--runZre :: (TBQueue Event -> TBQueue API -> IO a) -> IO ()
runZre app = do
    dr <- getDefRoute
    case dr of
      Nothing -> exitFail "Unable to get default route"
      Just (_route, iface) -> do

        ifaceInfo <- getIface iface
        case ifaceInfo of
          Nothing -> exitFail "Unable to get info for interace"
          (Just NetworkInterface{..}) -> do

            u <- maybeM (exitFail "Unable to get UUID") return nextUUID
            zrePort <- randPort
            let uuid = uuidByteString u
            (mCastAddr:_) <- getAddrInfo Nothing (Just mCastIP) (Just $ show mCastPort)

            (gossipAddr:_) <- getAddrInfo Nothing (Just "::1") (Just $ show gossipPort) --  show mCastPort)

            let mCastEndpoint = newTCPEndpointAddrInfo mCastAddr mCastPort
            let zreEndpoint = newTCPEndpoint (bshow ipv4) zrePort

            let gossipServerEndpoint = newTCPEndpoint "*" gossipPort
            let gossipClientEndpoint = newTCPEndpoint "172.17.1.63" gossipPort

            zreName <- fmap B.pack getHostName

            inQ <- atomically $ newTBQueue 10
            outQ <- atomically $ newTBQueue 10

            gossipQ <- atomically $ newTBQueue 10

            atomically $ mapM_ (writeTBQueue gossipQ) [ZGS.Hello, ZGS.Publish "test" "127.0.0.1" 1337]
            s <- newZREState zreName zreEndpoint u inQ outQ

            void $ runConcurrently $ Concurrently (beaconRecv s) *>
                              Concurrently (beacon mCastAddr uuid zrePort) *>
                              --Concurrently (zgossipServer gossipServerEndpoint) *> -- zgsHandle) *>
                              Concurrently (zgossipClient uuid gossipClientEndpoint zreEndpoint (zgossipZRE outQ)) *>
                              Concurrently (zreRouter zreEndpoint (inbox s)) *>
                              Concurrently (api s) *>
                              Concurrently (runZ app inQ outQ)
            return ()

api :: TVar ZREState -> IO ()
api s = forever $ do
  a <- atomically $ readTVar s >>= readTBQueue . zreOut
  handleApi s a

handleApi :: TVar ZREState -> API -> IO ()
handleApi s action = do
  case action of
    DoJoin group -> atomically $ do
      incGroupSeq
      modifyTVar s $ \x -> x { zreGroups = Set.insert group (zreGroups x) }
      st <- readTVar s
      msgAll s $ Join group (zreGroupSeq st)

    DoLeave group -> atomically $ do
      incGroupSeq
      modifyTVar s $ \x -> x { zreGroups = Set.delete group (zreGroups x) }
      st <- readTVar s
      msgAll s $ Leave group (zreGroupSeq st)

    DoShoutMulti group mmsg -> atomically $ msgGroup s group $ Shout group mmsg
    DoShout group msg -> atomically $ msgGroup s group $ Shout group [msg]
    DoWhisper uuid msg -> atomically $ do
      mpt <- lookupPeer s uuid
      case mpt of
        Nothing -> return ()
        Just peer -> do
          p <- readTVar peer
          msgPeerUUID s (peerUUID p) $ Whisper [msg]

    DoDiscover uuid endpoint -> do
      mp <- atomically $ lookupPeer s uuid
      case mp of
        Just _ -> return ()
        Nothing -> do
          dbg $ B.putStrLn $ B.concat ["New peer from discover ", B.pack $ show uuid, " (", pEndpoint endpoint, ")"]
          void $ makePeer s uuid $ newPeerFromEndpoint endpoint
      --return ()
  where
    incGroupSeq = modifyTVar s $ \x -> x { zreGroupSeq = (zreGroupSeq x) + 1 }

-- handles incoming ZRE messages
-- creates peers, updates state
inbox :: TVar ZREState -> ZREMsg -> IO ()
inbox s msg@ZREMsg{..} = do
  let uuid = fromJust msgFrom

  dbg $ B.putStrLn "msg"
  dbg $ print msg
  dbg $ B.putStrLn "state pre-msg"
  dbg $ printAll s

  mpt <- atomically $ lookupPeer s uuid
  case mpt of
    Nothing -> do
      case msgCmd of
        -- if the peer is not known but a message is HELLO we create a new
        -- peer, for other messages we don't know the endpoint to connect to
        h@(Hello _endpoint _groups _groupSeq _name _headers) -> do
          liftIO $ dbg $ B.putStrLn $ B.concat ["New peer from hello"]
          peer <- makePeer s uuid $ newPeerFromHello h
          atomically $ updatePeer peer $ \x -> x { peerSeq = (peerSeq x) + 1 }
        -- silently drop any other messages
        _ -> return ()

    (Just peer) -> do
      atomically $ updateLastHeard peer $ fromJust msgTime

      -- destroy/re-start peer when this doesn't match
      p <- atomically $ readTVar peer
      case peerSeq p == msgSeq of
        True -> do
          -- rename to peerExpectSeq, need to update at line 127 too
          atomically $ updatePeer peer $ \x -> x { peerSeq = (peerSeq x) + 1 }
          handleCmd s msg peer
        _ -> do
          dbg $ B.putStrLn "sequence mismatch, recreating peer"
          recreatePeer (peerUUID p) msgCmd

  dbg $ B.putStrLn "state post-msg"
  dbg $ printAll s
  where
    recreatePeer uuid h@(Hello _ _ _ _ _) = do
          destroyPeer s uuid
          peer <- makePeer s uuid $ newPeerFromHello h
          atomically $ updatePeer peer $ \x -> x { peerSeq = (peerSeq x) + 1 }
    recreatePeer uuid _ = destroyPeer s uuid

handleCmd :: TVar ZREState -> ZREMsg -> TVar Peer -> IO ()
handleCmd s msg@ZREMsg{..}  peer = do
      case msgCmd of
        (Whisper content) -> do
          atomically $ emit s $ Message msg
          dbg $ B.putStrLn $ B.intercalate " " ["whisper", B.concat content]

        (Shout group content) -> do
          atomically $ emit s $ Message msg
          dbg $ B.putStrLn $ B.intercalate " " ["shout for group", group, ">", B.concat content]

        (Join group groupSeq) -> do
          atomically $ joinGroup s peer group groupSeq
          dbg $ B.putStrLn $ B.intercalate " " ["join", group, bshow groupSeq]

        (Leave group groupSeq) -> do
          atomically $ leaveGroup s peer group groupSeq
          dbg $ B.putStrLn $ B.intercalate " " ["leave", group, bshow groupSeq]

        Ping -> atomically $ msgPeer peer PingOk
        PingOk -> return ()
        (Hello _endpoint groups groupSeq name headers) -> do
          -- if this peer was already registered
          -- (e.g. from beacon) update appropriate data
          atomically $ do
            joinGroups s peer groups groupSeq
            updatePeer peer $ \x -> x {
                         peerName = Just name
                       , peerHeaders = headers
                       }
            p <- readTVar peer
            emit s $ Ready p
          return ()

beaconRecv :: TVar ZREState -> IO b
beaconRecv s = do
    sock <- multicastReceiver mCastIP (fromIntegral mCastPort)
    forever $ do
        (msg, addr) <- recvFrom sock 22
        case parseBeacon msg of
          (Left err, _remainder) -> print err
          (Right (_lead, _ver, uuid, port), _) -> do
            case addr of
              x@(SockAddrInet _hisport _host) -> do
                beaconHandle s (showSockAddrBS x) uuid (fromIntegral port)
              x@(SockAddrInet6 _hisport _ _host _) -> do
                beaconHandle s (showSockAddrBS x) uuid (fromIntegral port)
              _ -> return ()

-- handle messages received on beacon
-- creates new peers
-- updates peers last heard
beaconHandle :: TVar ZREState -> B.ByteString -> UUID -> Int -> IO ()
beaconHandle s addr uuid port = do
    st <- atomically $ readTVar s

    if uuid == zreUUID st
      then return () -- our own message
      else do
        case M.lookup uuid $ zrePeers st of
          (Just peer) -> do
            now <- getCurrentTime
            atomically $ updateLastHeard peer now
          Nothing -> do
            dbg $ B.putStrLn $ B.concat ["New peer from beacon ", B.pack $ show uuid, " (", addr, ":", B.pack $ show port , ")"]
            void $ makePeer s uuid $ newPeerFromBeacon addr port
            return ()

--randomPort addr = do
--    withSocketsDo $ do
--      bracket (getSocket addr) close (pure ())
--  where
--    getSocket addr = do
--      s <- socket (addrFamily addr) Datagram defaultProtocol
--      bind s (addrAddress addr)
--      return s

-- sends udp multicast beacons
beacon :: AddrInfo -> B.ByteString -> Port -> IO a
beacon addrInfo uuid port = do
    withSocketsDo $ do
      bracket (getSocket addrInfo) close (talk (addrAddress addrInfo) (zreBeacon uuid port))
  where
    getSocket addr = do
      s <- socket (addrFamily addr) Datagram defaultProtocol
      mapM_ (\x -> setSocketOption s x 1) [Broadcast, ReuseAddr, ReusePort]
      bind s (addrAddress addr)
      return s
    talk addr msg s =
      forever $ do
      void $ sendTo s msg addr
      threadDelay zreBeaconMs
