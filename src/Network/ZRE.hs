{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.ZRE where

import Prelude hiding (putStrLn, take)
import Control.Exception
import Control.Monad hiding (join)
import Control.Monad.IO.Class
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import Network.BSD (getHostName)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.Multicast
import Network.SockAddr
import Network.Info
import qualified System.ZMQ4.Monadic as ZMQ
import qualified Data.ByteString.Char8 as B


import qualified Data.Set as Set
import Data.UUID
import Data.UUID.V1
import Data.Maybe
import Data.Time.Clock
import Data.Time.Clock.POSIX

import qualified Data.Map as M

import Data.Binary.Strict.Get as G

import Data.ZRE
import Network.ZRE.Utils
import Network.ZRE.Peer
import Network.ZRE.ZMQ
import Network.ZRE.Types
import System.ZMQ4.Endpoint

import GHC.Conc

dbg x = return ()
--dbg x = x

sec n = n * 1000000
msec n = n * 1000

main = do
  --a <- async $ runZre app
  runZre appW

  where
    appF inQ outQ = forever $ do
      --msg <- atomically $ readTBQueue inQ
      --print msg
      atomically $ writeTBQueue outQ ("shout", Shout "chat" ["13337!"])
      threadDelay (sec 1)
    -- forever shout
    appS inQ outQ =
      runConcurrently $ Concurrently (recv) *> Concurrently (broadcast)
      where
        recv = forever $ do
          msg <- atomically $ readTBQueue inQ
          print msg
        broadcast = forever $ do
          atomically $ writeTBQueue outQ ("shout", Shout "chat" ["13337!"])
          threadDelay (sec 1)

    -- chat
    app inQ outQ = do
      call $ join "CHAT"
      runConcurrently $ Concurrently (recv) *> Concurrently (broadcast)
      where
        recv = forever $ do
          evt <- atomically $ readTBQueue inQ
          case evt of
            New peer -> do
              call $ whisper peer "ohai"
              B.putStrLn $ B.intercalate " " ["New peer", printPeer peer]
            Update peer -> B.putStrLn $ B.intercalate " " ["Update peer", printPeer peer]
            Quit peer -> B.putStrLn $ B.intercalate " " ["Peer quit", printPeer peer]
            GroupJoin peer group -> B.putStrLn $ B.intercalate " " ["Join group", group, printPeer peer]
            GroupLeave peer group -> B.putStrLn $ B.intercalate " " ["Leave group", group, printPeer peer]
            Message m@ZREMsg{..} -> do
              B.putStrLn $ bshow m
              case msgCmd of
                (Shout group content) -> B.putStrLn $ B.intercalate " " ["shout for group", group, ">", B.concat content]
                (Whisper content) -> B.putStrLn $ B.intercalate " " ["whisper", B.concat content]

        broadcast = forever $ do
          B.putStr " >"
          msg <- fmap B.pack getLine
          call $ shout "CHAT" msg
          return ()

        call x = atomically $ writeTBQueue outQ x

    appW inQ outQ = do
      call $ join "pool"
      runConcurrently $ Concurrently (recv) *> Concurrently (broadcast)
       where
        recv = forever $ do
          evt <- atomically $ readTBQueue inQ
          case evt of
            New peer -> do
              call $ whisper peer "ohai"
              B.putStrLn $ B.intercalate " " ["New peer", printPeer peer]
            Update peer -> B.putStrLn $ B.intercalate " " ["Update peer", printPeer peer]
            Quit peer -> B.putStrLn $ B.intercalate " " ["Peer quit", printPeer peer]
            GroupJoin peer group -> B.putStrLn $ B.intercalate " " ["Join group", group, printPeer peer]
            GroupLeave peer group -> B.putStrLn $ B.intercalate " " ["Leave group", group, printPeer peer]
            Message m@ZREMsg{..} -> do
              B.putStrLn $ bshow m
              case msgCmd of
                (Shout group content) -> B.putStrLn $ B.intercalate " " ["shout for group", group, ">", B.concat content]
                (Whisper content) -> do
                  B.putStrLn $ B.intercalate " " ["whisper", B.concat content]
                  call $ shout "pool" (B.intercalate " " ["taking job", B.concat content])
                  call $ join $ B.concat content
                  mapM_ (\x -> (call $ shout (B.concat content) x) >> threadDelay 100000) (replicate 100 "lala")

        broadcast = forever $ do
          B.putStr " >"
          msg <- fmap B.pack getLine
          call $ shout "CHAT" msg
          return ()

        call x = atomically $ writeTBQueue outQ x


join = DoJoin
leave = DoLeave
shout = DoShout
whisper = DoWhisper

runZre app = do
    dr <- getDefRoute
    case dr of
      Nothing -> exitFail "Unable to get default route"
      Just (route, iface) -> do

        ifaceInfo <- getIface iface
        case ifaceInfo of
          Nothing -> exitFail "Unable to get info for interace"
          (Just NetworkInterface{..}) -> do

            u <- nextUUID
            zmqPort <- randPort
            let uuid = uuidByteString $ fromJust u -- BL.toStrict $ toByteString $ fromJust u
            (mCastAddr:_) <- getAddrInfo Nothing (Just mCastIP) (Just $ show mCastPort)

            let mCastEndpoint = newTCPEndpointAddrInfo mCastAddr mCastPort
            let endpoint = newTCPEndpoint (bshow ipv4) zmqPort

            name <- fmap B.pack getHostName

            inQ <- atomically $ newTBQueue 10
            outQ <- atomically $ newTBQueue 10
            -- new ZRE context
            s <- atomically $ newTVar $ ZREState (fromJust u) M.empty M.empty endpoint Set.empty 0 name M.empty inQ outQ
            -- api queues

            runConcurrently $ Concurrently (beaconRecv s) *>
                              Concurrently (beacon mCastAddr uuid zmqPort) *>
                              Concurrently (router zmqPort (inbox s inQ)) *>
                              Concurrently (api s) *>
                              Concurrently (app inQ outQ)
            return ()

api s = forever $ do
  action <- atomically $ do
    st <- readTVar s
    readTBQueue (zreOut st)

  case action of
    DoJoin group -> atomically $ do
      -- also modify US
      st <- readTVar s
      modifyTVar s $ \x -> x { zreGroups = Set.insert group (zreGroups x) }
      msgAll s $ Join group (zreGroupSeq st)

    -- DoShoutMulti needed
    DoShout group msg -> atomically $ do
      msgAll s $ Shout group [msg]
    DoWhisper peer msg -> atomically $ do
      msgPeerUUID s (peerUUID peer) $ Whisper [msg]
--    DoLeave group -> 
--    ("shout", msg) -> 

inbox s inQ msg@ZREMsg{..} = do
  let uuid = fromJust msgFrom

  -- print complete state
  dbg $ printAll s

  mpt <- atomically $ lookupPeer s uuid
  case mpt of
    Nothing -> do
      case msgCmd of
        -- if the peer is not known but a message is HELLO we create a new
        -- peer, for other messages we don't know the endpoint to connect to
        h@(Hello endpoint groups groupSeq name headers) -> do
          liftIO $ dbg $ B.putStrLn $ B.concat ["From hello"]
          void $ makePeer s uuid $ newPeerFromHello h
        -- silently drop any other messages
        _ -> return ()

    (Just peer) -> do
      atomically $ updateLastHeard peer $ fromJust msgTime

      -- FIXME: acually check sequence numbers
      -- FIXME: check if the received message is hello
      atomically $ updatePeer peer $ \x -> x { peerSeq = msgSeq }

      -- FIXME: also emit join/leave/hello and peer destroy
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

        Ping -> atomically $ msgPeerUUID s uuid PingOk
        PingOk -> return ()
        h@(Hello endpoint groups groupSeq name headers) -> do
          -- if this peer was already registered (e.g. from beacon) update appropriate data
          -- FIXME: refactor to updateFromHello
          atomically $ do
            joinGroups s peer groups groupSeq
            updatePeer peer $ \x -> x { peerName = name }
            p <- readTVar peer
            emit s $ Update p
          return ()

beaconRecv s = do
    sock <- multicastReceiver mCastIP 5670
    forever $ do
        (msg, addr) <- recvFrom sock 22
        case parseBeacon msg of
          (Left err, remainder) -> print err
          (Right (lead, ver, uuid, port), _) -> do
            case addr of
              x@(SockAddrInet _hisport host) -> do
                beaconHandle s (showSockAddrBS x) uuid (fromIntegral port)
              x@(SockAddrInet6 _hisport _ h _) -> do
                beaconHandle s (showSockAddrBS x) uuid (fromIntegral port)

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

beacon addr uuid port = do
    withSocketsDo $ do
      bracket (getSocket addr) close (talk (addrAddress addr))

  where
    getSocket addr = do
      s <- socket (addrFamily addr) Datagram defaultProtocol
      mapM_ (\x -> setSocketOption s x 1) [Broadcast, ReuseAddr, ReusePort]
      bind s (addrAddress addr)
      return s
    talk addr s = forever $ do
      --putStrLn "send"
      sendTo s (zreBeacon uuid port) addr
      threadDelay zreBeaconMs
