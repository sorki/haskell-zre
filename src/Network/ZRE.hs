{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.ZRE where

import Prelude hiding (putStrLn, take)
import Control.Exception
import Control.Monad
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

main = do
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

            -- newpeer q
            --q <- atomically $ newTBQueue $ 10
            -- inbox
            --inboxQ <- atomically $ newTBQueue $ 10
            (q, inboxQ) <- atomically $ do
              q <- newTBQueue $ 10
              inboxQ <- newTBQueue $ 10
              return (q, inboxQ)

            -- new ZRE context
            s <- atomically $ newTVar $ ZRE (fromJust u) M.empty M.empty endpoint Set.empty 0 name M.empty

            runConcurrently $ Concurrently (beaconRecv q) *>
                              Concurrently (beacon mCastAddr uuid zmqPort) *>
                              Concurrently (router inboxQ zmqPort) *>
                              Concurrently (peerPool q s) *>
                              Concurrently (inbox inboxQ s)
            return ()

inbox inboxQ s = forever $ do
  msg@ZREMsg{..} <- atomically $ readTBQueue inboxQ
  print msg

  let uuid = fromJust msgFrom

  printAll s

  mpt <- atomically $ lookupPeer s uuid
  case mpt of
    Nothing -> do
      case msgCmd of
        -- if the peer is not known but a message is HELLO we create a new
        -- peer, for other messages we don't know the endpoint to connect to
        h@(Hello endpoint groups groupSeq name headers) -> do
          liftIO $ B.putStrLn $ B.concat ["From hello"]
          void $ makePeer s uuid $ newPeerFromHello h
        -- silently drop any other messages
        _ -> return ()

    (Just peer) -> do
      atomically $ updateLastHeard peer $ fromJust msgTime

      -- FIXME: acually check sequence numbers
      atomically $ updatePeer peer $ \x -> x { peerSeq = msgSeq }

      case msgCmd of
        (Whisper content) -> B.putStrLn $ B.intercalate " " ["whisper", B.concat content]

        (Shout group content) -> B.putStrLn $ B.intercalate " " ["shout for group", group, ">", B.concat content]

        (Join group groupSeq) -> do
          atomically $ joinGroup s peer group groupSeq
          B.putStrLn $ B.intercalate " " ["join", group, bshow groupSeq]

        (Leave group groupSeq) -> do
          atomically $ leaveGroup s peer group groupSeq
          B.putStrLn $ B.intercalate " " ["leave", group, bshow groupSeq]

        Ping -> atomically $ msgPeerUUID s uuid PingOk
        PingOk -> return ()
        h@(Hello endpoint groups groupSeq name headers) -> do
          -- if this peer was already registered (e.g. from beacon) update appropriate data
          atomically $ joinGroups s peer groups groupSeq
          atomically $ updatePeer peer $
            \x -> x {
              peerName = name
              }
          atomically $ msgAll s $ Shout "chat" ["suckers!"]
          return ()

-- FIXME: this queue is obsolete as well
peerPool q s = forever $ do
  msg <- atomically $ readTBQueue q
  case msg of
    (NewPeer addr uuid port) -> do
      st <- atomically $ readTVar s

      if uuid == zreUUID st
        then return () -- our own message
        else do
          case M.lookup uuid $ zrePeers st of
            (Just peer) -> do
              now <- getCurrentTime
              atomically $ updateLastHeard peer now
            Nothing -> do
              B.putStrLn $ B.concat ["New peer from beacon ", B.pack $ show uuid, " (", addr, ":", B.pack $ show port , ")"]
              void $ makePeer s uuid $ newPeerFromBeacon addr port
              return ()


beaconRecv q = do
    sock <- multicastReceiver mCastIP 5670
    forever $ do
        (msg, addr) <- recvFrom sock 22
        case parseBeacon msg of
          (Left err, remainder) -> print err
          (Right (lead, ver, uuid, port), _) -> do
            case addr of
              x@(SockAddrInet _hisport host) -> do
                atomically $ writeTBQueue q $ NewPeer (showSockAddrBS x) uuid (fromIntegral port)
              x@(SockAddrInet6 _hisport _ h _) -> do
                atomically $ writeTBQueue q $ NewPeer (showSockAddrBS x) uuid (fromIntegral port)

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
