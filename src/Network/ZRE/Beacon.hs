{-# LANGUAGE OverloadedStrings #-}
module Network.ZRE.Beacon (beacon, beaconRecv) where

import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Network.Socket hiding (accept, send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.SockAddr
import Network.Multicast

import Data.Maybe
import Data.UUID
import Data.Time.Clock
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B

import Data.ZRE
import Network.ZRE.Peer
import Network.ZRE.Types
import System.ZMQ4.Endpoint

beaconRecv :: TVar ZREState -> Endpoint -> IO b
beaconRecv s e = do
    sock <- multicastReceiver (B.unpack $ endpointAddr e) (fromIntegral $ fromJust $ endpointPort e)
    forever $ do
        (msg, addr) <- recvFrom sock 22
        case parseBeacon msg of
          Left err -> print err
          Right (_lead, _ver, uuid, port) -> do
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
            -- B.putStrLn $ B.concat ["New peer from beacon ", B.pack $ show uuid, " (", addr, ":", B.pack $ show port , ")"]
            void $ makePeer s uuid $ newPeerFromBeacon addr port
            return ()


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
