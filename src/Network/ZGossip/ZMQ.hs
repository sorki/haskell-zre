{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE FlexibleContexts #-}
module Network.ZGossip.ZMQ (zgossipDealer, zgossipRouter) where

import Control.Monad
--import Control.Monad.State
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import qualified System.ZMQ4.Monadic as ZMQ
import System.ZMQ4.Monadic as ZMQ
import qualified Data.ByteString.Char8 as B
import qualified Data.List.NonEmpty as NE
import Data.Time.Clock.POSIX

import Data.Maybe


import Data.ZGossip
import Network.ZGossip.Types
import System.ZMQ4.Endpoint

zgossipDealer endpoint ourUUID peerQ handler = ZMQ.runZMQ $ do
  d <- ZMQ.socket ZMQ.Dealer
  ZMQ.setLinger (ZMQ.restrict 1) d
  -- Never block on sending; we use an infinite HWM and buffer as many
  -- messages as needed in outgoing pipes. Note that the maximum number
  -- is the overall tuple set size
  ZMQ.setSendHighWM (ZMQ.restrict 0) d
  ZMQ.setSendTimeout (ZMQ.restrict 0) d
  -- prepend '1' in front of 16bit UUID, ZMQ.restrict would do that for us but protocol requires it
  ZMQ.setIdentity (ZMQ.restrict $ ourUUID) d
  ZMQ.connect d $ B.unpack $ pEndpoint endpoint
  let send = forever $ do
        cmd <- liftIO $ atomically $ readTBQueue peerQ
        liftIO $ print "Spreading gossip" >> (print $ newZGS cmd)
        ZMQ.sendMulti d $ (NE.fromList [encodeZGS $ newZGS cmd] :: NE.NonEmpty B.ByteString)

  let recv = forever $ do
        input <- ZMQ.receiveMulti d
        case parseZGS input of
           (Left err, r) -> do
             liftIO $ print $ "Malformed gossip message received: " ++ err
             liftIO $ print input
           (Right msg@ZGSMsg{..}, _) -> do
             liftIO $ print "DRECV" >> print msg
             liftIO $ handler msg

  sa <- ZMQ.async send
  ra <- ZMQ.async recv
  liftIO $ wait sa

zgossipRouter endpoint handler = ZMQ.runZMQ $ do
  sock <- ZMQ.socket ZMQ.Router
  ZMQ.bind sock $ B.unpack $ pEndpoint endpoint

  --ZMQ.setIdentity (ZMQ.restrict $ ourUUID) d
  forever $ do
     input <- ZMQ.receiveMulti sock
     case parseZGS input of
        (Left err, _) -> liftIO $ print $ "Malformed gossip message received: " ++ err
        (Right msg@ZGSMsg{..}, _) -> do
            liftIO $ print msg
            res <- liftIO $ handler (fromJust zgsFrom) zgsCmd
            flip mapM_ res $ \(to, cmd) -> do
              liftIO $ print "FWDing" >> print (to, cmd)
              ZMQ.sendMulti sock $ (NE.fromList [to, to, encodeZGS $ newZGS $ cmd ] :: NE.NonEmpty B.ByteString)

