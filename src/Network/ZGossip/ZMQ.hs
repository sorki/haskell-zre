{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE FlexibleContexts #-}
module Network.ZGossip.ZMQ (zgossipDealer, zgossipRouter) where

import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.IO.Class
import qualified System.ZMQ4.Monadic as ZMQ
import qualified Data.ByteString.Char8 as B
import qualified Data.List.NonEmpty as NE

import Data.Maybe

import Data.ZGossip
import System.ZMQ4.Endpoint

zgossipDealer :: Control.Monad.IO.Class.MonadIO m
              => Endpoint
              -> B.ByteString
              -> TBQueue ZGSCmd
              -> (ZGSMsg -> IO ())
              -> m a
zgossipDealer endpoint ourUUID peerQ handler = ZMQ.runZMQ $ do
  d <- ZMQ.socket ZMQ.Dealer
  ZMQ.setLinger (ZMQ.restrict (1 :: Int)) d
  -- Never block on sending; we use an infinite HWM and buffer as many
  -- messages as needed in outgoing pipes. Note that the maximum number
  -- is the overall tuple set size
  ZMQ.setSendHighWM (ZMQ.restrict (0 :: Int)) d
  ZMQ.setSendTimeout (ZMQ.restrict (0 :: Int)) d
  ZMQ.setIdentity (ZMQ.restrict $ ourUUID) d
  ZMQ.connect d $ B.unpack $ pEndpoint endpoint
  let spam = forever $ do
        cmd <- liftIO $ atomically $ readTBQueue peerQ
        --liftIO $ print "Spreading gossip" >> (print $ newZGS cmd)
        ZMQ.sendMulti d $ (NE.fromList [encodeZGS $ newZGS cmd] :: NE.NonEmpty B.ByteString)

  let recv = forever $ do
        input <- ZMQ.receiveMulti d
        case parseZGS input of
           Left err -> do
             liftIO $ print $ "Malformed gossip message received: " ++ err
             liftIO $ print input
           Right msg@ZGSMsg{..} -> do
             liftIO $ handler msg

  sa <- ZMQ.async spam
  void $ ZMQ.async recv
  liftIO $ wait sa

zgossipRouter :: (Foldable t, Control.Monad.IO.Class.MonadIO m)
              => Endpoint
              -> (B.ByteString
              -> ZGSCmd
              -> IO (t (B.ByteString, ZGSCmd)))
              -> m a
zgossipRouter endpoint handler = ZMQ.runZMQ $ do
  sock <- ZMQ.socket ZMQ.Router
  ZMQ.bind sock $ B.unpack $ pEndpoint endpoint

  forever $ do
     input <- ZMQ.receiveMulti sock
     case parseZGS input of
        Left err -> liftIO $ print $ "Malformed gossip message received: " ++ err
        Right ZGSMsg{..} -> do
            --liftIO $ print msg
            res <- liftIO $ handler (fromJust zgsFrom) zgsCmd
            flip mapM_ res $ \(to, cmd) -> do
              --liftIO $ print "FWDing" >> print (to, cmd)
              ZMQ.sendMulti sock $ (NE.fromList [to, to, encodeZGS $ newZGS $ cmd ] :: NE.NonEmpty B.ByteString)

