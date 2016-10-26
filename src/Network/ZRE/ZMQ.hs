{-# LANGUAGE OverloadedStrings #-}
module Network.ZRE.ZMQ where

import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import qualified System.ZMQ4.Monadic as ZMQ
import System.ZMQ4.Monadic as ZMQ
import qualified Data.ByteString.Char8 as B
import qualified Data.List.NonEmpty as NE
import Data.Time.Clock.POSIX

import Data.ZRE
import Network.ZRE.Utils
import System.ZMQ4.Endpoint

--import Network.ZRE.Types

dealer endpoint ourUUID peerQ = ZMQ.runZMQ $ do
  d <- ZMQ.socket ZMQ.Dealer
  ZMQ.setLinger (ZMQ.restrict 1) d
  -- The sender MAY set a high-water mark (HWM) of, for example, 100 messages per second (if the timeout period is 30 second, this means a HWM of 3,000 messages).
  ZMQ.setSendHighWM (ZMQ.restrict $ 30 * 100) d
  ZMQ.setSendTimeout (ZMQ.restrict 0) d
  ZMQ.setIdentity (ZMQ.restrict $ uuidByteString ourUUID) d
  ZMQ.connect d $ B.unpack $ pEndpoint endpoint
  loop d 0
  where loop d x = do
           cmd <- liftIO $ atomically $ readTBQueue peerQ
           ZMQ.sendMulti d $ (NE.fromList $ encodeZRE $ newZRE x cmd :: NE.NonEmpty B.ByteString)
           loop d (x+1)

router inboxQ port = ZMQ.runZMQ $ do
  r <- ZMQ.socket ZMQ.Router
  ZMQ.bind r $ concat ["tcp://", "*:", show port]
  forever $ do
     input <- ZMQ.receiveMulti r
     now <- liftIO $ getCurrentTime
     case parseZRE input of
        (Left err, _) -> liftIO $ print $ "Malformed message received: " ++ err
        (Right msg, _) -> do
          let updateTime = \x -> x { msgTime = Just now }
          liftIO $ atomically $ writeTBQueue inboxQ (updateTime msg)
