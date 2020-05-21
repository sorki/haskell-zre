{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.ZGossip (
    zgossipServer
  , zgossipClient
  , zgossipZRE) where


import Prelude hiding (putStrLn, take)
import Control.Monad hiding (join)
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM

import Data.ByteString (ByteString)
import Data.UUID
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

import Data.ZGossip
import Network.ZRE.Types (API(DoDiscover))
import Network.ZRE.Utils (bshow)

import Network.ZGossip.ZMQ
import Network.ZGossip.Types

import System.ZMQ4.Endpoint

zgossipClient :: Key -> Endpoint -> Endpoint -> (ZGSMsg -> IO ()) -> IO ()
zgossipClient uuid endpoint ourEndpoint handler = do
  gossipQ <- atomically $ newTBQueue 10
  atomically $ mapM_ (writeTBQueue gossipQ) [Hello]
  pa <- async $ forever $ do
    atomically $ writeTBQueue gossipQ $ Publish uuid (pEndpoint ourEndpoint) 600
    -- publish every 50s
    threadDelay $ 1000000*50

  link pa
  zgossipDealer endpoint uuid gossipQ handler

zgossipServer :: Endpoint -> IO ()
zgossipServer endpoint = do
  gossipS <- atomically $ newTVar emptyGossipState

  let expire = forever $ do
        atomically $ modifyTVar gossipS $ \x -> x { gossipPairs = M.mapMaybe ttlUpdate (gossipPairs x) }
        threadDelay 1000000
        where
           ttlUpdate (_, 0)   = Nothing
           ttlUpdate (v, ttl) = Just (v, ttl - 1)

  ea <- async expire
  link ea

  zgossipRouter endpoint (serverHandle gossipS)

serverHandle :: TVar ZGossipState -> Peer -> ZGSCmd -> IO [(Peer, ZGSCmd)]
serverHandle s from Hello = do
  atomically $ modifyTVar s $ \x -> x { gossipPeers = S.insert from (gossipPeers x) }
  st <- atomically $ readTVar s
  dbg ["Hello from", tryUUID from]
  -- send all the k,v pairs to this client
  return [(from, cvtPub pub) | pub <- M.toList $ gossipPairs st ]
serverHandle s from pub@(Publish k v ttl) = do
  atomically $ modifyTVar s $ \x -> x { gossipPairs = M.insert k (v, ttl) (gossipPairs x) }
  st <- atomically $ readTVar s
  dbg ["Publish from", tryUUID from, tryUUID k, "=", v, "( ttl", bshow ttl, ")"]

  -- republish this to all other clients
  return [(to, pub) | to <- M.keys $ gossipPairs st, to /= from ]
serverHandle _ from Ping = do
  dbg ["Ping from", tryUUID from]
  return [(from, PingOk)]
serverHandle _ _ PingOk = return []
serverHandle _ _ Invalid = return []

tryUUID :: ByteString -> ByteString
tryUUID x = maybe x toASCIIBytes (fromByteString $ BL.fromStrict x)

dbg :: [ByteString] -> IO ()
dbg = B.putStrLn . (B.intercalate " ")

-- send DoDiscover ZRE API messages on new Publish message
zgossipZRE :: TBQueue API -> ZGSMsg -> IO ()
zgossipZRE q ZGSMsg{..} = handlePublish zgsCmd
  where handlePublish (Publish k v _) = do
          case fromByteString $ BL.fromStrict k of
            Nothing ->  liftIO $ B.putStrLn "Can't parse zgossip uuid"
            Just uuid -> do
             case parseAttoEndpoint v of
               (Left _err) -> liftIO $ B.putStrLn "Can't parse zgossip endpoint"
               (Right endpoint) -> atomically $ writeTBQueue q (DoDiscover uuid endpoint)
        handlePublish _ = return ()
