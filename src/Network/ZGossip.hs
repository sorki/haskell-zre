{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.ZGossip where

import Prelude hiding (putStrLn, take)
import Control.Exception
import Control.Monad hiding (join)
import Control.Monad.IO.Class
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import Network.BSD (getHostName)
import Network.Socket hiding (accept, send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.Multicast
import Network.SockAddr
import Network.Info
import qualified System.ZMQ4.Monadic as ZMQ
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL


import qualified Data.Set as Set
import Data.UUID
import Data.UUID.V1
import Data.Maybe
import Data.Time.Clock
import Data.Time.Clock.POSIX

import qualified Data.Map as M

import Data.Binary.Strict.Get as G
import qualified Data.Set as S

import Data.ZGossip
--import Network.ZRE.Utils
--import Network.ZRE.Peer
--import Network.ZRE.ZMQ
import Network.ZRE.Types
--import System.ZMQ4.Endpoint

import Network.ZGossip.ZMQ
import Network.ZGossip.Types

import System.ZMQ4.Endpoint
--zgossip

--zgsHandle msg = print msg
--

zgsHandle s from Hello = do
  atomically $ modifyTVar s $ \x -> x { gossipPeers = S.insert from (gossipPeers x) }
  st <- atomically $ readTVar s
  print st
  -- send all the k,v pairs to this client
  return [(from, cvtPub pub) | pub <- M.toList $ gossipPairs st ]
zgsHandle s from x@(Publish k v ttl) = do
  atomically $ modifyTVar s $ \x -> x { gossipPairs = M.insert k (v, ttl) (gossipPairs x) }
  st <- atomically $ readTVar s
  print st
  -- republish this to all other clients
  return [(to, x) | to <- M.keys $ gossipPairs st, to /= from ]
zsgHandle s _ _ = return []
--zsgHandle s from Ping = return

cvtPub (k, (v, ttl)) = Publish k v ttl

zgossipClient uuid endpoint ourEndpoint handler = do
  -- TODO: change to 100 and 90 for release
  --let pubcmd =
  gossipQ <- atomically $ newTBQueue 10
  -- TODO: publish e.g. every 9 secs
  atomically $ mapM_ (writeTBQueue gossipQ) [Hello] -- , pubcmd]
  pa <- async $ forever $ do
    atomically $ writeTBQueue gossipQ $ Publish uuid (pEndpoint ourEndpoint) 10 -- pubcmd
    threadDelay $ 1000000*9

  link pa
  zgossipDealer endpoint uuid gossipQ handler

zgossipZRE q msg@ZGSMsg{..} = handlePublish zgsCmd
  where handlePublish (Publish k v ttl) = do
          case fromByteString $ BL.fromStrict k of
            Nothing ->  liftIO $ print "Can't parse zgossip uuid"
            Just uuid -> do
             case parseAttoEndpoint v of
               (Left err) -> liftIO $ print "Can't parse zgossip endpoint"
               (Right endpoint) -> atomically $ writeTBQueue q (DoDiscover uuid endpoint)
        handlePublish _ = return ()

zgossipServer endpoint = do
  gossipS <- atomically $ newTVar emptyGossipState
  let handler = zgsHandle

  let expire = forever $ do
        st <- atomically $ readTVar gossipS
        atomically $ modifyTVar gossipS $ \x -> x { gossipPairs = M.mapMaybe ttlUpdate (gossipPairs x) }
        threadDelay 1000000
        where
           ttlUpdate (v, 0)   = Nothing
           ttlUpdate (v, ttl) = Just (v, ttl - 1)

  ea <- async expire
  link ea

  zgossipRouter endpoint (handler gossipS)

