{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.ZRE.Peer where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as B
import Data.Time.Clock

import Data.ZRE
import System.ZMQ4.Endpoint
import Network.ZRE.Types
import Network.ZRE.Utils
import Network.ZRE.ZMQ (dealer)

printPeer Peer{..} = B.intercalate " " 
  ["Peer",
    peerName,
    pEndpoint peerEndpoint,
    bshow peerSeq,
    bshow peerGroupSeq,
    bshow peerGroups,
    bshow peerLastHeard]

newPeer s endpoint uuid groups name t = do
  st <- readTVar s
  peerQ <- newTBQueue 10
  writeTBQueue peerQ $ Hello (zreEndpoint st) (zreGroups st) (zreGroupSeq st) (zreName st) (zreHeaders st)

  let p = Peer endpoint uuid 0 groups 0 name Nothing Nothing peerQ t
  np <- newTVar $ p

  modifyTVar s $ \x -> x { zrePeers = M.insert uuid np (zrePeers x) }

  emit s $ New p

  return $ (np, Just $ dealer endpoint (zreUUID st) peerQ, Just $ pinger s np)

newPeerFromBeacon addr port t uuid s = do
  let endpoint = newTCPEndpoint addr port
  newPeer s endpoint uuid (Set.empty :: Groups) "<unknown>" t
newPeerFromHello (Hello endpoint groups groupSeq name headers) t uuid s =
  newPeer s endpoint uuid groups name t

makePeer s uuid newPeerFn = do
  t <- getCurrentTime
  res <- atomically $ do
    st <- readTVar s
    case M.lookup uuid $ zrePeers st of
      (Just peer) -> return (peer, Nothing, Nothing)
      Nothing -> newPeerFn t uuid s

  case res of
    -- fixme: clumsy
    (peer, Nothing, Nothing) -> return peer
    (peer, Just deal, Just ping) -> do
      a <- async deal
      b <- async ping
      atomically $ do
        updatePeer peer $ \x -> x { peerAsync = (Just a) }
        updatePeer peer $ \x -> x { peerAsyncPing = (Just b) }

      return peer

destroyPeer s uuid = do
  asyncs <- atomically $ do
    mpt <- lookupPeer s uuid
    case mpt of
      Nothing -> return []
      (Just peer) -> do
        p@Peer{..} <- readTVar peer
        emit s $ Quit p
        modifyTVar s $ \x -> x { zrePeers = M.delete uuid (zrePeers x) } 
        leaveGroups s peer peerGroups peerGroupSeq

        return [peerAsync, peerAsyncPing]

  -- this is called from pinger so no more code is executed after this point
  mapM_ cancelM asyncs
  where
    cancelM Nothing = return ()
    cancelM (Just a) = cancel a

pinger s pt = forever $ do
  Peer{..} <- atomically $ readTVar pt
  now <- getCurrentTime
  if diffUTCTime now peerLastHeard > deadPeriod
    then destroyPeer s peerUUID
    else do
      if diffUTCTime now peerLastHeard > quietPeriod
        then atomically $ writeTBQueue peerQueue $ Ping
        else return ()

  -- FIXME: less retarded scheduling
  threadDelay 10000

lookupPeer s uuid = do
  st <- readTVar s
  return $ M.lookup uuid $ zrePeers st

updatePeer peer fn = do modifyTVar peer fn

updatePeerUUID s uuid fn = do
  st <- readTVar s
  case M.lookup uuid $ zrePeers st of
    Nothing -> return ()
    (Just peer) -> updatePeer peer fn

updateLastHeard peer val = updatePeer peer $ \x -> x { peerLastHeard = val }

joinGroup s peer group groupSeq = do
  updatePeer peer $ \x -> x { peerGroups = Set.insert group (peerGroups x) }
  updatePeer peer $ \x -> x { peerGroupSeq = groupSeq }
  p <- readTVar peer
  emit s $ GroupJoin p group
  modifyTVar s $ \x -> x { zrePeerGroups = M.alter (f p peer) group $ zrePeerGroups x }
  where
    f p peer Nothing = Just $ M.fromList [(peerUUID p, peer)]
    f p peer (Just old) = Just $ M.insert (peerUUID p) peer old

joinGroups s peer groups groupSeq = do
  mapM (\x -> joinGroup s peer x groupSeq) $ Set.toList groups

leaveGroup s peer group groupSeq = do
  updatePeer peer $ \x -> x { peerGroups = Set.delete group (peerGroups x) }
  updatePeer peer $ \x -> x { peerGroupSeq = groupSeq }
  p <- readTVar peer
  emit s $ GroupLeave p group
  modifyTVar s $ \x -> x { zrePeerGroups = M.alter (f p peer) group $ zrePeerGroups x }
  where
    f p peer Nothing = Nothing
    f p peer (Just old) = nEmpty $ M.delete (peerUUID p) old
    nEmpty map | M.null map = Nothing
    nEmpty map = Just map

leaveGroups s peer groups groupSeq = do
  mapM (\x -> leaveGroup s peer x groupSeq) $ Set.toList groups

msgPeer peer msg = do
  p <- readTVar peer
  writeTBQueue (peerQueue p) msg

msgPeerUUID s uuid msg = do
  st <- readTVar s
  case M.lookup uuid $ zrePeers st of
    Nothing -> return ()
    (Just peer) -> do
      msgPeer peer msg
      return ()

msgAll s msg = do
  st <- readTVar s
  mapM_ (flip msgPeer msg) (zrePeers st)

printPeers x = do
  mapM_ ePrint $ M.elems x
  where
    ePrint pt = do
      p <- atomically $ readTVar pt
      B.putStrLn $ printPeer p

printGroup (k,v) = do
  B.putStrLn $ B.intercalate " " ["group", k, "->"]
  printPeers v

printAll s = do
  st <- atomically $ readTVar s
  printPeers $ zrePeers st
  mapM_ printGroup $ M.toList $ zrePeerGroups st
