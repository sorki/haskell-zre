{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.ZRE.Peer (
    newPeerFromBeacon
  , newPeerFromHello
  , newPeerFromEndpoint
  , makePeer
  , destroyPeer
  , msgPeer
  , msgPeerUUID
  , msgAll
  , msgGroup
  , joinGroup
  , joinGroups
  , leaveGroup
  , leaveGroups
  , lookupPeer
  , updatePeer
  , updateLastHeard
  , printPeer
  , printAll
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as B
import Data.Time.Clock
import Data.UUID
import Data.ZRE
import System.ZMQ4.Endpoint
import Network.ZRE.Types
import Network.ZRE.Utils
import Network.ZRE.ZMQ (zreDealer)

printPeer :: Peer -> B.ByteString
printPeer Peer{..} = B.intercalate " "
  ["Peer",
    bshow peerName,
    pEndpoint peerEndpoint,
    toASCIIBytes peerUUID,
    bshow peerSeq,
    bshow peerGroupSeq,
    bshow peerGroups,
    bshow peerLastHeard]

newPeer :: MonadIO m
        => TVar ZREState
        -> Endpoint
        -> UUID
        -> Set.Set Group
        -> GroupSeq
        -> Maybe Name
        -> Headers
        -> UTCTime
        -> STM (TVar Peer, Maybe (m a), Maybe (IO b))
newPeer s endpoint uuid groups groupSeq mname headers t = do
  st <- readTVar s
  peerQ <- newTBQueue 10
  writeTBQueue peerQ $ Hello (zreEndpoint st) (zreGroups st) (zreGroupSeq st) (zreName st) (zreHeaders st)

  let p = Peer {
              peerEndpoint  = endpoint
            , peerUUID      = uuid
            , peerSeq       = 1
            , peerGroups    = groups
            , peerGroupSeq  = 0
            , peerName      = mname
            , peerHeaders   = headers
            , peerAsync     = Nothing
            , peerAsyncPing = Nothing
            , peerQueue     = peerQ
            , peerLastHeard = t }
  np <- newTVar $ p

  modifyTVar s $ \x -> x { zrePeers = M.insert uuid np (zrePeers x) }

  emit s $ New p
  case mname of
    (Just _) -> emit s $ Ready p
    Nothing -> return ()

  joinGroups s np groups groupSeq

  return $ (np, Just $ zreDealer endpoint (uuidByteString $ zreUUID st) peerQ, Just $ pinger s np)

newPeerFromBeacon :: MonadIO m
                  => Address
                  -> Port
                  -> UTCTime
                  -> UUID
                  -> TVar ZREState
                  -> STM (TVar Peer, Maybe (m a), Maybe (IO b))
newPeerFromBeacon addr port t uuid s = do
  let endpoint = newTCPEndpoint addr port
  newPeer s endpoint uuid (Set.empty :: Groups) 0 Nothing M.empty t

newPeerFromHello :: MonadIO m
                 => ZRECmd
                 -> UTCTime
                 -> UUID
                 -> TVar ZREState
                 -> STM (TVar Peer, Maybe (m a), Maybe (IO b))
newPeerFromHello (Hello endpoint groups groupSeq name headers) t uuid s =
  newPeer s endpoint uuid groups groupSeq (Just name) headers t
newPeerFromHello _ _ _ _ = fail "not a hello message"

newPeerFromEndpoint :: MonadIO m
                    => Endpoint
                    -> UTCTime
                    -> UUID
                    -> TVar ZREState
                    -> STM (TVar Peer, Maybe (m a), Maybe (IO b))
newPeerFromEndpoint endpoint t uuid s =
  newPeer s endpoint uuid (Set.empty :: Groups) 0 Nothing M.empty t

makePeer :: TVar ZREState
            -> UUID
            -> (UTCTime
                -> UUID
                -> TVar ZREState
                -> STM (TVar Peer, Maybe (IO ()), Maybe (IO ())))
            -> IO (TVar Peer)
makePeer s uuid newPeerFn = do
  t <- getCurrentTime
  res <- atomically $ do
    st <- readTVar s
    case M.lookup uuid $ zrePeers st of
      (Just peer) -> return (peer, Nothing, Nothing)
      Nothing -> newPeerFn t uuid s

  case res of
    -- fixme: clumsy
    (peer, Just deal, Just ping) -> do
      a <- async deal
      b <- async ping
      atomically $ do
        updatePeer peer $ \x -> x { peerAsync = (Just a) }
        updatePeer peer $ \x -> x { peerAsyncPing = (Just b) }

      return peer
    (peer, _, _) -> return peer

destroyPeer :: TVar ZREState -> UUID -> IO ()
destroyPeer s uuid = do
  asyncs <- atomically $ do
    mpt <- lookupPeer s uuid
    case mpt of
      Nothing -> return []
      (Just peer) -> do
        p@Peer{..} <- readTVar peer
        modifyTVar s $ \x -> x { zrePeers = M.delete uuid (zrePeers x) }
        leaveGroups s peer peerGroups peerGroupSeq
        emit s $ Quit p

        return [peerAsync, peerAsyncPing]

  -- this is called from pinger so no more code is executed after this point
  mapM_ cancelM asyncs
  where
    cancelM Nothing = return ()
    cancelM (Just a) = cancel a

pinger :: TVar ZREState -> TVar Peer -> IO b
pinger s peer = forever $ do
  Peer{..} <- atomically $ readTVar peer
  now <- getCurrentTime
  if diffUTCTime now peerLastHeard > deadPeriod
    then destroyPeer s peerUUID
    else do
      let tdiff = diffUTCTime now peerLastHeard
      if tdiff > quietPeriod
        then do
          atomically $ writeTBQueue peerQueue $ Ping
          threadDelay quietPingRate
        else do
          threadDelay $ toDelay (quietPeriod - tdiff)
  where toDelay = round . sec

lookupPeer :: TVar ZREState -> UUID -> STM (Maybe (TVar Peer))
lookupPeer s uuid = do
  st <- readTVar s
  return $ M.lookup uuid $ zrePeers st

updatePeer :: TVar Peer -> (Peer -> Peer) -> STM ()
updatePeer peer fn = modifyTVar peer fn

--updatePeerUUID :: TVar ZREState -> UUID -> (Peer -> Peer) -> STM ()
--updatePeerUUID s uuid fn = do
--  st <- readTVar s
--  case M.lookup uuid $ zrePeers st of
--    Nothing -> return ()
--    (Just peer) -> updatePeer peer fn

updateLastHeard :: TVar Peer -> UTCTime -> STM ()
updateLastHeard peer val = updatePeer peer $ \x -> x { peerLastHeard = val }

-- join `peer` to `group`, update group sequence nuber to `groupSeq`
joinGroup :: TVar ZREState -> TVar Peer -> Group -> GroupSeq -> STM ()
joinGroup s peer group groupSeq = do
  updatePeer peer $ \x -> x { peerGroups = Set.insert group (peerGroups x) }
  updatePeer peer $ \x -> x { peerGroupSeq = groupSeq }
  p <- readTVar peer
  emit s $ GroupJoin p group
  modifyTVar s $ \x -> x { zrePeerGroups = M.alter (f p) group $ zrePeerGroups x }
  where
    f p Nothing = Just $ M.fromList [(peerUUID p, peer)]
    f p (Just old) = Just $ M.insert (peerUUID p) peer old

joinGroups :: TVar ZREState -> TVar Peer -> Set.Set Group -> GroupSeq -> STM ()
joinGroups s peer groups groupSeq = do
  mapM_ (\x -> joinGroup s peer x groupSeq) $ Set.toList groups

leaveGroup :: TVar ZREState -> TVar Peer -> Group -> GroupSeq -> STM ()
leaveGroup s peer group groupSeq = do
  updatePeer peer $ \x -> x { peerGroups = Set.delete group (peerGroups x) }
  updatePeer peer $ \x -> x { peerGroupSeq = groupSeq }
  p <- readTVar peer
  emit s $ GroupLeave p group
  modifyTVar s $ \x -> x { zrePeerGroups = M.alter (f p) group $ zrePeerGroups x }
  where
    f _ Nothing = Nothing
    f p (Just old) = nEmpty $ M.delete (peerUUID p) old
    nEmpty pmap | M.null pmap = Nothing
    nEmpty pmap = Just pmap

leaveGroups :: TVar ZREState -> TVar Peer -> Set.Set Group -> GroupSeq -> STM ()
leaveGroups s peer groups groupSeq = do
  mapM_ (\x -> leaveGroup s peer x groupSeq) $ Set.toList groups

msgPeer :: TVar Peer -> ZRECmd -> STM ()
msgPeer peer msg = do
  p <- readTVar peer
  writeTBQueue (peerQueue p) msg

msgPeerUUID :: TVar ZREState -> UUID -> ZRECmd -> STM ()
msgPeerUUID s uuid msg = do
  st <- readTVar s
  case M.lookup uuid $ zrePeers st of
    Nothing -> return ()
    (Just peer) -> do
      msgPeer peer msg
      return ()

msgAll :: TVar ZREState -> ZRECmd -> STM ()
msgAll s msg = do
  st <- readTVar s
  mapM_ (flip msgPeer msg) (zrePeers st)

msgGroup :: TVar ZREState -> Group -> ZRECmd -> STM ()
msgGroup s groupname msg = do
  st <- readTVar s
  case M.lookup groupname $ zrePeerGroups st of
    Nothing -> return () -- XXX: should report no such group error?
    (Just group) -> do
      mapM_ (flip msgPeer msg) group

printPeers :: M.Map k (TVar Peer) -> IO ()
printPeers x = do
  mapM_ ePrint $ M.elems x
  where
    ePrint pt = do
      p <- atomically $ readTVar pt
      B.putStrLn $ printPeer p

printGroup :: (B.ByteString, M.Map k (TVar Peer)) -> IO ()
printGroup (k,v) = do
  B.putStrLn $ B.intercalate " " ["group", k, "->"]
  printPeers v

printAll :: TVar ZREState -> IO ()
printAll s = do
  st <- atomically $ readTVar s
  printPeers $ zrePeers st
  mapM_ printGroup $ M.toList $ zrePeerGroups st
