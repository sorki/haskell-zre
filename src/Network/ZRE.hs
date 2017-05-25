{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.ZRE (
    runZre
  , runZre'
  , readZ
  , writeZ
  , unReadZ
  , defaultConf
  , API(..)
  , Event(..)
  , ZRE
  , Z.Group
  , zjoin
  , zleave
  , zshout
  , zshout'
  , zwhisper
  , zdebug
  , znodebug
  , zquit
  , pEndpoint
  , toASCIIBytes) where

import Prelude hiding (putStrLn, take)
import Control.Monad hiding (join)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM

import Data.UUID
import Data.UUID.V1
import Data.Maybe

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B

import qualified Data.ZRE as Z
import Network.ZRE.Beacon
import Network.ZRE.Utils
import Network.ZRE.Peer
import Network.ZRE.ZMQ
import Network.ZRE.Types
import System.ZMQ4.Endpoint

import Network.ZGossip

import Options.Applicative
import Data.Semigroup ((<>))

parseCfg :: Parser ZRECfg
parseCfg = ZRECfg
  <$> (B.pack <$> strOption
        (long "name"
      <> short 'n'
      <> value ""
      <> help "Node name"))
  <*> (isec <$> option auto
        (long "quiet-period"
      <> short 'q'
      <> metavar "N"
      <> value (sec (1.0 :: Float))
      <> help "Ping peer after N seconds"))
  <*> ((*100000) <$> option auto
        (long "dead-period"
      <> short 'd'
      <> metavar "N"
      <> value (sec (1.0 :: Float))
      <> help "Mark peer dead after N seconds"))
  <*> ((*100000) <$> option auto
         (long "beacon-period"
      <> short 'b'
      <> metavar "N"
      <> value (sec (0.9 :: Float))
      <> help "Send beacon every N seconds"))
  <*> ((map B.pack) <$> many (strOption
        (long "iface"
      <> short 'i'
      <> metavar "IFACE"
      <> help "Interfaces")))
  <*> option (attoReadM parseAttoUDPEndpoint)
        (long "mcast"
      <> short 'm'
      <> metavar "IP:PORT"
      <> value defMCastEndpoint
      <> help "IP:PORT of the multicast group")
  <*> optional (option (attoReadM parseAttoTCPEndpoint)
        (long "gossip"
      <> short 'g'
      <> metavar "IP:PORT"
      <> help "IP:PORT of the gossip server"))

attoReadM :: (B.ByteString -> Either String a) -> ReadM a
attoReadM p = eitherReader (p . B.pack)

runZre :: ZRE a -> IO ()
runZre app = do
  cfg <- execParser opts
  print cfg
  runZre' cfg app
  where
    opts = info (parseCfg <**> helper)
      ( fullDesc
     <> progDesc "ZRE"
     <> header "zre tools" )

getIfaces :: [B.ByteString]
          -> IO [(B.ByteString, B.ByteString, B.ByteString)]
getIfaces ifcs = do
  case ifcs of
    [] -> do
      dr <- getDefRoute
      case dr of
        Nothing -> exitFail "Unable to get default route"
        Just (_route, iface) -> do
          i <- getIfaceReport iface
          return $ [i]
    x  -> do
      forM x getIfaceReport

runIface :: Show a
         => TVar ZREState
         -> Int
         -> (B.ByteString, B.ByteString, a)
         -> IO ()
runIface s port (iface, ipv4, ipv6) = do
   print ["Bind to", bshow port, bshow iface, bshow ipv4, bshow ipv6]
   r <- async $ zreRouter (newTCPEndpoint ipv4 port) (inbox s)
   atomically $ modifyTVar s $ \x ->
     x { zreIfaces = M.insert iface [r] (zreIfaces x) }

runZre' :: ZRECfg -> ZRE a -> IO ()
runZre' ZRECfg{..} app = do
    ifcs <- getIfaces zreInterfaces

    u <- maybeM (exitFail "Unable to get UUID") return nextUUID
    let uuid = uuidByteString u

    case ifcs of
      [] -> exitFail "No interfaces found"
      ifaces@((_ifcname, ipv4, _ipv6):_) -> do
        zrePort <- randPort ipv4

        let zreEndpoint = newTCPEndpoint ipv4 zrePort
        print zreEndpoint

        zreName <- getName zreNamed

        inQ <- atomically $ newTBQueue 1000
        outQ <- atomically $ newTBQueue 1000

        s <- newZREState zreName zreEndpoint u inQ outQ

        -- FIXME: support multiple gossip clients
        case zreZGossip of
          Nothing -> return ()
          Just end -> void $ async $ zgossipClient uuid end zreEndpoint (zgossipZRE outQ)

        (mCastAddr:_) <- toAddrInfo zreMCast
        _beaconAsync <- async $ beacon mCastAddr uuid zrePort
        _beaconRecvAsync <- async $ beaconRecv s zreMCast
        apiAsync <- async $ api s
        _userAppAsync <- async $ runZ app inQ outQ

        mapM_ (runIface s zrePort) ifaces

        wait apiAsync
        --wait userAppAsync
        return ()

api :: TVar ZREState -> IO ()
api s = do
  a <- atomically $ readTVar s >>= readTBQueue . zreOut
  handleApi s a
  case a of
    DoQuit -> return ()
    _ -> api s

handleApi :: TVar ZREState -> API -> IO ()
handleApi s act = do
  case act of
    DoJoin group -> atomically $ do
      incGroupSeq
      modifyTVar s $ \x -> x { zreGroups = S.insert group (zreGroups x) }
      st <- readTVar s
      msgAllJoin s group (zreGroupSeq st)

    DoLeave group -> atomically $ do
      incGroupSeq
      modifyTVar s $ \x -> x { zreGroups = S.delete group (zreGroups x) }
      st <- readTVar s
      msgAllLeave s group (zreGroupSeq st)

    DoShout group msg -> atomically $ shoutGroup s group msg
    DoShoutMulti group mmsg -> atomically $ shoutGroupMulti s group mmsg
    DoWhisper uuid msg -> atomically $ whisperPeerUUID s uuid msg

    DoDiscover uuid endpoint -> do
      mp <- atomically $ lookupPeer s uuid
      case mp of
        Just _ -> return ()
        Nothing -> do
          void $ makePeer s uuid $ newPeerFromEndpoint endpoint

    DoDebug bool -> atomically $ modifyTVar s $ \x -> x { zreDebug = bool }

    DoQuit -> do
      -- FIXME: wait for empty peer queues
      threadDelay (sec (1.0 :: Float))
  where
    incGroupSeq = modifyTVar s $ \x -> x { zreGroupSeq = (zreGroupSeq x) + 1 }

-- handles incoming ZRE messages
-- creates peers, updates state
inbox :: TVar ZREState -> Z.ZREMsg -> IO ()
inbox s msg@Z.ZREMsg{..} = do
  let uuid = fromJust msgFrom

  -- print msg , "state pre-msg", printAll s

  mpt <- atomically $ lookupPeer s uuid
  case mpt of
    Nothing -> do
      case msgCmd of
        -- if the peer is not known but a message is HELLO we create a new
        -- peer, for other messages we don't know the endpoint to connect to
        h@(Z.Hello _endpoint _groups _groupSeq _name _headers) -> do
          peer <- makePeer s uuid $ newPeerFromHello h
          atomically $ updatePeer peer $ \x -> x { peerSeq = (peerSeq x) + 1 }
        -- silently drop any other messages
        _ -> return ()

    (Just peer) -> do
      atomically $ updateLastHeard peer $ fromJust msgTime

      -- destroy/re-start peer when this doesn't match
      p <- atomically $ readTVar peer
      case peerSeq p == msgSeq of
        True -> do
          -- rename to peerExpectSeq, need to update at line 127 too
          atomically $ updatePeer peer $ \x -> x { peerSeq = (peerSeq x) + 1 }
          handleCmd s msg peer
        _ -> do
          atomically $ emitdbg s "sequence mismatch, recreating peer"
          recreatePeer (peerUUID p) msgCmd

  -- "state post-msg", printAll s
  where
    recreatePeer uuid h@(Z.Hello _ _ _ _ _) = do
          destroyPeer s uuid
          peer <- makePeer s uuid $ newPeerFromHello h
          atomically $ updatePeer peer $ \x -> x { peerSeq = (peerSeq x) + 1 }
    recreatePeer uuid _ = destroyPeer s uuid

handleCmd :: TVar ZREState -> Z.ZREMsg -> TVar Peer -> IO ()
handleCmd s Z.ZREMsg{msgFrom=(Just from), msgTime=(Just time), msgCmd=cmd} peer = do
      case cmd of
        (Z.Whisper content) -> atomically $ do
          emit s $ Whisper from content time
          emitdbg s $ B.intercalate " " ["whisper", B.concat content]

        Z.Shout group content -> atomically $ do
          emit s $ Shout from group content time
          emitdbg s $ B.intercalate " " ["shout for group", group, ">", B.concat content]

        Z.Join group groupSeq -> atomically $ do
          joinGroup s peer group groupSeq
          emitdbg s $ B.intercalate " " ["join", group, bshow groupSeq]

        Z.Leave group groupSeq -> atomically $ do
          leaveGroup s peer group groupSeq
          emitdbg s $ B.intercalate " " ["leave", group, bshow groupSeq]

        Z.Ping -> atomically $ do
          msgPeer peer Z.PingOk
          emitdbg s $ "ping"
        Z.PingOk -> return ()
        Z.Hello endpoint groups groupSeq name headers -> do
          -- if this peer was already registered
          -- (e.g. from beacon) update appropriate data
          atomically $ do
            joinGroups s peer groups groupSeq
            updatePeer peer $ \x -> x {
                         peerName = Just name
                       , peerHeaders = headers
                       }
            p <- readTVar peer
            emit s $ Ready (peerUUID p) name groups headers endpoint
            emitdbg s $ "update peer"
          return ()
handleCmd _ _ _ = return ()
