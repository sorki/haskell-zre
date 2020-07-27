{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.ZRE.Utils (
    uuidByteString
  , exitFail
  , bshow
  , getDefRoute
  , getIface
  , getIfaceReport
  , getName
  , randPort
  , emit
  , emitdbg
  ) where


import Data.ByteString (ByteString)

import System.Exit
import System.Process
import System.Random
import System.ZMQ4.Endpoint
import Network.BSD (getHostName)
import Network.Info
import Network.ZRE.Types
import Control.Concurrent.STM
import Control.Exception
import Network.Socket hiding (Debug)

import Data.UUID (UUID, toByteString)
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

uuidByteString :: UUID -> ByteString
uuidByteString = BL.toStrict . toByteString

exitFail :: ByteString -> IO b
exitFail msg = do
  B.putStrLn msg
  exitFailure

bshow :: (Show a) => a -> ByteString
bshow = B.pack . show

getDefRoute :: IO (Maybe (ByteString, ByteString))
getDefRoute = do
  ipr <- fmap lines $ readProcess "ip" ["route"] []
  return $ listToMaybe $ catMaybes $ map getDef (map words ipr)
  where
    getDef ("default":"via":gw:"dev":dev:_) = Just (B.pack gw, B.pack dev)
    getDef _ = Nothing

getIface :: ByteString -> IO (Maybe NetworkInterface)
getIface iname = do
  ns <- getNetworkInterfaces
  return $ listToMaybe $ filter (\x -> name x == B.unpack iname) ns

getIfaceReport :: ByteString
               -> IO (ByteString, ByteString, ByteString)
getIfaceReport iname = do
  i <- getIface iname
  case i of
    Nothing -> exitFail $ "Unable to get info for interface " `B.append` iname
    (Just NetworkInterface{..}) -> return (iname, B.pack $ show ipv4, B.pack $ show ipv6)

getName :: ByteString -> IO ByteString
getName "" = fmap B.pack getHostName
getName x  = return x

randPort :: ByteString -> IO Port
randPort ip = loop (100 :: Int)
  where
    loop cnt = do
      port <- randomRIO (41000, 41100)
      (xAddr:_) <- getAddrInfo Nothing (Just $ B.unpack ip) (Just $ show port)
      esocket <- try $ getSocket xAddr
      case esocket :: Either IOException Socket of
        Left e
            | cnt <= 1 -> error $ concat
                [ "Unable to bind to random port, last tried was "
                , show port
                , ". Exception was: "
                , show e
                ]
            | otherwise -> do
                loop $! cnt - 1

        Right s -> do
          close s
          return port

    getSocket addr = do
     s <- socket (addrFamily addr) Stream defaultProtocol
     bind s (addrAddress addr)
     return s

emit :: TVar ZREState -> Event -> STM ()
emit s x = do
  st <- readTVar s
  writeTBQueue (zreIn st) x

emitdbg :: TVar ZREState -> ByteString -> STM ()
emitdbg s x = do
  st <- readTVar s
  case zreDebug st of
    True -> writeTBQueue (zreIn st) $ Debug x
    _ -> return ()
