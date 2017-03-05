{-# LANGUAGE OverloadedStrings #-}
module Network.ZRE.Utils (
    uuidByteString
  , exitFail
  , bshow
  , getDefRoute
  , getIface
  , randPort
  , emit) where


import System.Exit
import System.Process
import System.Random
import System.ZMQ4.Endpoint
import Network.Info
import Network.ZRE.Types
import Control.Concurrent.STM
import Control.Exception
import Network.Socket

import Data.UUID (UUID, toByteString)
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

uuidByteString :: UUID -> B.ByteString
uuidByteString = BL.toStrict . toByteString

exitFail :: B.ByteString -> IO b
exitFail msg = do
  B.putStrLn msg
  exitFailure

bshow :: (Show a) => a -> B.ByteString
bshow = B.pack . show

getDefRoute :: IO (Maybe (String, String))
getDefRoute = do
  ipr <- fmap lines $ readProcess "ip" ["route"] []
  return $ listToMaybe $ catMaybes $ map getDef (map words ipr)
  where
    getDef ("default":"via":gw:"dev":dev:_) = Just (gw, dev)
    getDef _ = Nothing

getIface :: String -> IO (Maybe NetworkInterface)
getIface iname = do
  ns <- getNetworkInterfaces
  return $ listToMaybe $ filter (\x -> name x == iname) ns

randPort :: B.ByteString -> IO Port
randPort ip = loop (100 :: Int)
  where
    loop cnt = do
      port <- randomRIO (41000, 41010)
      print port
      print cnt
      (xAddr:_) <- getAddrInfo Nothing (Just $ B.unpack ip) (Just $ show port)
      esocket <- try $ getSocket xAddr
      print esocket
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
