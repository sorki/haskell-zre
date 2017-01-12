{-# LANGUAGE OverloadedStrings #-}
module Network.ZRE.Utils where

import System.Exit
import System.Process
import System.Random
import System.ZMQ4.Endpoint
import Network.Info
import Network.ZRE.Types
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue

import Data.UUID (toByteString)
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

uuidByteString = BL.toStrict . toByteString

exitFail msg = do
  B.putStrLn msg
  exitFailure

bshow :: (Show a) => a -> B.ByteString
bshow = B.pack . show

getDefRoute = do
  ipr <- fmap lines $ readProcess "ip" ["route"] []
  return $ listToMaybe $ catMaybes $ map getDef (map words ipr)
  where
    getDef ("default":"via":gw:"dev":dev:_) = Just (gw, dev)
    getDef _ = Nothing

getIface iname = do
  ns <- getNetworkInterfaces
  return $ listToMaybe $ filter (\x -> name x == iname) ns

-- FIXME: try binding to it up to n times
-- randPort = loop (n) where loop = rport >>= bind ..
randPort :: IO Port
randPort = randomRIO (41000, 65536)

emit s x = do
  st <- readTVar s
  writeTBQueue (zreIn st) x
