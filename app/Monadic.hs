{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Concurrent

import qualified Data.ByteString.Char8 as B


import Data.ZRE
import Network.ZRE
import Network.ZRE.Parse
import Network.ZRE.Peer
import Network.ZRE.Types
import Network.ZRE.Utils

main :: IO ()
main = runZre app

-- v1 non-concurrent

--app events api = runZ ap' events api

--ap = forever $ readZ >>= liftIO .print
--
----readZ = do
----  (e,a) <- ask
----  return
----
--
--ap' = do
--  --concurrentZre recv act
--  writeZ $ join "chat"
--  recv
--  where
--    recv = ap
--    act = ap

-- v2

app events api = concurrentZre (wrap recv) (wrap act)
  where
    recv = dump
    act = ap'
    wrap x = runZ x events api

ap = forever $ readZ >>= liftIO .print

group = "chat"

ap' = do
  zjoin group
  zshout group "ohai"
  forever $ do
    zshout group "vololo"
    liftIO $ threadDelay 1000000

dump = forever $ do
  e <- readZ
  case e of
    (Message ZREMsg{ msgCmd=(Shout group content) })  -> liftIO $ B.putStrLn $ B.concat content
    x -> liftIO $ print x
    --_ -> return ()
