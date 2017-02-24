{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Concurrent
--import Control.Concurrent.Async
import Control.Concurrent.Async.Lifted

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

--app events api = runZ worker events api
app events api = runZ test events api

--test = control $ \run -> print "kokot" -- dump -- `concurrently` worker
--test = dump `concurrently` worker
--test = dealer
test = worker

raw = forever $ readZ >>= liftIO .print

worker = forever $ do
  e <- readZ
  case e of
    (Message ZREMsg{ msgCmd=(Whisper content) }) -> do
      liftIO $ B.putStrLn $ B.concat content
      let group = B.concat content
      zjoin group
      void $ async $ forever $ zshout group msg >> (liftIO $ threadDelay (1000000))
      --forever $ zshout group msg >> (liftIO $ threadDelay (1000000))
      --replicateM_ 10 $ zshout group msg >> (liftIO $ threadDelay (1000000))
    x -> liftIO $ print x

  where msg = B.concat $ replicate 10 "woooork"

dealer = forever $ do
  e <- readZ
  case e of
    (Ready p) -> do
      zjoin "gimme"
      zwhisper (peerUUID p) "gimme"
    x -> liftIO $ print x
--
----readZ = do
----  (e,a) <- ask
----  return
----
--

-- v2

app' events api = concurrentZre (wrap recv) (wrap act)
  where
    recv = dump
    act = ap'
    wrap x = runZ x events api

--ap = forever $ readZ >>= liftIO .print

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
