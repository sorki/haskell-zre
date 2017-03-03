{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Applicative
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async.Lifted

import qualified Data.ByteString.Char8 as B


import Data.ZRE
import Network.ZRE
import Network.ZRE.Types

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

--app2 :: EventQueue -> APIQueue -> IO ()
--app2 events api = concurrentZre (wrap recv) (wrap act)
--  where
--    recv = dump
--    act = ap'
--    wrap x = runZ x events api

ap :: ZRE ()
ap = forever $ readZ >>= liftIO .print

--group :: Group
--group = "chat"

-- shouts ohai vololo for group, then one vololo per second, forever
ohaivololo :: ZRE ()
ohaivololo = do
  let group = "chat"
  zjoin group
  zshout group "ohai"
  forever $ do
    zshout group "vololo"
    liftIO $ threadDelay 1000000

dump :: ZRE ()
dump = forever $ do
  e <- readZ
  case e of
    (Message ZREMsg{ msgCmd=(Shout _ content) })  -> liftIO $ B.putStrLn $ B.concat content
    x -> liftIO $ print x
    --_ -> return ()

-- v3
-- app events api = wrap $ concurrentZre' (recv) (act)

-- v4
app4 = (recv `concurrently` act)
  where
    recv = dump
    act = ohaivololo

secdelay = liftIO $ threadDelay 1000000

withGroup g = do
  zjoin g
  a <- async $ forever $ zshout g "test" >> secdelay
  forever $ do
    m <- readZ
    --v <- liftIO $ atomically $ readTBQueue e
    case m of
      (Message ZREMsg{ msgCmd=(Shout mg content) }) | mg == g  -> zshout g $ B.concat content -- liftIO $ B.putStrLn $ B.concat content
      _ -> unReadZ m

    --zshout g "test"
    --liftIO $ threadDelay 1000000

-- not good, dump can still consume group msgs
app5 = (withGroup "x") `concurrently` (withGroup "y") `concurrently` dump

app = app5

-- switch to StateT, add acquireChannel releaseChannel, map for routing
