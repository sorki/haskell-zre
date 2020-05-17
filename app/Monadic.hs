{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Async.Lifted

import Network.ZRE

main :: IO ()
main = runZre app

app :: ZRE ((), ())
app = (recv `concurrently` act)
  where
    recv = dump
    act = ohaivololo

-- shouts ohai vololo for group, then one vololo per second, forever
ohaivololo :: ZRE ()
ohaivololo = do
  let group = mkGroup "chat"
  zjoin group
  zshout group "ohai"
  forever $ do
    zshout group "vololo"
    liftIO $ threadDelay 1000000

dump :: ZRE ()
dump = forever $ do
  e <- readZ
  case e of
    x -> liftIO $ print x
