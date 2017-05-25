{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Async.Lifted

import qualified Data.ByteString.Char8 as B

import Network.ZRE

main :: IO ()
main = runZre app

app :: ZRE ()
app = worker

raw :: ZRE ()
raw = forever $ readZ >>= liftIO .print

workersGroup :: Group
workersGroup = "work"

worker :: ZRE ()
worker = forever $ do
  e <- readZ
  case e of
    Whisper _uuid content _time -> do
      liftIO $ B.putStrLn $ B.concat content
      let grp = B.concat content
      zjoin grp
      void $ async $ forever $ do
        zshout workersGroup msg
        liftIO $ threadDelay (1000000)

    x -> liftIO $ print x

  where msg = B.concat $ replicate 10 "woooork"

dealer :: ZRE ()
dealer = forever $ do
  e <- readZ
  case e of
    (Ready uuid _name _groups _headers _endp) -> do
      zjoin "gimme"
      zwhisper uuid "gimme"
    x -> liftIO $ print x
