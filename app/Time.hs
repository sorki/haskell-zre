{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Async.Lifted
import Data.Time

import qualified Data.ByteString.Char8

import Network.ZRE

timeGroup :: Group
timeGroup = mkGroup "time"

main :: IO ()
main = runZre $ do
  void $ async $ forever $ readZ >>= liftIO . print
  zjoin timeGroup
  forever $ do
    ct <- liftIO $ getCurrentTime
    zshout timeGroup (Data.ByteString.Char8.pack $ show ct)
    liftIO $ threadDelay 1000000
