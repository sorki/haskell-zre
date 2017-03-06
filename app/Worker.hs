{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Concurrent
import Control.Concurrent.Async.Lifted

import qualified Data.ByteString.Char8 as B

import Network.ZRE

main :: IO ()
main = runZre app

app = worker

raw = forever $ readZ >>= liftIO .print

worker = forever $ do
  e <- readZ
  case e of
    Whisper _uuid content _time -> do
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
    (Ready uuid _name _groups _headers _endp) -> do
      zjoin "gimme"
      zwhisper uuid "gimme"
    x -> liftIO $ print x

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
    Shout _uuid group content time  -> liftIO $ B.putStrLn $ B.concat content
    x -> liftIO $ print x
    --_ -> return ()
