{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Async.Lifted

import qualified Data.ByteString.Char8 as B

import System.IO
import System.Environment

import Network.ZRE

main :: IO ()
main = do
  args <- getArgs
  let group = B.pack $ head args :: B.ByteString

  runZre $ groupCat group

groupCat :: Group -> ZRE ()
groupCat group = do
  zjoin group
  -- wait a bit so join is received by peers before sending stuff
  void $ async $ (liftIO $ threadDelay 500000) >> (stdin' group)
  cat

catln = forever $ do
  e <- readZ
  case e of
    Shout _uuid group content time  -> liftIO $ B.putStrLn $ B.concat content
    Whisper _uuid content _time -> liftIO $ B.putStrLn $ B.concat content
    _ -> return ()

cat = forever $ do
  e <- readZ
  case e of
    Shout _uuid group content time  -> liftIO $ B.putStr $ B.concat content
    Whisper _uuid content _time -> liftIO $ B.putStr $ B.concat content
    _ -> return ()

stdinln group = forever $ do
  l <- fmap B.pack $ liftIO getLine
  zshout group l

bufsize = 1024*128

stdin' group = do
  liftIO $ hSetBuffering stdin $ BlockBuffering (Just bufsize)
  forever $ do
    buf <- liftIO $ B.hGetSome stdin bufsize
    zshout group buf
