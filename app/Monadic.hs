{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async.Lifted

import qualified Data.ByteString.Char8 as B

import Network.ZRE

main :: IO ()
main = runZre app

app = (recv `concurrently` act)
  where
    recv = dump
    act = ohaivololo

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
--    (Message ZREMsg{ msgCmd=(Shout _ content) })  -> liftIO $ B.putStrLn $ B.concat content
    x -> liftIO $ print x
    --_ -> return ()
