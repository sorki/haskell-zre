{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Async.Lifted

import qualified Data.ByteString.Char8 as B

import Network.ZRE

main :: IO ()
main = runZre app

replyGroup f = do
    m <- readZ
    case m of
      Shout _uuid g content _time -> zshout g $ f $ B.concat content

echo = replyGroup id
rev =  replyGroup B.reverse

dump = readZ >>= liftIO . print
drop' = void $ readZ

handleGroup group action msg = do
    case msg of
      Shout _uuid g _content _time | g == group -> do
        unReadZ msg
        action
        return True
      _ -> return False

passThru action msg = unReadZ msg >> action >> return False

-- match list of handlers against a message, use default handler if not matched
match hs def = do
  m <- readZ
  handled  <- match' hs m
  case handled of
    True -> return ()
    False -> def

  where
    match' :: [Event -> ZRE Bool] -> Event -> ZRE Bool
    match' [] _ = return False
    match' (x:xs) msg = do
      res <- x msg
      case res of
        True -> return True
        False -> match' xs msg

-- join groups and echo messages sent to group a, reverse echo to group b
app = do
  zjoin "a"
  zjoin "b"
  forever $ match [
    passThru dump,
    handleGroup "a" echo,
    handleGroup "b" rev
    ] drop' -- dump'
