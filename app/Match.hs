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


import Data.ZRE
import Network.ZRE
import Network.ZRE.Types

main :: IO ()
main = runZre app

replyGroup f = do
    m <- readZ
    case m of
      (Message ZREMsg{ msgCmd=(Shout g content) }) -> zshout g $ f $ B.concat content

echo = replyGroup id
rev =  replyGroup B.reverse

dump = readZ >>= liftIO . print
drop' = void $ readZ

handleGroup group action msg = do
    case msg of
      (Message ZREMsg{ msgCmd=(Shout sgroup content) }) | sgroup == group -> do
        unReadZ msg
        action
        return True
      _ -> return False

passThru action msg = unReadZ msg >> action >> return False

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

app = do
  zjoin "a"
  zjoin "b"
  forever $ match [
    passThru dump,
    handleGroup "a" echo,
    handleGroup "b" rev
    ] drop' -- dump'
