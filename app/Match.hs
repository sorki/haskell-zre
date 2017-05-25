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

app = do
  zjoin "a"
  zjoin "b"
  forever $ match [
      handleGroup "a" echo
    , handleGroup "b" rev
    ]

isGroupMsg group (Shout _uuid g _content _time) = g == group
isGroupMsg _ _ = False

handleGroup :: MonadPlus m => Group -> b -> Event -> m b
handleGroup group action msg = do
  guard $ isGroupMsg group msg
  return $ action

match acts = do
  msg <- readZ
  go acts msg
  where
    go (act:rest) m = do
      case act m of
        Nothing -> go rest m
        Just a -> unReadZ m >> a

    go [] _ = return ()
