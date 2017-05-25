{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad

import qualified Data.ByteString.Char8 as B

import Network.ZRE

main :: IO ()
main = runZre app

replyGroup :: (B.ByteString -> B.ByteString) -> ZRE ()
replyGroup f = do
    m <- readZ
    case m of
      Shout _uuid g content _time -> zshout g $ f $ B.concat content
      _ -> return ()

echo :: ZRE ()
echo = replyGroup id

rev :: ZRE ()
rev =  replyGroup B.reverse

app :: ZRE ()
app = do
  zjoin "a"
  zjoin "b"
  forever $ match [
      handleGroup "a" echo
    , handleGroup "b" rev
    ]

isGroupMsg :: Group -> Event -> Bool
isGroupMsg group (Shout _uuid g _content _time) = g == group
isGroupMsg _ _ = False

handleGroup :: MonadPlus m => Group -> b -> Event -> m b
handleGroup group action msg = do
  guard $ isGroupMsg group msg
  return $ action

match :: [Event -> Maybe (ZRE ())] -> ZRE ()
match acts = do
  msg <- readZ
  go acts msg
  where
    go (act:rest) m = do
      case act m of
        Nothing -> go rest m
        Just a -> unReadZ m >> a

    go [] _ = return ()
