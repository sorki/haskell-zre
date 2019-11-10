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
  zjoin "c"
  zjoin "d"
  forever $ match [
      isGroupMsg "a" ==> echo
    , isGroupMsg "b" ==> rev
    , iff (isGroupMsg "c") echo
    , decodeShouts (\x -> Right $ B.pack $ show x) (\x -> zshout "d" x)
    ]
