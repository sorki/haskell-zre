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
  let gs@[a, b, c, d] = map mkGroup ["a", "b", "c", "d"]
  mapM_ zjoin gs
  forever $ match [
      isGroupMsg a ==> echo
    , isGroupMsg b ==> rev
    , iff (isGroupMsg c) echo
    , decodeShouts
        (\x -> Right $ B.pack $ show x) -- fake decode fn
        (\x -> either
          zfail
          (zshout d) x
        )
    ]
