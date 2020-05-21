{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.ByteString (ByteString)

import qualified Control.Monad
import qualified Data.ByteString.Char8

import Network.ZRE

main :: IO ()
main = runZre app

replyGroup :: (ByteString -> ByteString) -> ZRE ()
replyGroup f = do
    m <- readZ
    case m of
      Shout _uuid g content _time -> zshout g
        $ f
        $ Data.ByteString.Char8.concat content

      _ -> return ()

echo :: ZRE ()
echo = replyGroup id

rev :: ZRE ()
rev =  replyGroup Data.ByteString.Char8.reverse

app :: ZRE ()
app = do
  let gs@[a, b, c, d] = map mkGroup ["a", "b", "c", "d"]
  mapM_ zjoin gs
  Control.Monad.forever $ match [
      isGroupMsg a ==> echo
    , isGroupMsg b ==> rev
    , iff (isGroupMsg c) echo
    , decodeShouts
        (\x -> Right $ Data.ByteString.Char8.pack $ show x) -- fake decode fn
        (\x -> either
          zfail
          (zshout d) x
        )
    ]
