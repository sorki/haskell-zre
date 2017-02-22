{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.ZRE.Parse where

import Control.Applicative

import Data.UUID
import Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

import Network.ZRE.Types

parseAttoApi = A.parseOnly parseApi

parseApi :: Parser API
parseApi = do
  parseControl
--  <|> parseShout

parseControl = char '/' *> parseCmd
--parseShout = DoShout <$> uEOL

parseCmd =
      DoJoin <$> (string "join" *> lskip *> word)
  <|> DoLeave <$> (string "leave" *> lskip *> word)
  <|> DoShout <$> (string "shout" *> lskip *> word) <*> (lskip *> word)
  <|> DoWhisper <$> (string "whisper" *> uuid) <*> lw

lw = lskip *> word
lskip = skipWhile (==' ')
word = A.takeWhile (/=' ')
uEOL = A.takeTill (pure False)

uuid :: Parser UUID
uuid = do
  x <- fromASCIIBytes <$> lw
  case x of
        Nothing -> fail "no uuid"
        Just x -> return x
