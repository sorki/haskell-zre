{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.ZRE.Parse (parseApi, parseAttoApi) where

import Control.Applicative

import Data.UUID
import Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B

import Network.ZRE.Types

parseAttoApi :: B.ByteString -> Either String API
parseAttoApi = A.parseOnly parseApi

parseApi :: Parser API
parseApi = do
  parseControl

parseControl :: Parser API
parseControl = char '/' *> parseCmd

parseCmd :: Parser API
parseCmd =
      DoJoin <$> (string "join" *> lskip *> word)
  <|> DoLeave <$> (string "leave" *> lskip *> word)
  <|> DoShout <$> (string "shout" *> lskip *> word) <*> (lskip *> word)
  <|> DoWhisper <$> (string "whisper" *> uuid) <*> lw
  <|> DoDebug <$> (string "debug" *> pure True)
  <|> DoDebug <$> (string "nodebug" *> pure False)
  <|> (string "quit" >> pure DoQuit)

lw :: Parser B.ByteString
lw = lskip *> word

lskip :: Parser ()
lskip = skipWhile (==' ')

word :: Parser B.ByteString
word = A.takeWhile (/=' ')

--uEOL :: Parser B.ByteString
--uEOL = A.takeTill (pure False)

uuid :: Parser UUID
uuid = do
  mx <- fromASCIIBytes <$> lw
  case mx of
        Nothing -> fail "no uuid"
        Just x -> return x
