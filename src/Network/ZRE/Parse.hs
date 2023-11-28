{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.ZRE.Parse (parseApi, parseAttoApi) where

import Control.Applicative

import Data.ByteString (ByteString)
import Data.UUID
import Data.Attoparsec.ByteString.Char8 as A

import Data.ZRE (mkGroup)
import Network.ZRE.Types

parseAttoApi :: ByteString -> Either String API
parseAttoApi = A.parseOnly parseApi

parseApi :: Parser API
parseApi = do
  parseControl

parseControl :: Parser API
parseControl = char '/' *> parseCmd

parseCmd :: Parser API
parseCmd =
      DoJoin . mkGroup  <$> (string "join" *> lskip *> word)
  <|> DoLeave . mkGroup <$> (string "leave" *> lskip *> word)
  -- TODO quoted shouts
  <|> DoShout <$> (string "shout" *> lskip *> (mkGroup <$> word)) <*> (lskip *> word)
  <|> DoWhisper <$> (string "whisper" *> uuid) <*> lw
  <|> DoDebug <$> (string "debug" *> pure True)
  <|> DoDebug <$> (string "nodebug" *> pure False)
  <|> (string "quit" >> pure DoQuit)

lw :: Parser ByteString
lw = lskip *> word

lskip :: Parser ()
lskip = skipWhile (==' ')

word :: Parser ByteString
word = A.takeWhile (/=' ')

--uEOL :: Parser ByteString
--uEOL = A.takeTill (pure False)

uuid :: Parser UUID
uuid = do
  mx <- fromASCIIBytes <$> lw
  case mx of
        Nothing -> fail "no uuid"
        Just x -> return x
