{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- zgossip protocol https://github.com/zeromq/czmq/blob/master/src/zgossip_msg.bnf
-- client sends HELLO, recieves all stored tuples, forwards to other clients

module Data.ZGossip where
import Prelude hiding (putStrLn, take)
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

import GHC.Word

import Data.Binary.Strict.Get
import Data.Binary.Put

import qualified Data.Map as M
import Data.Maybe
import Data.Time.Clock
import Data.Set (Set)
import qualified Data.Set as Set

import System.ZMQ4.Endpoint
import Data.ZMQParse

zgsVer = 1 :: Int
zgsSig = 0xAAA0 :: Word16

type Key = B.ByteString
type Value = B.ByteString
type TTL = Int

data ZSGMsg = ZSGMsg {
    msgFrom :: Maybe B.ByteString
  , msgTime :: Maybe UTCTime
  , msgCmd :: ZSGCmd
  } deriving (Show, Eq, Ord)

data ZSGCmd =
    Hello
  | Publish Key Value TTL
  | Ping
  | PingOk
  | Invalid
  deriving (Show, Eq, Ord)

cmdCode Hello           = 0x01
cmdCode (Publish _ _ _) = 0x02
cmdCode Ping            = 0x03
cmdCode PingOk          = 0x04
cmdCode Invalid         = 0x05

newZSG cmd = ZSGMsg Nothing Nothing cmd

encodeZSG ZSGMsg{..} = msg
  where
    msg = BL.toStrict $ runPut $ do
      putWord16be zgsSig
      putWord8 $ cmdCode msgCmd
      putInt8 $ fromIntegral zgsVer
      encodeCmd msgCmd

encodeCmd (Publish k v ttl) = do
  putByteStringLen k
  putLongByteStringLen v
  putInt32be $ fromIntegral ttl
encodeCmd _ = return ()

parsePublish = Publish
  <$> parseString
  <*> parseLongString
  <*> getInt32

parseCmd from = do
    cmd <- getInt8
    ver <- getInt8

    if ver /= zgsVer
      then fail "Protocol version mismatch"
      else do

        cmd <- case cmd of
          0x01 -> pure Hello
          0x02 -> parsePublish
          0x03 -> pure Ping
          0x04 -> pure PingOk
          0x05 -> pure Invalid
          _    -> fail "Unknown command"

        return $ ZSGMsg (Just from) Nothing cmd

parseZGS [from, msg] = parseZgs from msg
parseZGS _ = (Left "empty message", "")

parseZgs from msg = flip runGet msg $ do
  sig <- getWord16be
  if sig /= zgsSig
    then fail "Signature mismatch"
    else do
      res <- parseCmd from
      return res
