{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data.ZRE where
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
import Data.UUID

import System.ZMQ4.Endpoint

zreVer = 2 :: Int
zreSig = 0xAAA1 :: Word16

type Seq = Int
type GroupSeq = Int
type Group = B.ByteString
type Groups = [Group]
type Name = B.ByteString
type Headers = M.Map B.ByteString B.ByteString
type Content = [B.ByteString]


data ZREMsg = ZREMsg {
    msgFrom :: Maybe UUID
  , msgSeq :: Seq
  , msgCmd :: ZRECmd
  } deriving (Show, Eq, Ord)

data ZRECmd =
    Hello Endpoint Groups GroupSeq Name Headers
  | Whisper Content
  | Shout Group Content
  | Join Group GroupSeq
  | Leave Group GroupSeq
  | Ping
  | PingOk
  deriving (Show, Eq, Ord)

-- to utils
getInt8 :: Get Int
getInt8  = fromIntegral <$> getWord8
getInt16 = fromIntegral <$> getWord16be
getInt32 = fromIntegral <$> getWord32be

zreBeacon uuid port = BL.toStrict $ runPut $ do
  putByteString "ZRE"
  putInt8 $ fromIntegral zreVer
  putByteString uuid
  putInt16be $ fromIntegral port

parseUUID =  do
  muuid <- fromByteString . BL.fromStrict <$> getByteString 16
  case muuid of
    Just uuid -> return uuid
    Nothing -> fail "Unable to parse UUID"

parseBeacon = runGet $ do
  lead <- getByteString 3
  ver <- fromIntegral <$> getWord8
  uuid <- parseUUID
  port <- fromIntegral <$> getWord16be
  return (lead, ver, uuid, port)

putByteStringLen x = do
  putInt8 $ fromIntegral $ B.length x
  putByteString x

putLongByteStringLen x = do
  putInt16be $ fromIntegral $ B.length x
  putByteString x

putByteStrings x = do
  putInt32be $ fromIntegral $ length x
  mapM_ putLongByteStringLen x
  where lx = length x

putKV (k, v) = do
  putByteStringLen k
  putLongByteStringLen v

putMap map = do
  count <- putInt32be $ fromIntegral $ length ml
  mapM_ putKV ml
  where ml = M.toList map

genZre = BL.toStrict $ runPut $ do
  putWord16be zreSig
  putWord8 0xd1 -- cmd
  putInt8 $ fromIntegral zreVer
  putWord16be 1337
  putByteStringLen "endpoint:1337"
  putByteStrings ["group1", "group2"]
  putInt8 7
  putByteStringLen "name"
  putMap (M.fromList [("abc", "val1"), ("qqq", "val2")])

cmdCode (Hello _ _ _ _ _) = 0xd1
cmdCode (Whisper _) = 0xd2
cmdCode (Shout _ _) = 0xd3
cmdCode (Join _ _) = 0xd4
cmdCode (Leave _ _) = 0xd5
cmdCode Ping = 0xd6
cmdCode PingOk = 0xd7

getContent (Whisper c) = c
getContent (Shout _ c) = c
getContent _ = []

newZRE seqNum cmd = ZREMsg Nothing seqNum cmd

encodeZRE ZREMsg{..} = msg:(getContent msgCmd)
  where
    msg = BL.toStrict $ runPut $ do
      putWord16be zreSig
      putWord8 $ cmdCode msgCmd
      putInt8 $ fromIntegral zreVer
      putInt16be $ fromIntegral msgSeq
      encodeCmd msgCmd

encodeCmd (Hello endpoint groups statusSeq name headers) = do
  putByteStringLen (pEndpoint endpoint)
  putByteStrings groups
  putInt8 $ fromIntegral statusSeq
  putByteStringLen name
  putMap headers
encodeCmd (Shout group content) = putByteStringLen group
encodeCmd (Join group statusSeq) = do
  putByteStringLen group
  putInt8 $ fromIntegral statusSeq
encodeCmd (Leave group statusSeq) = do
  putByteStringLen group
  putInt8 $ fromIntegral statusSeq
encodeCmd _ = return ()

parseString = do
  len <- getInt8
  st <- getByteString len
  return st

parseLongString = do
  len <- getInt16
  st <- getByteString len
  return st

parseStrings = do
  count <- getInt32
  res <- sequence $ replicate count parseLongString
  return res

parseKV = do
  key <- parseString
  value <- parseLongString
  return (key, value)

parseMap = do
  count <- getInt32
  res <- sequence $ replicate count parseKV
  return $ M.fromList res

parseHello = Hello
  <$> parseEndpoint'
  <*> parseStrings
  <*> getInt8
  <*> parseString
  <*> parseMap
  where
    parseEndpoint' = do
      s <- parseString
      case parseAttoEndpoint s of
        (Left err) -> fail $ "Unable to parse endpoint: " ++ err
        (Right endpoint) -> return endpoint

parseShout frames = Shout <$> parseString <*> pure frames
parseJoin = Join <$> parseString <*> getInt8
parseLeave = Leave <$> parseString <*> getInt8

parseCmd from frames = do
    cmd <- getInt8
    ver <- getInt8
    seq <- getInt16

    case runGet parseUUID from of
      (Left err, _) -> fail "No UUID"
      (Right uuid, _)-> do
        if ver /= zreVer
          then fail "Protocol version mismatch"
          else do

            cmd <- case cmd of
              0xd1 -> parseHello
              0xd2 -> pure $ Whisper frames -- parseWhisper
              0xd3 -> parseShout frames
              0xd4 -> parseJoin
              0xd5 -> parseLeave
              0xd6 -> pure Ping
              0xd7 -> pure PingOk
              _    -> fail "Unknown command"

            return $ ZREMsg (Just uuid) seq cmd

parseZRE (from:msg:rest) = parseZre from msg rest
parseZRE _ = (Left "empty message", "")

parseZre from msg frames = flip runGet msg $ do
  sig <- getWord16be
  if sig /= zreSig
    then fail "Signature mismatch"
    else do
      res <- parseCmd from frames
      return res
