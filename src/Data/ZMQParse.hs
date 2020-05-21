{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data.ZMQParse (
    getInt8
  , getInt16
  , getInt32
  , parseString
  , parseStrings
  , parseLongString
  , parseKV
  , parseMap
  , putByteStringLen
  , putByteStrings
  , putLongByteStringLen
  , putKV
  , putMap
  , Get.Get()
  , runGet
  , Get.getByteString
  , Put.Put
  , Put.PutM
  , runPut
  , Put.putInt8
  , Put.putWord8
  , Put.putByteString
  , Put.putWord16be
  , Put.putWord32be
  , Put.putInt16be
  , Put.putInt32be
  )
  where

import Prelude hiding (putStrLn, take)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

import Data.Binary.Get hiding (getInt8, runGet)
import Data.Binary.Put hiding (runPut)

import qualified Data.Map as M

import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put

getInt8 :: (Integral a) => Get a
getInt8  = fromIntegral <$> getWord8
getInt16 :: (Integral a) => Get a
getInt16 = fromIntegral <$> getWord16be
getInt32 :: (Integral a) => Get a
getInt32 = fromIntegral <$> getWord32be

parseString :: Get ByteString
parseString = do
  len <- getInt8
  st <- getByteString len
  return st

parseLongString :: Get ByteString
parseLongString = do
  len <- getInt32
  st <- getByteString len
  return st

parseStrings :: Get [ByteString]
parseStrings = do
  count <- getInt32
  res <- sequence $ replicate count parseLongString
  return res

parseKV :: Get (ByteString, ByteString)
parseKV = do
  key <- parseString
  value <- parseLongString
  return (key, value)

parseMap :: Get (M.Map ByteString ByteString)
parseMap = do
  count <- getInt32
  res <- sequence $ replicate count parseKV
  return $ M.fromList res

putByteStringLen :: ByteString -> PutM ()
putByteStringLen x = do
  putInt8 $ fromIntegral $ B.length x
  putByteString x

putLongByteStringLen :: ByteString -> PutM ()
putLongByteStringLen x = do
  putInt32be $ fromIntegral $ B.length x
  putByteString x

putByteStrings :: Foldable t => t ByteString -> PutM ()
putByteStrings x = do
  putInt32be $ fromIntegral $ length x
  mapM_ putLongByteStringLen x

putKV :: (ByteString, ByteString) -> PutM ()
putKV (k, v) = do
  putByteStringLen k
  putLongByteStringLen v

putMap :: M.Map ByteString ByteString -> PutM ()
putMap m = do
  putInt32be $ fromIntegral $ length ml
  mapM_ putKV ml
  where ml = M.toList m

runGet :: Get a -> ByteString -> Either String a
runGet g b = case Get.runGetOrFail g (BL.fromStrict b) of
  (Left (_unconsumed, _offset, err)) -> Left err
  (Right (_unconsumed, _offset, res)) -> Right res

runPut :: Put -> ByteString
runPut = BL.toStrict . Put.runPut
