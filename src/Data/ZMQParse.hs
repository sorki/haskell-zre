{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data.ZMQParse where

import Prelude hiding (putStrLn, take)
import qualified Data.ByteString.Char8 as B

import Data.Binary.Strict.Get
import Data.Binary.Put

import qualified Data.Map as M

getInt8 :: (Integral a) => Get a
getInt8  = fromIntegral <$> getWord8
getInt16 :: (Integral a) => Get a
getInt16 = fromIntegral <$> getWord16be
getInt32 :: (Integral a) => Get a
getInt32 = fromIntegral <$> getWord32be

parseString :: Get B.ByteString
parseString = do
  len <- getInt8
  st <- getByteString len
  return st

parseLongString :: Get B.ByteString
parseLongString = do
  len <- getInt32
  st <- getByteString len
  return st

parseStrings :: Get [B.ByteString]
parseStrings = do
  count <- getInt32
  res <- sequence $ replicate count parseLongString
  return res

parseKV :: Get (B.ByteString, B.ByteString)
parseKV = do
  key <- parseString
  value <- parseLongString
  return (key, value)

parseMap :: Get (M.Map B.ByteString B.ByteString)
parseMap = do
  count <- getInt32
  res <- sequence $ replicate count parseKV
  return $ M.fromList res

putByteStringLen :: B.ByteString -> PutM ()
putByteStringLen x = do
  putInt8 $ fromIntegral $ B.length x
  putByteString x

putLongByteStringLen :: B.ByteString -> PutM ()
putLongByteStringLen x = do
  putInt32be $ fromIntegral $ B.length x
  putByteString x

putByteStrings :: Foldable t => t B.ByteString -> PutM ()
putByteStrings x = do
  putInt32be $ fromIntegral $ length x
  mapM_ putLongByteStringLen x

putKV :: (B.ByteString, B.ByteString) -> PutM ()
putKV (k, v) = do
  putByteStringLen k
  putLongByteStringLen v

putMap :: M.Map B.ByteString B.ByteString -> PutM ()
putMap m = do
  putInt32be $ fromIntegral $ length ml
  mapM_ putKV ml
  where ml = M.toList m
