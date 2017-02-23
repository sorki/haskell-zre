{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data.ZMQParse where

import Prelude hiding (putStrLn, take)
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import qualified Data.ByteString.Char8 as B

import GHC.Word

import Data.Binary.Strict.Get
import Data.Binary.Put

import qualified Data.Map as M
import Data.Maybe
import Data.UUID
import Data.Time.Clock
import Data.Set (Set)
import qualified Data.Set as Set

getInt8 :: (Integral a) => Get a
getInt8  = fromIntegral <$> getWord8
getInt16 :: (Integral a) => Get a
getInt16 = fromIntegral <$> getWord16be
getInt32 :: (Integral a) => Get a
getInt32 = fromIntegral <$> getWord32be

parseString = do
  len <- getInt8
  st <- getByteString len
  return st

parseLongString = do
  len <- getInt32
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

putByteStringLen x = do
  putInt8 $ fromIntegral $ B.length x
  putByteString x

putLongByteStringLen x = do
  putInt32be $ fromIntegral $ B.length x
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
