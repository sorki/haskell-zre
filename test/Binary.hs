{-# LANGUAGE TemplateHaskell #-}

module Binary where

import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Instances
import System.Exit

import Data.ZMQParse

roundTrip :: Eq a => (a -> Put) -> Get a -> a -> Bool
roundTrip putter getter x =
    Right x == runGet getter (runPut (putter x))

prop_i8 = roundTrip putInt8 getInt8
prop_i16 = roundTrip putInt16be getInt16
prop_i32 = roundTrip putInt32be getInt32

prop_str = roundTrip putByteStringLen parseString

prop_long_str = roundTrip putLongByteStringLen parseLongString

prop_strings =  roundTrip putByteStrings parseStrings

prop_kv = roundTrip putKV parseKV

prop_map = roundTrip putMap parseMap

return []
runTests :: IO Bool
runTests = $quickCheckAll
