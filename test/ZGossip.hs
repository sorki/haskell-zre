{-# LANGUAGE TemplateHaskell #-}

module ZGossip where

import Test.QuickCheck.All
import Data.Maybe

import Data.ZGossip

import Arbitrary

prop_zgs = roundTrip
  (\z -> (\x -> [fromJust $ zgsFrom z, x]) . encodeZGS $ z)
  parseZGS

return []
runTests :: IO Bool
runTests = $quickCheckAll
