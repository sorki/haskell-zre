{-# LANGUAGE TemplateHaskell #-}

module ZRE where

import Test.QuickCheck.All
import Data.Maybe

import Data.UUID (toByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

import Arbitrary
import Data.ZRE

prop_zre = roundTrip
  (\z -> (\x -> ('1' `B.cons` (BL.toStrict $ toByteString $ fromJust $ msgFrom z)):x) . encodeZRE $ z)
  parseZRE

return []
runTests :: IO Bool
runTests = $quickCheckAll
