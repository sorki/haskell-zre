{-# LANGUAGE TemplateHaskell #-}

module Endpoint where

import Test.QuickCheck.All
import System.ZMQ4.Endpoint

import Arbitrary

prop_endpoint = roundTrip pEndpoint parseAttoEndpoint

return []
runTests :: IO Bool
runTests = $quickCheckAll
