module Main where

import System.Exit

import qualified Endpoint
import qualified ZGossip
import qualified ZRE
import qualified Binary

main :: IO ()
main = do
  good <- and <$> sequence [
      Binary.runTests
    , Endpoint.runTests
    , ZGossip.runTests
    , ZRE.runTests
    ]
  if good
     then exitSuccess
     else exitFailure
