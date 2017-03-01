{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative
import Control.Monad (forever)
--import Control.Concurrent
--import Control.Concurrent.STM
--import Control.Concurrent.STM.TBQueue

import qualified Data.ByteString.Char8 as B


--import Data.ZRE
import Network.ZGossip
import System.ZMQ4.Endpoint

gossipPort = 31337

main :: IO ()
main = do
  let gossipServerEndpoint = newTCPEndpoint "*" gossipPort
  zgossipServer gossipServerEndpoint
  --runZgossip chatApp
