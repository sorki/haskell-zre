{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.ZGossip
import System.ZMQ4.Endpoint

gossipPort = 31337

main :: IO ()
main = do
  let gossipServerEndpoint = newTCPEndpoint "*" gossipPort
  zgossipServer gossipServerEndpoint
