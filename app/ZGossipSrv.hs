{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.ZGossip
import System.ZMQ4.Endpoint

gossipPort :: Int
gossipPort = 31337

main :: IO ()
main = do
  let gossipServerEndpoint = newTCPEndpoint "*" gossipPort
  zgossipServer gossipServerEndpoint
