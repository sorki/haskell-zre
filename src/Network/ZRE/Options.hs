{-# LANGUAGE OverloadedStrings #-}

module Network.ZRE.Options (
    parseOptions
  ) where

import Options.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Network.ZRE.Types
import System.ZMQ4.Endpoint

parseOptions :: Parser ZRECfg
parseOptions = ZRECfg
  <$> (B.pack <$> strOption
        (long "name"
      <> short 'n'
      <> value ""
      <> help "Node name"))
  <*> (option auto
        (long "quiet-period"
      <> short 'q'
      <> metavar "N"
      <> value 1.0
      <> help "Ping peer after N seconds"))
  <*> (option auto
        (long "quiet-ping-rate"
      <> short 'p'
      <> metavar "N"
      <> value 1.0
      <> help "Peer ping rate after quiet period passed"))
  <*> (option auto
        (long "dead-period"
      <> short 'd'
      <> metavar "N"
      <> value 5.0
      <> help "Mark peer dead after N seconds"))
  <*> (option auto
         (long "beacon-period"
      <> short 'b'
      <> metavar "N"
      <> value 0.9
      <> help "Send beacon every N seconds"))
  <*> ((map B.pack) <$> many (strOption
        (long "interface"
      <> short 'i'
      <> metavar "IFACE"
      <> help "Interfaces")))
  <*> option (attoReadM parseAttoUDPEndpoint)
        (long "multicast-group"
      <> short 'm'
      <> metavar "IP:PORT"
      <> value defMCastEndpoint
      <> help "IP:PORT of the multicast group")
  <*> optional (option (attoReadM parseAttoTCPEndpoint)
        (long "gossip"
      <> short 'g'
      <> metavar "IP:PORT"
      <> help "IP:PORT of the gossip server"))
  <*> switch (long "debug" <> short 'd')

attoReadM :: (ByteString -> Either String a) -> ReadM a
attoReadM p = eitherReader (p . B.pack)
