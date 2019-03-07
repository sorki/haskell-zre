{-# LANGUAGE OverloadedStrings #-}

module Network.ZRE.Options (
    parseOptions
  ) where

import Options.Applicative
import Data.Semigroup ((<>))
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
  <*> (isec <$> option auto
        (long "quiet-period"
      <> short 'q'
      <> metavar "N"
      <> value (sec (1.0 :: Float))
      <> help "Ping peer after N seconds"))
  <*> ((*100000) <$> option auto
        (long "dead-period"
      <> short 'd'
      <> metavar "N"
      <> value (sec (1.0 :: Float))
      <> help "Mark peer dead after N seconds"))
  <*> ((*100000) <$> option auto
         (long "beacon-period"
      <> short 'b'
      <> metavar "N"
      <> value (sec (0.9 :: Float))
      <> help "Send beacon every N seconds"))
  <*> ((map B.pack) <$> many (strOption
        (long "iface"
      <> short 'i'
      <> metavar "IFACE"
      <> help "Interfaces")))
  <*> option (attoReadM parseAttoUDPEndpoint)
        (long "mcast"
      <> short 'm'
      <> metavar "IP:PORT"
      <> value defMCastEndpoint
      <> help "IP:PORT of the multicast group")
  <*> optional (option (attoReadM parseAttoTCPEndpoint)
        (long "gossip"
      <> short 'g'
      <> metavar "IP:PORT"
      <> help "IP:PORT of the gossip server"))
  <*> (flag' False (long "debug" <> short 'd'))

attoReadM :: (B.ByteString -> Either String a) -> ReadM a
attoReadM p = eitherReader (p . B.pack)
