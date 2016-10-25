{-# LANGUAGE OverloadedStrings #-}
module System.ZMQ4.Endpoint where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import Data.Char (toLower)

import Network.Socket  -- only need addrAddress
import Network.SockAddr (showSockAddrBS)

type Port = Int
type Address = B.ByteString
data Transport = TCP | IPC | InProc | PGM | EPGM
  deriving (Show, Eq, Ord)

data Endpoint = Endpoint Transport Address (Maybe Port)
  deriving (Show, Eq, Ord)

pTransport x = B.pack $ map toLower $ show x
pEndpoint (Endpoint t a Nothing) = B.concat [pTransport t, "://" , a]
pEndpoint (Endpoint t a (Just p)) = B.concat [pTransport t, "://" , a, ":", B.pack $ show p]

newEndpoint transport addr = newEndpointPort' transport addr Nothing
newEndpointAddrInfo transport addr = newEndpointPortAddrInfo' transport addr Nothing

newEndpointPort' transport addr port = Endpoint transport addr port
newEndpointPortAddrInfo' transport addr port = newEndpointPort' transport (showSockAddrBS $ addrAddress addr) port

newEndpointPort transport addr port = newEndpointPort' transport addr (Just port)
newEndpointPortAddrInfo transport addr port = newEndpointPortAddrInfo' transport addr (Just port)

newTCPEndpoint addr port = newEndpointPort TCP addr port
newTCPEndpointAddrInfo addr port = newEndpointPortAddrInfo TCP addr port

parseTransport :: Parser Transport
parseTransport = do
  t <- A.takeWhile (/=':')
  string "://"
  r <- case t of
    "tcp" -> pure TCP
    "ipc" -> pure IPC
    "inproc" -> pure InProc
    "pgm" -> pure PGM
    "epgm" -> pure EPGM
    _ -> fail $ "Unknown transport" ++ (B.unpack t)

  return r

parseAddress :: Parser Address
parseAddress = A.takeWhile(/=':')

parsePort :: Parser Port
parsePort = do
  char ':'
  d <- decimal
  return d

parseEndpoint :: Parser Endpoint
parseEndpoint = Endpoint <$> parseTransport <*> parseAddress <*> optional parsePort

parseAttoEndpoint = A.parseOnly parseEndpoint
