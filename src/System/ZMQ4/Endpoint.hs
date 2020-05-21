{-# LANGUAGE OverloadedStrings #-}
module System.ZMQ4.Endpoint (
    parseAttoEndpoint
  , parseAttoTCPEndpoint
  , parseAttoUDPEndpoint
  , pTransport
  , pEndpoint
  , endpointAddr
  , endpointPort
  , endpointTransport
  , newEndpoint
  , newEndpointPort
  , newEndpointAddrInfo
  , newEndpointPortAddrInfo
  , newTCPEndpoint
  , newTCPEndpointAddrInfo
  , newUDPEndpoint
  , toAddrInfo
  , Port
  , Address
  , Transport(..)
  , Endpoint(..)
  ) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString (ByteString)
import Network.Socket (AddrInfo)

import qualified Data.ByteString.Char8 as B
import qualified Data.Char

import qualified Network.Socket
import qualified Network.SockAddr

type Port = Int
type Address = ByteString
data Transport = TCP | UDP | IPC | InProc | PGM | EPGM
  deriving (Show, Eq, Ord)

data Endpoint = Endpoint Transport Address (Maybe Port)
  deriving (Show, Eq, Ord)

pTransport :: Show a => a -> ByteString
pTransport x = B.pack $ map Data.Char.toLower $ show x

pEndpoint :: Endpoint -> ByteString
pEndpoint (Endpoint t a Nothing) = B.concat [pTransport t, "://" , a]
pEndpoint (Endpoint t a (Just p)) = B.concat [pTransport t, "://" , a, ":", B.pack $ show p]

newEndpoint :: Transport -> Address -> Endpoint
newEndpoint transport addr = newEndpointPort' transport addr Nothing

newEndpointAddrInfo :: Transport -> AddrInfo -> Endpoint
newEndpointAddrInfo transport addr = newEndpointPortAddrInfo' transport addr Nothing

newEndpointPort' :: Transport -> Address -> Maybe Port -> Endpoint
newEndpointPort' transport addr port = Endpoint transport addr port

newEndpointPortAddrInfo' :: Transport -> AddrInfo -> Maybe Port -> Endpoint
newEndpointPortAddrInfo' transport addr port =
  newEndpointPort'
    transport
    (Network.SockAddr.showSockAddrBS $ Network.Socket.addrAddress addr)
    port

newEndpointPort :: Transport -> Address -> Port -> Endpoint
newEndpointPort transport addr port = newEndpointPort' transport addr (Just port)

newEndpointPortAddrInfo :: Transport -> AddrInfo -> Port -> Endpoint
newEndpointPortAddrInfo transport addr port =
  newEndpointPortAddrInfo'
    transport addr (Just port)

newTCPEndpoint :: Address -> Port -> Endpoint
newTCPEndpoint addr port = newEndpointPort TCP addr port

newUDPEndpoint :: Address -> Port -> Endpoint
newUDPEndpoint addr port = newEndpointPort UDP addr port

newTCPEndpointAddrInfo :: AddrInfo -> Port -> Endpoint
newTCPEndpointAddrInfo addr port = newEndpointPortAddrInfo TCP addr port

toAddrInfo :: Endpoint -> IO [AddrInfo]
toAddrInfo (Endpoint _ a (Just p)) = Network.Socket.getAddrInfo Nothing (Just $ B.unpack a) (Just $ show p)
toAddrInfo (Endpoint _ a _) = Network.Socket.getAddrInfo Nothing (Just $ B.unpack a) Nothing

parseTransport :: Parser Transport
parseTransport = do
  t <- A.takeWhile (/=':')
  _ <- string "://"
  r <- case t of
    "tcp"    -> pure TCP
    "udp"    -> pure UDP
    "ipc"    -> pure IPC
    "inproc" -> pure InProc
    "pgm"    -> pure PGM
    "epgm"   -> pure EPGM
    _ -> fail $ "Unknown transport " ++ (B.unpack t)

  return r

parseAddress :: Parser Address
parseAddress = A.takeWhile(/=':')

parsePort :: Parser Port
parsePort = do
  _ <- char ':'
  d <- decimal
  return d

parseEndpoint :: Parser Endpoint
parseEndpoint = Endpoint <$> parseTransport <*> parseAddress <*> optional parsePort

parseTCPEndpoint :: Parser Endpoint
parseTCPEndpoint = Endpoint <$> pure TCP <*> parseAddress <*> optional parsePort

parseUDPEndpoint :: Parser Endpoint
parseUDPEndpoint = Endpoint <$> pure UDP <*> parseAddress <*> optional parsePort

parseAttoEndpoint :: ByteString -> Either String Endpoint
parseAttoEndpoint = A.parseOnly parseEndpoint

parseAttoTCPEndpoint :: ByteString -> Either String Endpoint
parseAttoTCPEndpoint = A.parseOnly parseTCPEndpoint

parseAttoUDPEndpoint :: ByteString -> Either String Endpoint
parseAttoUDPEndpoint = A.parseOnly parseUDPEndpoint

endpointAddr :: Endpoint -> Address
endpointAddr (Endpoint _ a _) = a

endpointPort :: Endpoint -> Maybe Port
endpointPort (Endpoint _ _ p) = p

endpointTransport :: Endpoint -> Transport
endpointTransport (Endpoint t _ _) = t
