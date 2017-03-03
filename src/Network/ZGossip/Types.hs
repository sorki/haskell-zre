module Network.ZGossip.Types (
    ZGossipState(..)
  , emptyGossipState
  , cvtPub) where

import qualified Data.Map as M
import qualified Data.Set as S

import Data.ZGossip

data ZGossipState = ZGossipState {
    gossipPeers   :: S.Set Peer
  , gossipPairs   :: M.Map Key (Value, TTL)
} deriving (Show)

emptyGossipState :: ZGossipState
emptyGossipState = ZGossipState S.empty M.empty

cvtPub :: (Key, (Value, TTL)) -> ZGSCmd
cvtPub (k, (v, ttl)) = Publish k v ttl
