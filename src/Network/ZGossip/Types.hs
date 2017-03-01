module Network.ZGossip.Types where

import System.IO (Handle)
import qualified Data.Map as M
import qualified Data.Set as S

import Data.ZGossip

data ZGossipState = ZGossipState {
    gossipPeers   :: S.Set Peer
  , gossipPairs   :: M.Map Key (Value, TTL)
} deriving (Show)

emptyGossipState = ZGossipState S.empty M.empty
