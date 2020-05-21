module Network.ZRE.Lib where

import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)

import qualified Data.ByteString.Char8

import Data.ZRE (Group)
import Network.ZRE.Types

zrecvWithShout :: Group
               -> (ByteString -> ZRE ())
               -> ZRE ()
zrecvWithShout group f = do
  e <- zrecv
  case e of
    Shout _ forGroup content _time | forGroup == group ->
      f (Data.ByteString.Char8.concat content)

    _ | otherwise -> return ()

zrecvShouts :: Group
            -> (ByteString -> ZRE ())
            -> ZRE ()
zrecvShouts group fn = forever $ zrecvWithShout group fn

zrecvShoutsDecode :: Group
                  -> (ByteString -> Either String decoded)
                  -> (Either String decoded -> ZRE ())
                  -> ZRE ()
zrecvShoutsDecode group decFn handler = zrecvShouts group $ handler . decFn

decodeShouts :: (Monad m, Alternative m)
             => (Event -> Either String decoded)
             -> (Either String decoded -> ZRE ())
             -> Event
             -> m (ZRE ())
decodeShouts fn action msg = do
  guard $ isShout msg
  return $ readZ >> (action . fn $ msg)

isShout :: Event -> Bool
isShout (Shout _uuid _group _content _time) = True
isShout _ = False

isGroupMsg :: Group -> Event -> Bool
isGroupMsg group (Shout _uuid g _content _time) = g == group
isGroupMsg _ _ = False

(==>) :: (Monad m, Alternative m) => (t -> Bool) -> b -> t -> m b
(==>) f act = iff f act

iff :: (Monad m, Alternative m) => (t -> Bool) -> b -> t -> m b
iff f act msg = do
  guard $ f msg
  return $ act

match :: [Event -> Maybe (ZRE ())] -> ZRE ()
match acts = do
  msg <- readZ
  go acts msg
  where
    go (act:rest) m = do
      case act m of
        Nothing -> go rest m
        Just a -> unReadZ m >> a

    go [] _ = return ()
