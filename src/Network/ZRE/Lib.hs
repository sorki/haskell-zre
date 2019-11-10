module Network.ZRE.Lib where

import Control.Applicative
import Control.Monad

import Data.Either
import qualified Data.ByteString.Char8 as B

import Data.ZRE (Group)
import Network.ZRE.Types

zrecvWithShout:: (B.ByteString -> ZRE ()) -> ZRE ()
zrecvWithShout f = do
  e <- zrecv
  case e of
    Shout _ _ content _time -> f (B.concat content)
    _ -> return ()

zrecvShouts :: (B.ByteString -> ZRE ()) -> ZRE b
zrecvShouts fn = forever $ zrecvWithShout fn

whenDecodes :: Monad m
            => (msg -> Either a decoded)
            -> (decoded -> m ())
            -> msg
            -> m ()
whenDecodes decoder action content = case decoder content of
  Left _ -> return ()
  Right x -> action x

decodeShouts :: (Monad m, Alternative m)
             => (Event -> Either a decoded)
             -> (decoded -> ZRE ())
             -> Event
             -> m (ZRE ())
decodeShouts fn action msg = do
  guard $ isShout msg
  let res = fn msg
  guard $ isRight res
  case res of
    Left _ -> return $ pure ()
    Right x -> return $ readZ >> action x

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
