{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Async.Lifted

import qualified Data.ByteString.Char8 as B

import System.IO
import System.Environment

import Network.ZRE
import Options.Applicative

data CatOpts = CatOpts {
    shutdown     :: Bool  -- ^ Close connection after EOF on the input
  , lineBuffered :: Bool
  , bufferSize   :: Int
  , catGroup     :: Group
  } deriving (Eq, Show, Ord)

parseCatOptions = CatOpts
  <$> switch (long "shutdown" <> short 'N')
  <*> switch (long "bufbyline" <> short 'l')
  <*> option auto (long "bufsize" <> short 'O' <> value (1024*128))
  <*> (mkGroup <$> strArgument (metavar "GROUP"))

main :: IO ()
main = do
  runZreParse parseCatOptions $ groupCat

groupCat :: CatOpts -> ZRE ()
groupCat CatOpts{..} = do
  zjoin catGroup
  void $ async $ catln
  -- wait a sec so join is received by peers before sending stuff
  void $ async $ (liftIO $ threadDelay 1000000) >> (stdin' catGroup)
  cat

catln :: ZRE ()
catln = forever $ do
  e <- readZ
  case e of
    Shout _uuid _group content _time  -> liftIO $ B.putStrLn $ B.concat content
    Whisper _uuid content _time -> liftIO $ B.putStrLn $ B.concat content
    _ -> return ()

cat :: ZRE ()
cat = forever $ do
  e <- readZ
  case e of
    Shout _uuid _group content _time  -> liftIO $ B.putStr $ B.concat content
    Whisper _uuid content _time -> liftIO $ B.putStr $ B.concat content
    _ -> return ()

stdinln :: Group -> ZRE ()
stdinln group = forever $ do
  l <- fmap B.pack $ liftIO getLine
  zshout group l

bufsize :: Int
bufsize = 1024*128

stdin' :: Group -> ZRE ()
stdin' group = do
  liftIO $ hSetBuffering stdin $ BlockBuffering (Just bufsize)
  forever $ do
    eof <- liftIO $ hIsEOF stdin
    case eof of
      True -> zquit
      False -> do
        buf <- liftIO $ B.hGetSome stdin bufsize
        zshout group buf
