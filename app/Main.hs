{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Main where

import Control.Monad (forever, void)
import Control.Monad.IO.Class
import Control.Concurrent.Async.Lifted

import qualified Data.ByteString.Char8 as B

import Network.ZRE
import Network.ZRE.Parse
import Network.ZRE.Types

import Control.Concurrent.STM
import System.Console.Repline
import Data.List (isPrefixOf)

type Repl a = HaskelineT IO a

completer :: Monad m => WordCompleter m
completer n = do
  let names = [ "join"
              , "leave"
              , "shout"
              , "whisper"
              , "debug"
              , "nodebug"
              , "quit" ]

  return $ filter (isPrefixOf n) (map ('/':) names)

main :: IO ()
main = runZre replApp

replApp :: ZRE ()
replApp = void $ do
  recv `concurrently` repl
  where
    recv = forever $ do
      evt <- readZ
      case evt of
        New uuid mname groups headers endpoint ->
          put ["New peer", toASCIIBytes uuid, pEndpoint endpoint]
        Ready uuid name groups headers endpoint ->
          put ["Ready peer", name, toASCIIBytes uuid]
        Quit uuid mname ->
          put ["Peer quit", toASCIIBytes uuid]
        GroupJoin uuid group ->
          put ["Join group", unGroup group, toASCIIBytes uuid]
        GroupLeave uuid group ->
          put ["Leave group", unGroup group, toASCIIBytes uuid]
        Shout _uuid group content _time ->
          put ["Shout for group", unGroup group, ">", B.concat content]
        Whisper uuid content _time ->
          put ["Whisper from", toASCIIBytes uuid, B.concat content]
        x -> liftIO $ print x

    repl = do
      q <- getApiQueue
      liftIO $ evalRepl (const $ pure ">>> ") (cmd q) [] Nothing Nothing (Word completer) ini end
      liftIO $ atomically $ writeTBQueue q DoQuit

    ini = liftIO $ putStrLn "Welcome!"
    end = liftIO $ putStrLn "Exiting" >> return Exit

    cmd :: APIQueue -> String -> Repl ()
    cmd q x = do
      case parseAttoApi $ B.pack x of
        (Left err) -> liftIO $ B.putStrLn $ B.pack $ "Unable to parse command: " ++ err
        (Right cmd) -> liftIO $ atomically $ writeTBQueue q cmd
      return ()

    put = liftIO . B.putStrLn . (B.intercalate " ")
