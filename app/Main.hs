{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative
import Control.Monad (forever)
--import Control.Concurrent
--import Control.Concurrent.STM
--import Control.Concurrent.STM.TBQueue

import qualified Data.ByteString.Char8 as B


import Data.ZRE
import Network.ZRE
import Network.ZRE.Parse
import Network.ZRE.Peer
import Network.ZRE.Types
import Network.ZRE.Utils

main :: IO ()
main = runZre chatApp

chatApp inQ outQ = do
      concurrentZre recv act
      where
        recv = forever $ do
          evt <- readZreQueue inQ
          case evt of
            New peer -> do
              B.putStrLn $ B.intercalate " " ["New peer", printPeer peer]
            Ready peer -> B.putStrLn $ B.intercalate " " ["Ready peer", printPeer peer]
            Quit peer -> B.putStrLn $ B.intercalate " " ["Peer quit", printPeer peer]
            GroupJoin peer group -> B.putStrLn $ B.intercalate " " ["Join group", group, printPeer peer]
            GroupLeave peer group -> B.putStrLn $ B.intercalate " " ["Leave group", group, printPeer peer]
            Message m@ZREMsg{..} -> do
              B.putStrLn $ bshow m
              case msgCmd of
                (Shout group content) -> B.putStrLn $ B.intercalate " " ["shout for group", group, ">", B.concat content]
                (Whisper content) -> B.putStrLn $ B.intercalate " " ["whisper", B.concat content]

        act = forever $ do
          B.putStr " >"
          msg <- fmap B.pack getLine
          case parseAttoApi msg of
            (Left err) -> print err
            (Right cmd) -> call cmd
          return ()

        call = writeZreQueue outQ
