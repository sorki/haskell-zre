{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Concurrent.Async.Lifted

import qualified Data.ByteString.Char8 as B

import Data.ZRE
import Network.ZRE
import Network.ZRE.Types
import Network.ZRE.Parse
import Network.ZRE.Peer
import Network.ZRE.Utils

main :: IO ()
main = runZre chatApp

chatApp = do
      recv `concurrently` act
      where
        recv = forever $ do
          evt <- readZ
          case evt of
            New peer -> put ["New peer", printPeer peer]
            Ready peer -> put ["Ready peer", printPeer peer]
            Quit peer -> put ["Peer quit", printPeer peer]
            GroupJoin peer group -> put ["Join group", group, printPeer peer]
            GroupLeave peer group -> put ["Leave group", group, printPeer peer]
            Message m@ZREMsg{..} -> do
              liftIO $ B.putStrLn $ bshow m
              case msgCmd of
                (Shout group content) -> put ["shout for group", group, ">", B.concat content]
                (Whisper content) -> put ["whisper", B.concat content]

        act = forever $ do
          liftIO $ B.putStr " >"
          msg <- fmap B.pack $ liftIO getLine
          case parseAttoApi msg of
            (Left err) -> liftIO $ B.putStr $ B.pack err
            (Right cmd) -> writeZ cmd
          return ()

        put = liftIO . B.putStrLn . (B.intercalate " ")
        --call = writeZreQueue outQ
