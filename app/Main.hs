{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Concurrent.Async.Lifted

import qualified Data.ByteString.Char8 as B

import Network.ZRE
import Network.ZRE.Parse

main :: IO ()
main = runZre chatApp

chatApp = do
      recv `concurrently` act
      where
        recv = forever $ do
          evt <- readZ
          case evt of
            New uuid mname groups headers endpoint -> put ["New peer", toASCIIBytes uuid, pEndpoint endpoint]
            Ready uuid name groups headers endpoint -> put ["Ready peer", name, toASCIIBytes uuid]
            Quit uuid mname -> put ["Peer quit", toASCIIBytes uuid]
            GroupJoin uuid group -> put ["Join group", group, toASCIIBytes uuid]
            GroupLeave uuid group -> put ["Leave group", group, toASCIIBytes uuid]
            Shout _uuid group content _time -> put ["Shout for group", group, ">", B.concat content]
            Whisper uuid content _time -> put ["Whisper from", toASCIIBytes uuid, B.concat content]
            x -> liftIO $ print x
            _ -> return ()

        act = forever $ do
          liftIO $ B.putStr " >"
          msg <- fmap B.pack $ liftIO getLine
          case parseAttoApi msg of
            (Left err) -> liftIO $ B.putStr $ B.pack err
            (Right cmd) -> writeZ cmd
          return ()

        put = liftIO . B.putStrLn . (B.intercalate " ")
