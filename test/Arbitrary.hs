module Arbitrary where

import Test.QuickCheck
import Test.QuickCheck.Instances

import Data.ByteString
import qualified Data.ByteString.Char8 as B

import Data.ZRE
import qualified Data.ZGossip as ZG
import System.ZMQ4.Endpoint

instance Arbitrary ZG.ZGSCmd where
  arbitrary = oneof [
      return ZG.Hello
    , ZG.Publish <$> arbitrary <*> arbitrary <*> (abs <$> arbitrary)
    , return ZG.Ping
    , return ZG.PingOk
    , return ZG.Invalid
    ]

instance Arbitrary ZG.ZGSMsg where
  arbitrary = ZG.ZGSMsg <$> (Just <$> arbitrary) <*> arbitrary

instance Arbitrary Transport where
  arbitrary = oneof [
      pure TCP
    , pure UDP
    , pure IPC
    , pure InProc
    , pure PGM
    , pure EPGM
    ]

instance Arbitrary Endpoint where
  arbitrary = Endpoint <$> arbitrary
                       <*> (arbitrary `suchThat` (not . B.elem ':'))
                       <*> (fmap abs <$> arbitrary)


instance Arbitrary ZRECmd where
  arbitrary = oneof [
      Hello <$> arbitrary
            <*> arbitrary
            <*> (abs <$> arbitrary)
            <*> arbitrary
            <*> arbitrary

    , Whisper <$> arbitrary
    , Shout   <$> arbitrary <*> arbitrary

    , Join  <$> arbitrary
            <*> (abs <$> arbitrary)
    , Leave <$> arbitrary
            <*> (abs <$> arbitrary)

    , pure Ping
    , pure PingOk
    ]

instance Arbitrary ZREMsg where
  arbitrary = ZREMsg <$> (Just <$> arbitrary)
                     <*> (abs <$> arbitrary)
                     <*> pure Nothing
                     <*> arbitrary

roundTrip :: Eq a
          => (a -> b)
          -> (b -> Either String a)
          -> a
          -> Bool
roundTrip putter getter x =
    Right x == (getter . putter $ x)

