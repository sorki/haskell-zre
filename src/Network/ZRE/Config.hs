{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.ZRE.Config where

import System.Environment
import System.Directory
import System.FilePath.Posix
import qualified Data.ByteString.Char8 as B

import Network.ZRE.Types

import Data.Default (def)
import qualified Data.Either
import qualified Data.Foldable

import Options.Applicative
import Network.ZRE.Options

import qualified Data.Text
import qualified Data.Attoparsec.Text

trueStr :: Data.Attoparsec.Text.Parser Bool
trueStr = pure True <$> (
    Data.Foldable.asum
  . map Data.Attoparsec.Text.string $ [ "true", "t", "yes", "y" ]
  )

falseStr :: Data.Attoparsec.Text.Parser Bool
falseStr =  pure False <$> (
    Data.Foldable.asum
  . map Data.Attoparsec.Text.string $ [ "false" , "f" , "no" , "n" ]
  )

iniFileToArgs :: [String] -> String -> [String]
iniFileToArgs sections file =
    concatMap (\(k, v) -> ["--" ++ k] ++ (if v /= "" then [v] else []))
  . map (Data.Text.unpack <$>)
  . map (\(k, v) -> if Data.Either.isRight $ Data.Attoparsec.Text.parseOnly (trueStr) v then (k, "") else (k, v)) -- fix --flag true -> --flag
  . filter (\(_, v) -> case Data.Attoparsec.Text.parseOnly (trueStr <|> falseStr) v of
      Left _e -> True
      Right b -> b)
  . map (Data.Text.pack <$>)
  . map (\x ->
          let t = takeWhile (flip elem $ '-':['a'..'z'])
          in (t x, dropWhile (== ' ') $ drop 1 $ dropWhile (/= '=') x)
        )
  . concatMap (\(_section, fields) -> fields)
  . filter (\(section, _fields) -> section `elem` sections)
  . groupBySections
  . filter (\(x:_xs) -> x /= '#') -- comments
  . filter (/="") -- empty
  $ lines file

-- transform [ "[zre]", "debug = false" "gossip=localhost:31337" "[zrecat]" "bufsize = 300"
-- to
-- [("zre", ["debug=false", "gossip=localhost:31337"]), ("zrecat", ["bufsize=300"])]
groupBySections :: [String] -> [(String, [String])]
groupBySections lines' = go lines'
  where
    go [] = []
    go ((x:xs):ls) | x == '[' = (takeWhile (flip elem $ '-':['a'..'z']) xs, keyVals ls):go ls
    go (_l:ls)     | otherwise = go ls
    keyVals [] = []
    keyVals ls = takeWhile (\(x:_) -> '[' /= x) ls

-- | Override config value from new iff it differs to default value
--
-- This could be done with `gzipWithT` and Generics
overrideNonDefault :: ZRECfg -> ZRECfg -> ZRECfg
overrideNonDefault orig new = ZRECfg {
    zreNamed         = ovr (zreNamed orig)         (zreNamed new)         (zreNamed def)
  , zreQuietPeriod   = ovr (zreQuietPeriod orig)   (zreQuietPeriod new)   (zreQuietPeriod def)
  , zreQuietPingRate = ovr (zreQuietPingRate orig) (zreQuietPingRate new) (zreQuietPingRate def)
  , zreDeadPeriod    = ovr (zreDeadPeriod orig)    (zreDeadPeriod new)    (zreDeadPeriod def)
  , zreBeaconPeriod  = ovr (zreBeaconPeriod orig)  (zreBeaconPeriod new)  (zreBeaconPeriod def)
  , zreInterfaces    = ovr (zreInterfaces orig)    (zreInterfaces new)    (zreInterfaces def)
  , zreMCast         = ovr (zreMCast orig)         (zreMCast new)         (zreMCast def)
  , zreZGossip       = ovr (zreZGossip orig)       (zreZGossip new)       (zreZGossip def)
  , zreDbg           = ovr (zreDbg orig)           (zreDbg new)           (zreDbg def)
  }
  where
    ovr :: (Eq a) => a -> a -> a -> a
    ovr _o n d | n /= d = n
    ovr o _n _d | otherwise = o

parseZRECfg :: String -> FilePath -> IO (Either String ZRECfg)
parseZRECfg exeName fpath = do
  isFile <- doesFileExist fpath
  case isFile of
    False -> pure $ Left "No such file"
    True -> do
      f <- readFile fpath
      let cfg = execParserPure defaultPrefs opts (iniFileToArgs ["zre", exeName] f)
      case cfg of
        -- we always fail when one of the configs fails to parse
        Failure e -> error $ fst $ renderFailure e ""
        Success cfg' -> return $ Right $ cfg'
        CompletionInvoked _ -> error "No completion"
  where
    opts = info (parseOptions <**> helper)
      ( fullDesc
     <> progDesc "ZRE"
     <> header "zre tools" )

-- The order is
-- * path of @ZRECFG@ env iff set
-- * @/etc/zre.conf@
-- * @~/.zre.conf@
-- * @default@
--
-- This also tries to parse subsection for zre programs according to their
-- name and construct correct command line for these so we can do
--
-- @[zrecat]@
-- @bufsize = 1024@
--
-- If @ZRENAME@ env var is set, it overrides name field in the result config.
envZRECfg :: String -> IO (ZRECfg)
envZRECfg exeName = do
  menv  <- lookupEnv "ZRECFG"
  mname <- lookupEnv "ZRENAME"

  hom <- getHomeDirectory

  cfg <- asumOneConfig [
      maybe (pure $ Left "No ZRECFG env") (parseZRECfg exeName) menv
    , parseZRECfg exeName "/etc/zre.conf"
    , parseZRECfg exeName $ hom </> ".zre.conf"
    , return $ Right def
    ]
  return $ maybeUpdateName cfg mname
  where
    maybeUpdateName cfg mname = maybe cfg (\x -> cfg { zreNamed = B.pack x}) mname
    asumOneConfig [] = error "Can't happen"
    asumOneConfig (x:xs) = x >>= \y -> case y of
      Left _e -> asumOneConfig xs
      Right cfg -> return $ cfg
