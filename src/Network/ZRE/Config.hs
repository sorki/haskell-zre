{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.ZRE.Config where

import System.Environment
import System.Directory
import System.FilePath.Posix
import System.Exit (exitFailure)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Network.ZRE.Types
import System.ZMQ4.Endpoint

import Data.Ini.Config
import Data.Default

iniParser :: IniParser ZRECfg
iniParser = section "zre" $ do
  zreNamed        <- B.pack . T.unpack <$> fieldDef "name" (T.pack . B.unpack $ zreNamed def)
  zreInterfaces   <- fieldDefOf "interfaces" (return . map B.pack . words . T.unpack) []
  zreQuietPeriod  <- fieldDefOf "quiet-period"  (fmap isec . number) (zreQuietPeriod def)
  zreDeadPeriod   <- fieldDefOf "dead-period"   (fmap isec . number) (zreDeadPeriod def)
  zreBeaconPeriod <- fieldDefOf "beacon-period" (fmap isec . number) (zreBeaconPeriod def)
  zreZGossip      <- fieldDefOf "gossip" (fmap Just . parseAttoTCPEndpoint . B.pack . T.unpack) (zreZGossip def)
  zreMCast        <- fieldDefOf "multicast-group" (parseAttoTCPEndpoint . B.pack . T.unpack) (zreMCast def)
  zreDbg          <- fieldDefOf "debug" flag (zreDbg def)
  return $ ZRECfg {..}

parseZRECfg :: FilePath -> IO (Either String ZRECfg)
parseZRECfg fpath = do
    rs <- TIO.readFile fpath
    return $ parseIniFile rs iniParser

-- If ZRECFG env var is set, try parsing config file it is pointing to,
-- return default config otherwise.
--
-- if ZRENAME env var is set, it overrides name field in ZRECFG config
-- or default config respectively.
envZRECfg :: IO (ZRECfg)
envZRECfg = do
  menv <- lookupEnv "ZRECFG"
  mname <- lookupEnv "ZRENAME"
  case menv of
    Nothing -> do
      hom <- getHomeDirectory
      let homPth = hom </> ".zre.conf"
      tst <- doesFileExist homPth
      case tst of
        False -> return $ maybeUpdateName def mname
        True -> do
          res <- parseZRECfg homPth
          case res of
           Left err -> putStrLn ("Unable to parse config: " ++ err) >> exitFailure
           Right cfg -> return $ maybeUpdateName cfg mname
    Just env -> do
      res <- parseZRECfg env
      case res of
        Left err -> putStrLn ("Unable to parse config: " ++ err) >> exitFailure
        Right cfg -> return $ maybeUpdateName cfg mname
  where
    maybeUpdateName cfg mname = maybe cfg (\x -> cfg { zreNamed = B.pack x}) mname
