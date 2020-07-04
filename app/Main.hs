{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Time.Calendar
import Data.Time.Clock
import Options.Applicative as OA
import System.Directory (createDirectoryIfMissing)
import System.ProgressBar
import qualified Data.Text.Lazy.IO as TL


import DownloadHackage
import HackageJson


data Opts = Opts
  { optBasePath    :: FilePath
  , optHackageJson :: FilePath
  , optDownloadLog :: Maybe FilePath
  } deriving (Show)


main :: IO ()
main = run =<< customExecParser p opts
  where
    opts = info (helper <*> optsParser)
      ( fullDesc
        <> header "mirror-hackage - \
                  \download all hackage packages (including revisions) \
                  \ready to be served with http server provided hackage.json"
      )
    p = defaultPrefs {prefShowHelpOnError = True}

run :: Opts -> IO ()
run opts = do
  createDirectoryIfMissing True (optBasePath opts)
  hackageJsonParsed <- parseHackageJson (optHackageJson opts)
  putStrLn
    $ show (optHackageJson opts) ++ " contains "
    ++ show (length hackageJsonParsed) ++ " "
    ++ "packages (each segregates into versions and metadata (cabal) revisions)"
  let downloadPlan = getDownloadPlan hackageJsonParsed
  putStrLn $ "download plan: " ++ show (length downloadPlan) ++ " files"
  putStrLn "downloading..."
  let t = UTCTime (ModifiedJulianDay 0) 0
      showProgr done =
        TL.putStr $ "\r" <> renderProgressBar
        defStyle (Progress done (length downloadPlan) ()) (Timing t t)
  runDownloadPlan showProgr (optBasePath opts) downloadPlan
  putStrLn "download complete!"

optsParser :: Parser Opts
optsParser = do
  optBasePath <- strOption
    $ long "base-path" <> metavar "BASE_PATH"
    <> value "hackage-mirror" <> showDefault
    <> help "Local base path for hackage mirror contents"
  optHackageJson <- strOption
    $ long "hackage-json" <> metavar "HACKAGE_JSON"
    <> value "hackage.json" <> showDefault
    <> help "hackage.json file from hackage.nix repository"
  optDownloadLog <- optional $ strOption
    $ long "dl-log" <> metavar "DL_LOG"
    <> help "Output log file"
  pure Opts {..}
