{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Options.Applicative as OA
import System.Directory (createDirectoryIfMissing)

import Text.Pretty.Simple
import qualified Data.HashMap.Strict as HM


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
  pPrint $ take 10 $ getDownloadPlan hackageJsonParsed
  -- downloadHackage (optHackageJson opts)

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
