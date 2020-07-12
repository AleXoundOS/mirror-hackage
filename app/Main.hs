{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Monoid ((<>))
import Data.Time.Clock
import Options.Applicative as OA
import System.Directory (createDirectoryIfMissing)
import System.IO (hFlush, stdout)
import System.ProgressBar
import qualified Data.HashMap.Strict as HM (delete)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified System.Console.Terminal.Size as TS (size, width)


import DownloadHackage
import HackageJson


data Opts = Opts
  { optBasePath    :: FilePath
  , optHackageJson :: FilePath
  , optExcludeFp   :: FilePath
  , optDownloadLog :: Maybe FilePath
  } deriving (Show)


main :: IO ()
main = do
  termWidth <- getTermWidth
  run termWidth =<< customExecParser (p termWidth) opts
  where
    opts = info (helper <*> optsParser)
      ( fullDesc
        <> header "mirror-hackage - \
                  \download all hackage packages (including revisions) \
                  \ready to be served with http server provided hackage.json"
      )
    p termWidth =
      defaultPrefs {prefShowHelpOnError = True, prefColumns = termWidth}

getTermWidth :: IO Int
getTermWidth = do
  mbWindow <- TS.size
  return $ case mbWindow of
             Nothing -> 80
             Just window -> TS.width window

run :: Int -> Opts -> IO ()
run termWidth opts = do
  createDirectoryIfMissing True (optBasePath opts)
  excludePackagesList <- T.lines <$> T.readFile (optExcludeFp opts)
  hackageJsonParsed <- (\hm -> foldr HM.delete hm excludePackagesList)
                       <$> parseHackageJson (optHackageJson opts)
  putStrLn
    $ show (optHackageJson opts) ++ " contains "
    ++ show (length hackageJsonParsed) ++ " "
    ++ "packages (each segregates into versions and metadata/cabal revisions)"
  let downloadPlan = getDownloadPlan hackageJsonParsed
  putStrLn "downloading..."
  tStart <- getCurrentTime
  let showProgr done curFp = do
        timing <- Timing tStart <$> getCurrentTime
        let pStyle =
              defStyle { stylePrefix =
                           exact <> msg " files (rem "
                           <> remainingTime renderTime "--:--" <> ")"
                       , styleWidth = ConstantWidth 80
                       }
            renderTime diffTime = renderDuration diffTime <> "s"
            progress = Progress done (length downloadPlan) ()
            eraseTxt = "\r" <> TL.justifyRight (fromIntegral termWidth) ' ' "\r"
        TL.putStr
          $ eraseTxt <> renderProgressBar pStyle progress timing
          <> " " <> TL.pack curFp <> " "
        hFlush stdout
  runDownloadPlan showProgr (optBasePath opts) downloadPlan
  putStrLn "\ndownload is completed!"

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
  optExcludeFp <- strOption
    $ long "exclude-from" <> metavar "FILE"
    <> value "exclude-packages.txt" <> showDefault
    <> help "read hackage packages exclude list from file \
            \(use /dev/null if none needed)"
  optDownloadLog <- optional $ strOption
    $ long "dl-log" <> metavar "DL_LOG"
    <> help "Output log file (NOT IMPLEMENTED)"
  pure Opts {..}
