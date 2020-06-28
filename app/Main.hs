module Main (main) where

import Options.Applicative as OA
import System.Directory (createDirectoryIfMissing)


data Opts = Opts
  { optBasePath      :: FilePath
  , optHackageJsonFp :: FilePath
  } deriving (Show)


main :: IO ()
main = run =<< customExecParser p opts
  where
    opts = info (helper <*> optsParser)
      ( fullDesc
        <> header "mirror-hackage - \
                  \download all hackages packages archives and cabal revisions"
      )
    p = defaultPrefs {prefShowHelpOnError = True}

run :: Opts -> IO ()
run opts = do
  createDirectoryIfMissing True (optBasePath opts)

optsParser :: Parser Opts
optsParser = Opts
  <$> strOption
  (long "base-path" <> metavar "BASE_PATH"
  <> value "hackage-mirror" <> showDefault
  <> help "Local base path for hackage contents")
  <*> strOption
  (long "hackage-json" <> metavar "HACKAGE_JSON"
  <> value "hackage.json" <> showDefault
  <> help "Path to hackage.json file from hackage.nix repository")
