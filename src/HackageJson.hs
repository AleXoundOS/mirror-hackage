{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HackageJson
  ( HackageJson, RevisionData(..), Package(..), Revisions(..), Hash
  , PkgName, Version, Revision, OutPath
  , parseHackageJson
  )
  where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as T


type PkgName  = Text
type Version  = Text
type Revision = Text
type OutPath  = Text
type Hash = ByteString

type HackageJson = HashMap PkgName (HashMap Version Package)

data Package = Package
  { pkgRevs :: Revisions
  , pkgHash :: Hash
  } deriving (Eq, Show)

data Revisions = Revisions
  { revsDefault :: Revision
  , revsRevs    :: HashMap Revision RevisionData
  } deriving (Eq, Show)

data RevisionData = RevisionData
  { revDataOutPath   :: OutPath
  , revDataNum       :: Int
  , revDataCabalHash :: Hash
  } deriving (Eq, Show)


instance FromJSON Package where
  parseJSON = withObject "HackageJson" $ \o -> Package
    <$> (o .: "revisions")
    <*> (o .: "sha256" >>= parseSha256)

instance FromJSON Revisions where
  parseJSON = withObject "revisions" $ \o -> do
    revsDefault <- parseJSON $ o HM.! "default"
    revsRevs    <- mapM parseJSON $ HM.delete "default" o
    return $ Revisions{..}

instance FromJSON RevisionData where
  parseJSON = withObject "rn" $ \o -> RevisionData
    <$> (o .: "outPath")
    <*> (o .: "revNum")
    <*> (o .: "sha256" >>= parseSha256)


parseSha256 :: Value -> Parser Hash
parseSha256 =
  withText "sha256" $ \t -> return $ fst $ Base16.decode $ T.encodeUtf8 t

parseHackageJson :: FilePath -> IO HackageJson
parseHackageJson = return . either error id <=< eitherDecodeFileStrict'
