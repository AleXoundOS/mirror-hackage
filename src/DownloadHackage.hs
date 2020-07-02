{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module DownloadHackage
  ( downloadHackage, getDownloadPlan
  ) where

import Data.HashMap.Strict (HashMap)
import Network.HTTP.Req
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import DownloadFile
import HackageJson


type DownloadPlan = [Download]
data Download = Download
  { dlUrl  :: Url 'Https
  , dlHash :: Maybe Hash
  , dlFp   :: FilePath
  } deriving (Show)


baseUrl :: Url 'Https
baseUrl = https "hackage.haskell.org" /: "package"


downloadHackage :: FilePath -> DownloadPlan -> IO ()
downloadHackage hackageJsonFp dlPlan = putStrLn "downloadHackage"

getDownloadPlan :: HackageJson -> DownloadPlan
getDownloadPlan = concatMap mkFullDlOfPkg . HM.toList

mkFullDlOfPkg :: (PkgName, HashMap Version Package) -> DownloadPlan
mkFullDlOfPkg (pkgName, pkgsByVer) =
  concatMap (mkDlOfPkg pkgName) $ HM.toList pkgsByVer

mkDlOfPkg :: PkgName -> (Version, Package) -> DownloadPlan
mkDlOfPkg pkgName (ver, Package (Revisions _ revs) hash) =
  let pkgHyphenVer = pkgName <> "-" <> ver
      pkgBaseUrl = baseUrl /: pkgHyphenVer
      pkgArchiveUrl = pkgBaseUrl /: (pkgHyphenVer <> ".tar.gz")
      dlOfArchive = Download
        { dlUrl = pkgArchiveUrl
        , dlHash = Just hash
        , dlFp = T.unpack $ pkgHyphenVer <> pkgHyphenVer <> ".tar.gz"
        }
      mkDlOfCabal :: RevisionData -> Download
      mkDlOfCabal (RevisionData _ revNum cabalHash) = Download
        { dlUrl = pkgBaseUrl /: "/revision/" /: T.pack (show revNum) <> ".cabal"
        , dlHash = Just cabalHash
        , dlFp =
            T.unpack
            $ pkgHyphenVer <> "/revision/" <> T.pack (show revNum) <> ".cabal"
        }
  in dlOfArchive : map mkDlOfCabal (HM.elems revs)
