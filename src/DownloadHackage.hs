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
  { dlReq  :: Request
  , dlHash :: Maybe Hash
  , dlFp   :: FilePath
  }

instance Show Download where
  show (Download (Right (url, _)) hash fp) =
    "url: " ++ show url ++ "\nhash: " ++ show hash ++ "\nfp: " ++ show fp


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
        { dlReq  = Right (pkgArchiveUrl, mempty)
        , dlHash = Just hash
        , dlFp   = T.unpack $ pkgHyphenVer <> pkgHyphenVer <> ".tar.gz"
        }
      mkDlOfCabal :: RevisionData -> Download
      mkDlOfCabal (RevisionData _ revNum cabalHash) = Download
        { dlReq =
            Right (pkgBaseUrl /: "revisions" /: T.pack (show revNum), mempty)
        , dlHash = Just cabalHash
        , dlFp = T.unpack $ pkgHyphenVer <> "revisions" <> T.pack (show revNum)
        }
  in dlOfArchive : map mkDlOfCabal (HM.elems revs)
