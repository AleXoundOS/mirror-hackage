{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module DownloadFile
  ( Request, HashSink
  , downloadCheckAndSave, downloadCheckAndSave'
  , downloadAndSave, downloadAndSave'
  , showDownloadError
  , mkRequest
  , sinkHash, sinkSha256, sinkBypass
  , sha256
  ) where

import Conduit
import Control.Exception.Safe
import Data.ByteString (ByteString, readFile, writeFile)
import Data.Foldable (sequenceA_)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Network.HTTP.Client (Response, BodyReader)
import Network.HTTP.Req
import Network.HTTP.Req.Conduit (responseBodySource)
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.FilePath.Posix (takeDirectory)
import System.IO (hClose)
import System.IO.Temp (withTempFile)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as Base16 (encode)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)


type Request = Either (Url 'Http, Option 'Http) (Url 'Https, Option 'Https)
type HashSink = ConduitT ByteString Void (ResourceT IO) ByteString

data DownloadError = HttpError HttpException
                   | CheckError (ByteString, ByteString) FilePath
  deriving (Show)

data HashFuncs c = HashFuncs
  { _ctx      :: c
  , _update   :: c -> ByteString -> c
  , _finalize :: c -> ByteString
  }


showDownloadError :: DownloadError -> String
showDownloadError (HttpError he) = show he
showDownloadError (CheckError (csExp, csGot) fp) =
  "Downloaded file (" ++ fp ++ ") checksum validation failure!\n" ++
  "Expected: " ++ show (Base16.encode csExp) ++ "\n" ++
  "Got:      " ++ show (Base16.encode csGot)


sha256 :: HashFuncs SHA256.Ctx
sha256 = HashFuncs SHA256.init SHA256.update SHA256.finalize

sinkSha256 :: HashSink
sinkSha256 = sinkHash sha256

myRespTimeout :: Option scheme
myRespTimeout = responseTimeout $ 60 * 1000000  -- ^ 60 seconds

-- | GET an HTTP body. Returns entire HTTP body as `Right` strict `ByteString`.
-- `HttpException`s are treated as `Left`.
get :: Request -> IO (Either HttpException ByteString)
get r =
  -- FIXME: can it be better???? wtf?
  case r of
    (Left x) -> doReq x
    (Right x) -> doReq x
  where
    doReq (url, options) = handle handler
      $ fmap (Right . responseBody)
      $ runReq defaultHttpConfig
      $ req GET url NoReqBody bsResponse (options <> myRespTimeout)
    -- | Catch only `HttpException`.
    handler :: HttpException -> IO (Either HttpException ByteString)
    handler = return . Left

getWithBodyReader :: Request -> (Response BodyReader -> IO a)
                  -> IO (Either HttpException a)
getWithBodyReader r bodyReader =
  -- FIXME: can it be better???? wtf?
  case r of
    (Left x) -> doReq x
    (Right x) -> doReq x
  where
    doReq (url, options) = handle handler $ fmap Right
      $ runReq defaultHttpConfig
      $ reqBr GET url NoReqBody (options <> myRespTimeout) bodyReader
      -- | Catch only `HttpException`.
    handler :: HttpException -> IO (Either HttpException a)
    handler = return . Left

parseUrl = undefined

mkRequest :: Text -> Request
mkRequest t = fromMaybe
  (error $ "cannot parse url: " ++ T.unpack t)
  -- Why does it work with `Request` being not polymorphic??????
  -- What if we get (Url 'Https) (Option 'Http)???
  (parseUrl $ T.encodeUtf8 t)

-- | Downloads a file if it hasn't been found in FS, checks and writes to a file
-- system. Download is a stream of HTTP body to a temporary file, computing the
-- checksum on the fly. If the checksum matches, the file is renamed to the
-- given name. Returns the resulting `FilePath` as `Right`. Otherwise, it's put
-- into special "bad-checksum" dir. It's assumed that if a file is present it
-- has to be valid, hence download is skipped.
downloadCheckAndSave :: MonadIO m
  => Request -> FilePath
  -> (ConduitT ByteString Void (ResourceT IO) ByteString, ByteString)
  -> FilePath
  -> m (Either DownloadError FilePath)
downloadCheckAndSave request filename (sink, hash1) destPath = do
  let filepath = destPath ++ "/" ++ filename
      filepathBadCS = destPath ++ ".bad-checksum/" ++ filename
  exists <- liftIO $ doesFileExist filepath
  if exists
    then return (Right filepath)
    else liftIO
         $ withTempFile destPath tmpFileTemplate
         $ \fpTmp hndl ->
             do
               dlRes <- getWithBodyReader request $ bodyReader hndl
               case dlRes of
                 Left httpExcept -> return $ Left (HttpError httpExcept)
                 Right hash2 ->
                   condRename (hash1 == hash2) fpTmp
                   (filepathBadCS, Left . CheckError (hash1, hash2))
                   (filepath, Right)
  where
    tmpFileTemplate = T.unpack $ T.takeWhileEnd (/= '/') $ T.pack filename
    bodyReader hndl r = do
      hash <- runConduitRes
        $ responseBodySource r
        .| getZipSink (ZipSink (sinkHandle hndl) *> ZipSink sink)
      hClose hndl
      return hash
    condRename bool src (dstFalse, useFalse) (dstTrue, useTrue) =
      let (dst, use) = if bool
                       then (dstTrue, useTrue)
                       else (dstFalse, useFalse)
      in do
        createDirectoryIfMissing True $ takeDirectory dst
        -- | `withTempFile` allows removing the temporary file inside the action
        renameFile src dst
        return $ use dst

-- | Same as `downloadCheckAndSave`, except throws exception on `DownloadError`
-- with msg.
downloadCheckAndSave' :: MonadIO m
  => Request -> FilePath
  -> (ConduitT ByteString Void (ResourceT IO) ByteString, ByteString)
  -> FilePath
  -> (DownloadError -> String)
  -> m FilePath
downloadCheckAndSave' r f (s, h) d mkErrMsg = downloadCheckAndSave r f (s, h) d
  >>= \case Left de -> error $ mkErrMsg de
            Right fp -> return fp

-- | Download (GET) a file and write to FS (without checksum validation).
-- Returns entire HTTP body as `Right` strict `ByteString`. Throws an exception
-- if file exists. `HttpException`s are treated as `Left`.
downloadAndSave :: MonadIO m
  => Request -> FilePath -> FilePath -> m (Either HttpException ByteString)
downloadAndSave request filename destPath = do
  let filepath = destPath ++ "/" ++ filename
  exists <- liftIO $ doesFileExist filepath
  if exists
    then Right <$> liftIO (Data.ByteString.readFile filepath)
    else liftIO $ get request
         >>= \eRes ->
               sequenceA_ (Data.ByteString.writeFile filepath <$> eRes)
               >> return eRes

downloadAndSave' :: MonadIO m => Request -> FilePath -> FilePath -> m ByteString
downloadAndSave' r f d =
  either (error . mkErrMsg) id <$> downloadAndSave r f d
  where
    mkErrMsg he = error
      $ "exception while downloading `" ++ urlShow ++
      "` to \"" ++ f ++ "\"\n" ++ show he ++ "`"
    urlShow = case r of
                Left (url, _) -> show url
                Right (url, _) -> show url

-- | A 'Sink' that hashes a stream of 'ByteString'@s@ and creates a digest.
sinkHash :: Monad m => HashFuncs a -> ConduitT ByteString Void m ByteString
sinkHash (HashFuncs init' update finalize) = sink init'
  where
    sink ctx = do
      b <- await
      case b of
        Nothing -> return $! finalize ctx
        Just bs -> sink $! update ctx bs

sinkBypass :: ConduitT ByteString Void m ByteString
sinkBypass = return $! mempty
