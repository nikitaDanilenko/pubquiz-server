{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE TemplateHaskell  #-}

module Api.Services.SnapUtil where

import           Api.Services.HashCheck     (authenticate)
import           Control.Applicative        (liftA2)
import           Control.Monad.Trans.Except (ExceptT (ExceptT))
import           Data.Aeson                 (ToJSON, eitherDecode, encode)
import           Data.Aeson.TH              (defaultOptions, deriveJSON)
import           Data.Aeson.Types           (FromJSON)
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy       as L
import           Data.CaseInsensitive       (CI, mk, original)
import           Data.Either.Combinators    (mapLeft)
import           Data.Word                  (Word64)
import           Db.DbConversion            (Credentials (Credentials))
import           General.EitherT.Extra      (exceptFromMaybeF)
import           General.Types              (Wrapped, wrap)
import           Snap                       (readRequestBody)
import           Snap.Core                  (MonadSnap, Response, getHeader,
                                             getRequest, modifyResponse,
                                             setHeader, setResponseCode,
                                             writeLBS)

-- todo: Use general constants for header values
setResponseCodeJSON :: Int -> Response -> Response
setResponseCodeJSON code = setResponseCode code . setHeader (mkFromString "Content-Type") (B.pack "application/json")

mkFromString :: String -> CI B.ByteString
mkFromString = mk . B.pack

readParsedBody :: (FromJSON a, MonadSnap m) => ExceptT L.ByteString m (Parsed a)
readParsedBody =
  ExceptT
    (fmap
       (\a -> fmap (Parsed (L.toStrict a)) (mapLeft (L.fromStrict . B.pack) (eitherDecode a)))
       (readRequestBody maxRequestSize))

readBody :: (FromJSON a, MonadSnap m) => ExceptT L.ByteString m a
readBody = fmap parsedJson readParsedBody

readCredentials :: MonadSnap m => ExceptT L.ByteString m Credentials
readCredentials = liftA2 Credentials (readHeader userHeader) (readHeader signatureHeader)

userHeader :: CI B.ByteString
userHeader = mkFromString "User"

signatureHeader :: CI B.ByteString
signatureHeader = mkFromString "Signature"

maxRequestSize :: Word64
maxRequestSize = 65536

readHeader :: (Wrapped v B.ByteString, MonadSnap m) => CI B.ByteString -> ExceptT L.ByteString m v
readHeader hd = fmap wrap (exceptFromMaybeF (fmap (getHeader hd) getRequest) errorMsg)
  where
    errorMsg = L.fromStrict (B.unwords [B.pack "Missing header:", original hd])

jsonResponseCode :: MonadSnap m => Int -> m ()
jsonResponseCode = modifyResponse . setResponseCodeJSON

anyResponseCode :: MonadSnap m => Int -> m ()
anyResponseCode = modifyResponse . setResponseCode

errorWithCode :: MonadSnap m => Int -> L.ByteString -> m ()
errorWithCode c e =
  writeLBS (encode (ErrorResponse (B.unpack (L.toStrict e)))) >> modifyResponse (setResponseCodeJSON c)

okJsonResponse :: (ToJSON v, MonadSnap m) => v -> m ()
okJsonResponse value = do
  writeLBS (encode value)
  jsonResponseCode 200

data Parsed a =
  Parsed
    { originalText :: B.ByteString
    , parsedJson   :: a
    }

newtype ErrorResponse =
  ErrorResponse
    { errorResponse :: String
    }

deriveJSON defaultOptions ''ErrorResponse

readVerified :: (FromJSON a, MonadSnap m) => ExceptT L.ByteString m a
readVerified = do
  credentials <- readCredentials
  parsed <- readParsedBody
  authenticate credentials (originalText parsed)
  pure (parsedJson parsed)
