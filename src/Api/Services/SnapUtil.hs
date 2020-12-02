{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}

module Api.Services.SnapUtil where

import           Control.Applicative       (liftA2)
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import           Data.Aeson                (decode)
import           Data.Aeson.Types          (FromJSON)
import qualified Data.ByteString.Char8     as B
import           Data.CaseInsensitive      (CI, mk)
import qualified Data.Text.Encoding        as E (encodeUtf8)
import           Data.Word                 (Word64)
import           Db.DbConversion           (Credentials (Credentials))
import           General.Types             (Wrapped, wrap)
import           Snap                      (Handler, readRequestBody)
import           Snap.Core                 (Request, Response, getHeader,
                                            getRequest, setHeader,
                                            setResponseCode)

-- todo: Use general constants for header values
setResponseCodeJSON :: Int -> Response -> Response
setResponseCodeJSON code = setResponseCode code . setHeader (mkFromString "Content-Type") (B.pack "application/json")

mkFromString :: String -> CI B.ByteString
mkFromString = mk . B.pack

readBody :: FromJSON a => Handler b service (Maybe a)
readBody = fmap decode (readRequestBody maxRequestSize)

readCredentials :: MaybeT (Handler b service) Credentials
readCredentials = liftA2 Credentials (readHeader userHeader) (readHeader signatureHeader)

userHeader :: CI B.ByteString
userHeader = mkFromString "User"

signatureHeader :: CI B.ByteString
signatureHeader = mkFromString "Signature"

maxRequestSize :: Word64
maxRequestSize = 65536

readHeader :: Wrapped v B.ByteString => CI B.ByteString -> MaybeT (Handler b service) v
readHeader hd = fmap wrap (MaybeT (fmap (getHeader hd) getRequest))
