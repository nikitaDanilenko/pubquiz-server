{-# LANGUAGE TupleSections #-}
module Api.Services.SnapUtil where

import           Data.Aeson            (decode, ToJSON, encode)
import           Data.Aeson.Types      (FromJSON)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as L
import           Data.CaseInsensitive  (CI, mk)
import           Snap.Core             (Response, setContentType, setHeader,
                                        setResponseCode, getPostParam)
import Snap (Handler)
import Data.Maybe (fromMaybe)

setResponseCodePlain :: Int -> Response -> Response
setResponseCodePlain code = setResponseCode code . setContentType (B.pack "text/plain")

setResponseCodeJSON :: Int -> Response -> Response
setResponseCodeJSON code = setResponseCode code . setHeader (mkFromString "Content-Type") (B.pack "application/json")

mkFromString :: String -> CI B.ByteString
mkFromString = mk . B.pack
--todo : Check uses of this function and replace with getJSONPostParam
attemptDecode :: (Functor f, FromJSON a) => f (Maybe B.ByteString) -> f (Maybe a)
attemptDecode = fmap maybeDecode

maybeDecode :: FromJSON a => Maybe B.ByteString -> Maybe a
maybeDecode = (>>= strictDecode)

strictDecode :: FromJSON a => B.ByteString -> Maybe a
strictDecode = decode . L.fromStrict

strictEncode :: (Functor f, ToJSON a) => f a -> f B.ByteString
strictEncode = fmap (L.toStrict . encode)

encodeOrEmpty :: ToJSON a => Maybe a -> B.ByteString
encodeOrEmpty = fromMaybe (B.pack "") . strictEncode

getJSONPostParamWithPure :: FromJSON a => B.ByteString -> Handler b service (Maybe (B.ByteString, a))
getJSONPostParamWithPure = fmap (\mValue -> maybeDecode mValue >>= \v -> fmap (, v) mValue) . getPostParam

getJSONPostParam :: FromJSON a => B.ByteString -> Handler b service (Maybe a)
getJSONPostParam = fmap (fmap snd) . getJSONPostParamWithPure