{-# LANGUAGE TupleSections #-}

module Api.Services.SnapUtil where

import           Data.Aeson            (ToJSON, decode, encode)
import           Data.Aeson.Types      (FromJSON)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as L
import           Data.CaseInsensitive  (CI, mk)
import           Data.Maybe            (fromMaybe)
import           Snap                  (Handler)
import           Snap.Core             (Response, getParam, getPostParam,
                                        setContentType, setHeader,
                                        setResponseCode)

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

strictEncode :: ToJSON a => a -> B.ByteString
strictEncode = L.toStrict . encode

strictEncodeF :: (Functor f, ToJSON a) => f a -> f B.ByteString
strictEncodeF = fmap strictEncode

encodeOrEmpty :: ToJSON a => Maybe a -> B.ByteString
encodeOrEmpty = fromMaybe (B.pack "") . strictEncodeF

processJSONWithPure :: (FromJSON a, Functor f) => f (Maybe B.ByteString) -> f (Maybe (B.ByteString, a))
processJSONWithPure = processJSONWith (\mbs a -> fmap (, a) mbs)

processJSON :: (FromJSON a, Functor f) => f (Maybe B.ByteString) -> f (Maybe a)
processJSON = processJSONWith (const pure)

processJSONWith ::
     (FromJSON a, Functor f) => (Maybe B.ByteString -> a -> Maybe b) -> f (Maybe B.ByteString) -> f (Maybe b)
processJSONWith f = fmap (\mValue -> maybeDecode mValue >>= f mValue)

getJSONPostParamWithPure :: FromJSON a => B.ByteString -> Handler b service (Maybe (B.ByteString, a))
getJSONPostParamWithPure = processJSONWithPure . getPostParam

getJSONPostParam :: FromJSON a => B.ByteString -> Handler b service (Maybe a)
getJSONPostParam = processJSON . getPostParam

getJSONParamWithPure :: FromJSON a => B.ByteString -> Handler b service (Maybe (B.ByteString, a))
getJSONParamWithPure = processJSONWithPure . getParam

getJSONParam :: FromJSON a => B.ByteString -> Handler b service (Maybe a)
getJSONParam = processJSON . getParam

fKey :: Functor f => f (a, b) -> f a
fKey = fmap fst

fValue :: Functor f => f (a, b) -> f b
fValue = fmap snd
