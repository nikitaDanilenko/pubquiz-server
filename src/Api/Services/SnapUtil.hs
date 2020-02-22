module Api.Services.SnapUtil where

import           Data.Aeson            (decode)
import           Data.Aeson.Types      (FromJSON)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as L
import           Data.CaseInsensitive  (CI, mk)
import           Snap.Core             (Response, setContentType, setHeader,
                                        setResponseCode, getPostParam)
import Snap (Handler)

setResponseCodePlain :: Int -> Response -> Response
setResponseCodePlain code = setResponseCode code . setContentType (B.pack "text/plain")

setResponseCodeJSON :: Int -> Response -> Response
setResponseCodeJSON code = setResponseCode code . setHeader (mkFromString "Content-Type") (B.pack "application/json")

mkFromString :: String -> CI B.ByteString
mkFromString = mk . B.pack

attemptDecode :: (Functor f, FromJSON a) => f (Maybe B.ByteString) -> f (Maybe a)
attemptDecode = fmap (>>= decode . L.fromStrict)

getJSONPostParam :: FromJSON a => B.ByteString -> Handler b service (Maybe a)
getJSONPostParam = attemptDecode . getPostParam