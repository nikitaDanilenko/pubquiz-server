module Api.Services.SnapUtil where

import qualified Data.ByteString.Char8 as B
import           Data.CaseInsensitive  (CI, mk)
import           Snap.Core             (Response, setContentType, setHeader,
                                        setResponseCode)

setResponseCodePlain :: Int -> Response -> Response
setResponseCodePlain code = setResponseCode code . setContentType (B.pack "text/plain")

setResponseCodeJSON :: Int -> Response -> Response
setResponseCodeJSON code = setResponseCode code . setHeader (mkFromString "Content-Type") (B.pack "application/json")

mkFromString :: String -> CI B.ByteString
mkFromString = mk . B.pack
