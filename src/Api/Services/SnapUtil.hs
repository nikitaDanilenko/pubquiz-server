module Api.Services.SnapUtil where

import qualified Data.ByteString.Char8 as B 
import Snap.Core                            ( Response, setContentType, setResponseCode )

setResponseCodePlain :: Int -> Response -> Response
setResponseCodePlain code = setResponseCode code . setContentType (B.pack "text/plain")