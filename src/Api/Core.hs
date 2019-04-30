{-# Language OverloadedStrings #-}

module Api.Core where

import qualified Data.ByteString.Char8 as B
import           Snap.Core
import           Snap.Snaplet

data Api = Api

apiRoutes :: [(B.ByteString, Handler b Api ())]
apiRoutes = [("status", method GET respondOk)]

respondOk :: Handler b Api ()
respondOk = modifyResponse $ setResponseCode 200

apiInit :: SnapletInit b Api
apiInit = makeSnaplet "api" "Core Api" Nothing $ do
    addRoutes apiRoutes
    return Api