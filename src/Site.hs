{-# LANGUAGE OverloadedStrings #-}

module Site ( app ) where

import Data.ByteString (ByteString)
import Snap.Snaplet

import Api.Core ( Api( Api ), apiInit )
import Application

routes :: [(ByteString, Handler App App ())]
routes = []

app :: SnapletInit App App
app = makeSnaplet "app" "Example application" Nothing $ do
    api <- nestSnaplet "api" api apiInit
    addRoutes routes
    return $ App api