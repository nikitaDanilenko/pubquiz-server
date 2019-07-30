{-# LANGUAGE OverloadedStrings #-}

module Site ( app ) where

import Snap.Snaplet ( SnapletInit, makeSnaplet, nestSnaplet )

import Api.Core     ( apiInit )
import Application  ( App ( .. ), api )

app :: SnapletInit App App
app = makeSnaplet "app" "Example application" Nothing $ do
    apiInstance <- nestSnaplet "api" api apiInit
    return $ App apiInstance