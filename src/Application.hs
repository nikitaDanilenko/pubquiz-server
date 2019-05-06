{-# LANGUAGE TemplateHaskell #-}

module Application where

import Api.Core
import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session

data App = App { _api :: Snaplet Api }

makeLenses ''App

type AppHandler = Handler App App