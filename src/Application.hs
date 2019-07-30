{-# LANGUAGE TemplateHaskell #-}

module Application where

import Api.Core
import Control.Lens
import Snap.Snaplet

newtype App = App { _api :: Snaplet Api }

makeLenses ''App

type AppHandler = Handler App App