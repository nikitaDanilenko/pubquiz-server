{-# LANGUAGE TemplateHaskell #-}

module Application where

import Api.Core     ( Api )
import Control.Lens ( makeLenses )
import Snap.Snaplet ( Snaplet, Handler )

newtype App = App { _api :: Snaplet Api }

makeLenses ''App

type AppHandler = Handler App App