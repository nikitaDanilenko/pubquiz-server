{-# LANGUAGE TemplateHaskell #-}

module Api.Requests.SecretRequest where

import           Api.Services.SavedUserHandler (Password)
import           Data.Aeson.TH                 (deriveJSON)
import           General.Types                 (UserName)
import           Utils                         (elmOptions)

data SecretRequest =
  SecretRequest
    { secretRequestUserName :: UserName
    , secretRequestPassword :: Password
    }

deriveJSON elmOptions ''SecretRequest
