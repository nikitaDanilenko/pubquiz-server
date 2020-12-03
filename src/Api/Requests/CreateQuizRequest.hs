{-# LANGUAGE TemplateHaskell #-}

module Api.Requests.CreateQuizRequest where

import           Data.Aeson.TH   (deriveJSON)
import           Db.DbConversion (QuizIdentifier, QuizSettings)
import           Utils           (elmOptions)

data CreateQuizRequest =
  CreateQuizRequest
    { createQuizRequestQuizIdentifier :: QuizIdentifier
    , createQuizRequestQuizSettings   :: QuizSettings
    }

deriveJSON elmOptions ''CreateQuizRequest
