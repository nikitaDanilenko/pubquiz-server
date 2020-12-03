{-# LANGUAGE TemplateHaskell #-}

module Api.Requests.QuizUpdateRequest where

import           Data.Aeson.TH   (deriveJSON)
import           Db.Connection   (DbQuizId)
import           Db.DbConversion (QuizIdentifier, QuizSettings)
import           Utils           (elmOptions)

data QuizUpdateRequest =
  QuizUpdateRequest
    { quizUpdateRequestQuizId         :: DbQuizId
    , quizUpdateRequestQuizIdentifier :: QuizIdentifier
    , quizUpdateRequestQuizSettings   :: QuizSettings
    }

deriveJSON elmOptions ''QuizUpdateRequest
