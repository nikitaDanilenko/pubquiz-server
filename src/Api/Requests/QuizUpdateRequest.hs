{-# LANGUAGE TemplateHaskell #-}

module General.Requests.QuizUpdateRequest where

import           Data.Aeson.TH   (defaultOptions, deriveJSON)
import           Db.Connection   (DbQuizId)
import           Db.DbConversion (QuizIdentifier, QuizSettings)

data QuizUpdateRequest =
  QuizUpdateRequest
    { quizUpdateRequestQuizId         :: DbQuizId
    , quizUpdateRequestQuizIdentifier :: QuizIdentifier
    , quizUpdateRequestQuizSettings   :: QuizSettings
    }

deriveJSON defaultOptions ''QuizUpdateRequest
