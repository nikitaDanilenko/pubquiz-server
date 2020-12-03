{-# LANGUAGE TemplateHaskell #-}

module Api.Requests.QuizIdRequest where

import           Data.Aeson.TH (deriveJSON)
import           Db.Connection (DbQuizId)
import           Utils         (elmOptions)

newtype QuizIdRequest =
  QuizIdRequest
    { quizIdRequestQuizId :: DbQuizId
    }

deriveJSON elmOptions ''QuizIdRequest
