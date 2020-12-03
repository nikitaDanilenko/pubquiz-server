{-# LANGUAGE TemplateHaskell #-}

module Api.Requests.UpdateQuizRatingsRequest where

import           Data.Aeson.TH   (deriveJSON)
import           Db.Connection   (DbQuizId)
import           Db.DbConversion (QuizRatings)
import           Utils           (elmOptions)

data UpdateQuizRatingsRequest =
  UpdateQuizRatingsRequest
    { updateQuizRatingsRequestQuizId      :: DbQuizId
    , updateQuizRatingsRequestQuizRatings :: QuizRatings
    }

deriveJSON elmOptions ''UpdateQuizRatingsRequest
