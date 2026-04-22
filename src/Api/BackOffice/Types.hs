{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Api.BackOffice.Types where

import           Core.Domain  (QuizId, QuizIdentifier)
import           Data.Aeson   (ToJSON)
import           GHC.Generics (Generic)

data QuizSummary = QuizSummary
  { quizId     :: QuizId
  , identifier :: QuizIdentifier
  , active     :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON)
