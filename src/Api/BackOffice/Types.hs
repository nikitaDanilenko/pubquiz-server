{-# LANGUAGE DuplicateRecordFields #-}

module Api.BackOffice.Types where

import Core.Domain (QuizIdentifier)

data QuizSummary = QuizSummary
  { identifier :: QuizIdentifier,
    teamCount :: Int,
    roundCount :: Int
  }
  deriving (Show, Eq)
