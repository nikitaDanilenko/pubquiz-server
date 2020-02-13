module Db.DbTypes where

newtype TeamNumber =
  TeamNumber Integer

newtype RoundNumber =
  RoundNumber Integer

newtype Code =
  Code String

newtype TeamName =
  TeamName String

newtype QuizName =
  QuizName String
  
newtype Place =
  Place String

data Activity = Active | Inactive
