module Db.DbTypes where

import           GHC.Natural (Natural)

newtype TeamNumber =
  TeamNumber
    { unTeamNumber :: Natural
    }

newtype RoundNumber =
  RoundNumber
    { unRoundNumber :: Natural
    }

newtype Code =
  Code String

newtype TeamName =
  TeamName String

newtype QuizName =
  QuizName String

newtype Place =
  Place String

data Activity
  = Active
  | Inactive
