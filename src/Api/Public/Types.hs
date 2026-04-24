{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Api.Public.Types where

import           Core.Domain  (Points, QuizIdentifier, RoundNumber, TeamName,
                               TeamNumber)
import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

-- Team's perspective (filtered to show only their scores and leaderboard)
data TeamView = TeamView
  { identifier :: QuizIdentifier,
    team       :: TeamInfo,
    scores     :: [RoundScore],
    standings  :: [StandingEntry]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data StandingEntry = StandingEntry
  { rank            :: Int,
    teamName        :: TeamName,
    totalPoints     :: Points,
    reachablePoints :: Points
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data TeamInfo = TeamInfo
  { number :: TeamNumber,
    name   :: TeamName
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data RoundScore = RoundScore
  { roundNumber :: RoundNumber,
    points      :: Points
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
