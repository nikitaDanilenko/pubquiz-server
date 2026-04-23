{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Api.Public.Types where

import           Core.Domain        (Place, Points, QuizIdentifier, QuizName,
                                     RoundNumber, TeamName, TeamNumber)
import           Data.Aeson         (FromJSON, ToJSON)
import           Data.Time.Calendar (Day)
import           GHC.Generics       (Generic)

-- Public list view (search/browse quizzes)
data QuizSummary = QuizSummary
  { identifier :: QuizIdentifier,
    standings  :: [StandingEntry]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- Public single quiz view (leaderboard)
data QuizView = QuizView
  { identifier :: QuizIdentifier,
    standings  :: [StandingEntry],
    roundStats :: [RoundStat]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- Team's perspective (filtered to show only their scores and leaderboard)
data TeamView = TeamView
  { identifier :: QuizIdentifier,
    myTeam     :: TeamInfo,
    myScores   :: [RoundScore],
    standings  :: [StandingEntry]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data StandingEntry = StandingEntry
  { rank        :: Int,
    teamName    :: TeamName,
    totalPoints :: Points
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data RoundStat = RoundStat
  { roundNumber       :: RoundNumber,
    displayMaxPoints  :: Points,
    numberOfQuestions :: Int
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
