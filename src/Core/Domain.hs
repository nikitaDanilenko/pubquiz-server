{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module Core.Domain where

import           Data.Aeson         (FromJSON, ToJSON)
import           Data.Map.Strict    (Map)
import           Data.Text          (Text)
import           Data.Time.Calendar (Day)
import           GHC.Generics       (Generic)
import           Numeric.Natural    (Natural)

data QuizState = Active | Locked

newtype QuizName = QuizName {unQuizName :: Text} deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype Place = Place {unPlace :: Text} deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype TeamName = TeamName {unTeamName :: Text} deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype RoundNumber = RoundNumber {unRoundNumber :: Int} deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

newtype Points = Points {unPoints :: Double} deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype QuizId = QuizId {unQuizId :: Int} deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype TeamNumber = TeamNumber {unTeamNumber :: Int} deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

newtype NumberOfQuestions = NumberOfQuestions {unNumberOfQuestions :: Natural} deriving (Show, Eq, Generic, FromJSON, ToJSON)

data QuizIdentifier = QuizIdentifier
  { name  :: QuizName,
    place :: Place,
    date  :: Day
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data QuizSettings = QuizSettings
  { -- The number of the round is the index in the list + 1
    questionsPerRound :: [NumberOfQuestions],
    numberOfTeams     :: Int
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- Core domain types
data Quiz (state :: QuizState) = Quiz
  { quizId     :: QuizId,
    identifier :: QuizIdentifier,
    rounds     :: [Round],
    teams      :: [Team],
    scoreBoard :: ScoreBoard
  }
  deriving (Show, Eq, Generic)

data Round = Round
  { roundNumber       :: RoundNumber,
    displayMaxPoints  :: Points,
    numberOfQuestions :: Int
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Team = Team
  { number   :: TeamNumber,
    teamName :: TeamName,
    active   :: Bool
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype ScoreBoard = ScoreBoard
  { unScoreBoard :: Map (TeamNumber, RoundNumber) Points
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data SomeQuiz where
  SomeActive :: Quiz Active -> SomeQuiz
  SomeLocked :: Quiz Locked -> SomeQuiz
