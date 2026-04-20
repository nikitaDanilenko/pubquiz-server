{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Core.Domain where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time.Calendar (Day)

data QuizState = Active | Locked

newtype QuizName = QuizName {unQuizName :: Text} deriving (Show, Eq)

newtype Place = Place {unPlace :: Text} deriving (Show, Eq)

newtype TeamName = TeamName {unTeamName :: Text} deriving (Show, Eq)

newtype RoundNumber = RoundNumber {unRoundNumber :: Int} deriving (Show, Eq, Ord)

newtype Points = Points {unPoints :: Double} deriving (Show, Eq, Ord)

newtype QuizId = QuizId {unQuizId :: Int} deriving (Show, Eq, Ord)

newtype TeamNumber = TeamNumber {unTeamNumber :: Int} deriving (Show, Eq, Ord)

data QuizIdentifier = QuizIdentifier
  { name :: QuizName,
    place :: Place,
    date :: Day
  }
  deriving (Show, Eq)

-- Core domain types
data Quiz (state :: QuizState) = Quiz
  { quizId :: QuizId,
    identifier :: QuizIdentifier,
    rounds :: [Round],
    teams :: [Team],
    scoreBoard :: ScoreBoard
  }
  deriving (Show, Eq)

data Round = Round
  { roundNumber :: RoundNumber,
    displayMaxPoints :: Points,
    numberOfQuestions :: Int
  }
  deriving (Show, Eq)

data Team = Team
  { number :: TeamNumber,
    teamName :: TeamName,
    active :: Bool
  }
  deriving (Show, Eq)

newtype ScoreBoard = ScoreBoard
  { unScoreBoard :: Map (TeamNumber, RoundNumber) Points
  }
  deriving (Show, Eq)

data SomeQuiz where
  SomeActive :: Quiz Active -> SomeQuiz
  SomeLocked :: Quiz Locked -> SomeQuiz