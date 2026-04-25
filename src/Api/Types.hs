{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE RankNTypes            #-}

module Api.Types where

import           Data.Aeson         (FromJSON, ToJSON (..))
import           Data.Map.Strict    (Map)
import           Data.Text          (Text)
import           Data.Time.Calendar (Day)
import           GHC.Generics       (Generic)
import           Numeric.Natural    (Natural)
import           Servant            (FromHttpApiData (..))

-- Newtypes shared across request and response boundaries

newtype QuizName = QuizName {unQuizName :: Text}
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype Place = Place {unPlace :: Text}
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype TeamName = TeamName {unTeamName :: Text}
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype RoundNumber = RoundNumber {unRoundNumber :: Int}
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

newtype Points = Points {unPoints :: Double}
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype QuizId = QuizId {unQuizId :: Int}
  deriving (Show, Eq, Generic, ToJSON)

instance FromHttpApiData QuizId where
  parseUrlPiece = fmap QuizId . parseUrlPiece

newtype TeamNumber = TeamNumber {unTeamNumber :: Int}
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

instance FromHttpApiData TeamNumber where
  parseUrlPiece = fmap TeamNumber . parseUrlPiece

newtype NumberOfQuestions = NumberOfQuestions {unNumberOfQuestions :: Natural}
  deriving (Show, Eq, Generic, FromJSON)

-- Composite request type

data QuizIdentifier = QuizIdentifier
  { name  :: QuizName
  , place :: Place
  , date  :: Day
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data QuizSettings = QuizSettings
  { questionsPerRound :: [NumberOfQuestions]
  , numberOfTeams     :: Int
  }
  deriving (Show, Eq, Generic, FromJSON)

-- Response-only types

data QuizState = Active | Locked

data Quiz (state :: QuizState) = Quiz
  { quizId     :: QuizId
  , identifier :: QuizIdentifier
  , rounds     :: [Round]
  , scoreBoard :: ScoreBoard
  }
  deriving (Show, Eq, Generic, ToJSON)

data Round = Round
  { number            :: RoundNumber
  , displayMaxPoints  :: Points
  , numberOfQuestions :: Int
  }
  deriving (Show, Eq, Generic, ToJSON)

data Team = Team
  { number :: TeamNumber
  , name   :: TeamName
  , active :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON)

data ScoreBoard = ScoreBoard
  { teams  :: [Team]
  , scores :: Map (TeamNumber, RoundNumber) Points
  }
  deriving (Show, Eq, Generic, ToJSON)

data SomeQuiz where
  SomeActive :: Quiz Active -> SomeQuiz
  SomeLocked :: Quiz Locked -> SomeQuiz

instance ToJSON SomeQuiz where
  toJSON (SomeActive quiz) = toJSON quiz
  toJSON (SomeLocked quiz) = toJSON quiz

fromActivity :: Bool -> (forall state. Quiz state) -> SomeQuiz
fromActivity True  quiz = SomeActive quiz
fromActivity False quiz = SomeLocked quiz

data QuizSummary = QuizSummary
  { quizId     :: QuizId
  , identifier :: QuizIdentifier
  , active     :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON)
