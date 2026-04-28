{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}

module Api.Types where

import           Data.Aeson         (FromJSON, ToJSON (..))
import           Data.Text          (Text)
import           Data.Time.Calendar (Day)
import           GHC.Generics       (Generic)
import           Numeric.Natural    (Natural)
import           Servant            (FromHttpApiData (..))

-- Newtypes shared across request and response boundaries

newtype QuizName = QuizName {unQuizName :: Text}
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype Place = Place {unPlace :: Text}
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype TeamName = TeamName {unTeamName :: Text}
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype RoundNumber = RoundNumber {unRoundNumber :: Int}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype Points = Points {unPoints :: Double}
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype QuizId = QuizId {unQuizId :: Int}
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON)

instance FromHttpApiData QuizId where
  parseUrlPiece = fmap QuizId . parseUrlPiece

newtype TeamNumber = TeamNumber {unTeamNumber :: Int}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype NumberOfQuestions = NumberOfQuestions {unNumberOfQuestions :: Natural}
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

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
  , numberOfQuestions :: NumberOfQuestions
  }
  deriving (Show, Eq, Generic, ToJSON)

data Team = Team
  { number :: TeamNumber
  , name   :: TeamName
  , active :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON)

data ScoreEntry = ScoreEntry
  { teamNumber  :: TeamNumber
  , roundNumber :: RoundNumber
  , points      :: Points
  }
  deriving (Show, Eq, Generic, ToJSON)

data ScoreBoard = ScoreBoard
  { teams  :: [Team]
  , scores :: [ScoreEntry]
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
