{-# LANGUAGE DataKinds      #-}
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

newtype QuizName = QuizName {unQuizName :: Text} deriving (Show, Eq, Generic)

instance FromJSON QuizName
instance ToJSON QuizName

newtype Place = Place {unPlace :: Text} deriving (Show, Eq, Generic)
instance FromJSON Place
instance ToJSON Place

newtype TeamName = TeamName {unTeamName :: Text} deriving (Show, Eq, Generic)
instance FromJSON TeamName
instance ToJSON TeamName

newtype RoundNumber = RoundNumber {unRoundNumber :: Int} deriving (Show, Eq, Ord, Generic)
instance FromJSON RoundNumber
instance ToJSON RoundNumber

newtype Points = Points {unPoints :: Double} deriving (Show, Eq, Generic)
instance FromJSON Points
instance ToJSON Points

newtype QuizId = QuizId {unQuizId :: Int} deriving (Show, Eq, Generic)
instance FromJSON QuizId
instance ToJSON QuizId

newtype TeamNumber = TeamNumber {unTeamNumber :: Int} deriving (Show, Eq, Ord, Generic)
instance FromJSON TeamNumber
instance ToJSON TeamNumber

newtype NumberOfQuestions = NumberOfQuestions {unNumberOfQuestions :: Natural} deriving (Show, Eq, Generic)
instance FromJSON NumberOfQuestions
instance ToJSON NumberOfQuestions

data QuizIdentifier = QuizIdentifier
  { name  :: QuizName,
    place :: Place,
    date  :: Day
  }
  deriving (Show, Eq, Generic)

instance FromJSON QuizIdentifier
instance ToJSON QuizIdentifier

data QuizSettings = QuizSettings
  { -- The number of the round is the index in the list + 1
    questionsPerRound :: [NumberOfQuestions],
    numberOfTeams     :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON QuizSettings
instance ToJSON QuizSettings

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
  deriving (Show, Eq, Generic)

instance FromJSON Round
instance ToJSON Round

data Team = Team
  { number   :: TeamNumber,
    teamName :: TeamName,
    active   :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON Team
instance ToJSON Team

newtype ScoreBoard = ScoreBoard
  { unScoreBoard :: Map (TeamNumber, RoundNumber) Points
  }
  deriving (Show, Eq, Generic)

instance FromJSON ScoreBoard
instance ToJSON ScoreBoard

data SomeQuiz where
  SomeActive :: Quiz Active -> SomeQuiz
  SomeLocked :: Quiz Locked -> SomeQuiz
