{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Api.BackOffice.Types where

import           Api.Types           (Points, QuizIdentifier, QuizSettings,
                                      RoundNumber, TeamName, TeamNumber)
import           Data.Aeson          (FromJSON, ToJSON)
import           GHC.Generics        (Generic)
import           Servant.Auth.Server (FromJWT, ToJWT)

-- JWT payload: authenticated organizer
newtype AuthenticatedUser = AuthenticatedUser
  { isAdmin :: Bool
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToJWT, FromJWT)

-- Request types (FromJSON only)

data QuizMetaData = QuizMetaData
  { identifier :: QuizIdentifier
  , settings   :: QuizSettings
  }
  deriving (Show, Eq, Generic, FromJSON)

newtype ChangeSettingsCommand = ChangeSettingsCommand
  { newIdentifier :: QuizIdentifier
  }
  deriving (Show, Eq, Generic, FromJSON)

newtype AddTeamsCommand = AddTeamsCommand
  { additionalTeams :: Int
  }
  deriving (Show, Eq, Generic, FromJSON)

data RecordRoundScoresCommand = RecordRoundScoresCommand
  { roundNumber :: RoundNumber
  , scores      :: [(TeamNumber, Points)]
  }
  deriving (Show, Eq, Generic, FromJSON)

data CorrectScoreCommand = CorrectScoreCommand
  { teamNumber  :: TeamNumber
  , roundNumber :: RoundNumber
  , points      :: Points
  }
  deriving (Show, Eq, Generic, FromJSON)

data RenameTeamCommand = RenameTeamCommand
  { teamNumber :: TeamNumber
  , newName    :: TeamName
  }
  deriving (Show, Eq, Generic, FromJSON)

data SetTeamActiveCommand = SetTeamActiveCommand
  { teamNumber :: TeamNumber
  , active     :: Bool
  }
  deriving (Show, Eq, Generic, FromJSON)
