{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Config where

import           Dhall        (FromDhall, Natural, Text, auto, input)
import           GHC.Generics (Generic)

data DatabaseConfig = DatabaseConfig
  { host     :: Text,
    name     :: Text,
    user     :: Text,
    password :: Text,
    port     :: Natural
  }
  deriving (Show, Generic, FromDhall)

data Organizer = Organizer
  { name         :: Text,
    passwordHash :: Text,
    isAdmin      :: Bool
  }
  deriving (Show, Generic, FromDhall)

data JwtConfig = JwtConfig
  { secret            :: Text,
    expirationSeconds :: Natural
  }
  deriving (Show, Generic, FromDhall)

data Config = Config
  { port       :: Natural,
    database   :: DatabaseConfig,
    organizers :: [Organizer],
    jwt        :: JwtConfig
  }
  deriving (Show, Generic, FromDhall)

loadConfig :: IO Config
loadConfig = input auto "./config/Config.dhall"
