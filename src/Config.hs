{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import Dhall (FromDhall, Natural, Text, auto, input)
import GHC.Generics (Generic)

data DatabaseConfig = DatabaseConfig
  { host :: Text,
    name :: Text,
    user :: Text,
    password :: Text,
    port :: Natural
  }
  deriving (Show, Generic, FromDhall)

data Organizer = Organizer
  { organizerName :: Text,
    passwordHash :: Text,
    isAdmin :: Bool
  }
  deriving (Show, Generic, FromDhall)

data Config = Config
  { serverPath :: Text,
    database :: DatabaseConfig,
    organizers :: [Organizer]
  }
  deriving (Show, Generic, FromDhall)

loadConfig :: IO Config
loadConfig = input auto "./config/Config.dhall"
