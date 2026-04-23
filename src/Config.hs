{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE OverloadedStrings      #-}

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
  { name :: Text,
    passwordHash :: Text,
    isAdmin :: Bool
  }
  deriving (Show, Generic, FromDhall)

data Config = Config
  { serverPath :: Text,
    port :: Natural,
    database :: DatabaseConfig,
    organizers :: [Organizer],
    jwtSecret :: Text
  }
  deriving (Show, Generic, FromDhall)

loadConfig :: IO Config
loadConfig = input auto "./config/Config.dhall"
