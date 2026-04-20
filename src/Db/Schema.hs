-- All these are necessary for the schema definition.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

---

module Db.Schema where

import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Quiz
  place String
  date Day
  name String
  active Bool
  UniqueQuiz place date name
  deriving Show
Team
  quizId QuizId
  number Int
  name String
  active Bool
  Primary quizId number
  deriving Show
Round
  quizId QuizId
  roundNumber Int
  reachablePoints Double
  numberOfQuestions Int
  Primary quizId roundNumber
  deriving Show
TeamRoundScore
  quizId QuizId
  roundNumber Int
  teamNumber Int
  points Double
  Primary quizId roundNumber teamNumber
  Foreign Round fkTeamRoundScoreRound quizId roundNumber
  Foreign Team fkTeamRoundScoreTeam quizId teamNumber
  deriving Show
Organizer
  name String
  salt String
  hash String
  Primary name
  UniqueOrganizerName name
  deriving Show
LoginThrottle
  organizerName String
  failedAttempts Int
  lastAttemptAt UTCTime
  Primary organizerName
  Foreign Organizer fkLoginThrottleOrganizer organizerName References name
  deriving Show
|]