-- All of these are necessary for the schema definition.
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

---

module Db.Schema where

import           Data.Text           (Text)
import           Data.Time.Calendar  (Day)
import           Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase,
                                      share, sqlSettings)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Quiz
  place Text
  date Day
  name Text
  active Bool
  UniqueQuiz place date name
  deriving Show
Team
  quizId QuizId
  number Int
  name Text
  active Bool
  Primary quizId number
  deriving Show
Round
  quizId QuizId
  number Int
  reachablePoints Double
  numberOfQuestions Int
  Primary quizId number
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
|]
