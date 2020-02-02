{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Db.Connection where

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Logger        (runStderrLoggingT)
import           Data.Time.Calendar          (Day)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Db.Configuration            (readConfiguration, toConnection)
import           Db.Instances
import           GHC.Natural                 (Natural)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
DbQuiz
  place String
  date Day
  name String
  locked Bool
  deriving Show
DbLabels
  quizId DbQuizId
  roundLabel String
  teamLabel String
  ownPointsLabel String
  maxReachedLabel String
  maxReachableLabel String
  backToChartView String
  mainLabel String
  ownPageLabel String
  viewPrevious String
  cumulativeLabel String
  individualRoundsLabel String
  progressionLabel String
  placementLabel String
  placeLabel String
  pointsLabel String
  roundWinnerLabel String
  deriving Show
DbTeamNameCode
  quizId DbQuizId
  teamNumber Natural
  teamCode String
  teamName String
  active Bool
  Primary quizId teamNumber
  deriving Show
DbRoundReachable
  quizId DbQuizId
  roundNumber Natural
  points Double
  Primary quizId roundNumber
  deriving Show
DbRoundReached
  quizId DbQuizId
  roundNumber Natural
  teamNumber Natural
  points Double
  Primary quizId roundNumber teamNumber
  deriving Show
|]

connStr = "host=localhost dbname=testdb user=test password=test port=5432"

performMigration :: IO ()
performMigration =
  readConfiguration >>= \config ->
    runStderrLoggingT $
    withPostgresqlPool (toConnection config) 10 $ \pool ->
      liftIO $ flip runSqlPersistMPool pool $ runMigration migrateAll
