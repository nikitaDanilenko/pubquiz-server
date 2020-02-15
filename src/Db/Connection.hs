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

import           Control.Monad.IO.Class                (MonadIO, liftIO)
import           Control.Monad.Logger                  (NoLoggingT,
                                                        runStderrLoggingT)
import           Control.Monad.Trans.Reader            (ReaderT)
import           Control.Monad.Trans.Resource.Internal (ResourceT)
import           Data.Time.Calendar                    (Day)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Db.Configuration                      (readConfiguration,
                                                        toConnection)
import           Db.Instances
import           GHC.Natural                           (Natural)

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
  Primary quizId
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
DbUser
  userName String
  userSalt String
  userHash String
  Primary userName
  UniqueDbUserUserName userName
  deriving Show
|]

runSql :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
runSql action =
  readConfiguration >>= \config ->
    runStderrLoggingT $ withPostgresqlPool (toConnection config) 10 $ liftIO . runSqlPersistMPool action

performMigration :: IO ()
performMigration = runSql (runMigration migrateAll)

insertOrReplace ::
     ( MonadIO m
     , PersistQueryRead backend
     , PersistEntity record
     , PersistEntityBackend record ~ BaseBackend backend
     , PersistStoreWrite backend
     )
  => [record -> Filter record]
  -> record
  -> ReaderT backend m (Key record)
insertOrReplace mkFilters record = do
  r <- selectFirst (map ($ record) mkFilters) []
  case r of
    Nothing -> insert record
    Just s -> fmap (const ek) (repsert ek record)
      where ek = entityKey s

mkFilter :: PersistField key => EntityField record key -> (record -> key) -> record -> Filter record
mkFilter field keyOf record = field ==. keyOf record