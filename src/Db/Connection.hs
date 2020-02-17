{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE ExplicitForAll             #-}
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
import           Db.DbTypes                            (Activity,
                                                        BackToChartViewLabel,
                                                        Code, CumulativeLabel,
                                                        Fallback (fallback),
                                                        IndividualRoundsLabel,
                                                        MainLabel,
                                                        MaxReachableLabel,
                                                        MaxReachedLabel,
                                                        OwnPageLabel,
                                                        OwnPointsLabel, Place,
                                                        PlaceLabel,
                                                        PlacementLabel,
                                                        PointsLabel,
                                                        ProgressionLabel,
                                                        QuizDate, QuizName,
                                                        RoundLabel, RoundNumber,
                                                        RoundWinnerLabel,
                                                        TeamLabel, TeamName,
                                                        TeamNumber,
                                                        Unwrappable (unwrap),
                                                        UserHash, UserName,
                                                        UserSalt,
                                                        ViewPreviousLabel)
import           Db.Instances
import           GHC.Natural                           (Natural)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
DbQuiz
  place String
  date Day
  name String
  active Bool
  UniqueDbQuiz place date name
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

mkDbQuiz :: Place -> QuizDate -> QuizName -> Activity -> DbQuiz
mkDbQuiz p qd qn a = DbQuiz (unwrap p) (unwrap qd) (unwrap qn) (unwrap a)

mkDbLabels ::
     DbQuizId
  -> RoundLabel
  -> TeamLabel
  -> OwnPointsLabel
  -> MaxReachedLabel
  -> MaxReachableLabel
  -> BackToChartViewLabel
  -> MainLabel
  -> OwnPageLabel
  -> ViewPreviousLabel
  -> CumulativeLabel
  -> IndividualRoundsLabel
  -> ProgressionLabel
  -> PlacementLabel
  -> PlaceLabel
  -> PointsLabel
  -> RoundWinnerLabel
  -> DbLabels
mkDbLabels qid rd t own mr mred btc m op vp c ir pr plcmt plc pts rw =
  DbLabels
    qid
    (unwrap rd)
    (unwrap t)
    (unwrap own)
    (unwrap mr)
    (unwrap mred)
    (unwrap btc)
    (unwrap m)
    (unwrap op)
    (unwrap vp)
    (unwrap c)
    (unwrap ir)
    (unwrap pr)
    (unwrap plcmt)
    (unwrap plc)
    (unwrap pts)
    (unwrap rw)

mkFallbackLabels :: DbQuizId -> DbLabels
mkFallbackLabels qid = 
  DbLabels
    qid
    (unwrap (fallback :: RoundLabel))
    (unwrap (fallback :: TeamLabel))
    (unwrap (fallback :: OwnPointsLabel))
    (unwrap (fallback :: MaxReachedLabel))
    (unwrap (fallback :: MaxReachableLabel))
    (unwrap (fallback :: BackToChartViewLabel))
    (unwrap (fallback :: MainLabel))
    (unwrap (fallback :: OwnPageLabel))
    (unwrap (fallback :: ViewPreviousLabel))
    (unwrap (fallback :: CumulativeLabel))
    (unwrap (fallback :: IndividualRoundsLabel))
    (unwrap (fallback :: ProgressionLabel))
    (unwrap (fallback :: PlacementLabel))
    (unwrap (fallback :: PlaceLabel))
    (unwrap (fallback :: PointsLabel))
    (unwrap (fallback :: RoundWinnerLabel))

mkDbTeamNameCode :: DbQuizId -> TeamNumber -> Code -> TeamName -> Activity -> DbTeamNameCode
mkDbTeamNameCode qid tn c tl a = DbTeamNameCode qid (unwrap tn) (unwrap c) (unwrap tl) (unwrap a)

mkDbRoundReachable :: DbQuizId -> RoundNumber -> Double -> DbRoundReachable
mkDbRoundReachable qid rn = DbRoundReachable qid (unwrap rn)

mkDbRoundReached :: DbQuizId -> RoundNumber -> TeamNumber -> Double -> DbRoundReached
mkDbRoundReached qid rn tn = DbRoundReached qid (unwrap rn) (unwrap tn)

mkDbUser :: UserName -> UserSalt -> UserHash -> DbUser
mkDbUser n s h = DbUser (unwrap n) (unwrap s) (unwrap h)

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
