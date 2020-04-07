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
import           Database.Persist.Postgresql           (BaseBackend,
                                                        EntityField, Filter,
                                                        Key, PersistEntity,
                                                        PersistEntityBackend,
                                                        PersistField,
                                                        PersistQueryRead,
                                                        PersistStoreWrite,
                                                        SqlBackend, entityKey,
                                                        insert, repsert,
                                                        runMigration,
                                                        runSqlPersistMPool,
                                                        selectFirst,
                                                        withPostgresqlPool,
                                                        (==.))

import           Database.Persist.TH                   (mkMigrate, mkPersist,
                                                        persistLowerCase, share,
                                                        sqlSettings)
import           Db.Configuration                      (readConfiguration,
                                                        toConnection)
import           Db.Instances
import           General.Labels                        (Labels (backToChartView, cumulativeLabel, individualRoundsLabel, maxReachableLabel, maxReachedLabel, ownPageLabel, ownPointsLabel, placeLabel, placementLabel, pointsLabel, progressionLabel, roundLabel, roundWinnerLabel, teamLabel, viewPrevious, placeInRoundLabel, placeAfterRoundLabel),
                                                        mkLabels)
import           General.Types                         (Activity,
                                                        BackToChartViewLabel,
                                                        Code, CumulativeLabel,
                                                        IndividualRoundsLabel,
                                                        MaxReachableLabel,
                                                        MaxReachedLabel,
                                                        NumberOfQuestions,
                                                        OwnPageLabel,
                                                        OwnPointsLabel, Place,
                                                        PlaceAfterRoundLabel,
                                                        PlaceInRoundLabel,
                                                        PlaceLabel,
                                                        PlacementLabel,
                                                        PointsLabel,
                                                        ProgressionLabel,
                                                        QuizDate, QuizName,
                                                        RoundLabel, RoundNumber,
                                                        RoundWinnerLabel,
                                                        TeamLabel, TeamName,
                                                        TeamNumber,
                                                        Wrapped (unwrap, wrap),
                                                        UserHash, UserName,
                                                        UserSalt,
                                                        ViewPreviousLabel)
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
  ownPageLabel String
  viewPrevious String
  cumulativeLabel String
  individualRoundsLabel String
  progressionLabel String
  placementLabel String
  placeLabel String
  pointsLabel String
  roundWinnerLabel String
  placeInRoundLabel String
  placeAfterRoundLabel String
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
DbRoundQuestions
  quizId DbQuizId
  roundNumber Natural
  questions Natural
  Primary quizId roundNumber
  deriving Show
DbUser
  userName String
  userSalt String
  userHash String
  Primary userName
  UniqueDbUserUserName userName
  deriving Show
DbSessionKey
  userName String
  userHash String
  Primary userName
  UniqueDbSessionKeyUserName userName
  deriving Show
|]

mkDbQuiz :: Place -> QuizDate -> QuizName -> Activity -> DbQuiz
mkDbQuiz p qd qn a = DbQuiz (unwrap p) (unwrap qd) (unwrap qn) (unwrap a)

-- todo: This function will become obsolete, once labels are fully typed.
mkDbLabels ::
     DbQuizId
  -> RoundLabel
  -> TeamLabel
  -> OwnPointsLabel
  -> MaxReachedLabel
  -> MaxReachableLabel
  -> BackToChartViewLabel
  -> OwnPageLabel
  -> ViewPreviousLabel
  -> CumulativeLabel
  -> IndividualRoundsLabel
  -> ProgressionLabel
  -> PlacementLabel
  -> PlaceLabel
  -> PointsLabel
  -> RoundWinnerLabel
  -> PlaceInRoundLabel
  -> PlaceAfterRoundLabel
  -> DbLabels
mkDbLabels qid rd t own mr mred btc op vp c ir pr plcmt plc pts rw pird pard =
  DbLabels
    qid
    (unwrap rd)
    (unwrap t)
    (unwrap own)
    (unwrap mr)
    (unwrap mred)
    (unwrap btc)
    (unwrap op)
    (unwrap vp)
    (unwrap c)
    (unwrap ir)
    (unwrap pr)
    (unwrap plcmt)
    (unwrap plc)
    (unwrap pts)
    (unwrap rw)
    (unwrap pird)
    (unwrap pard)

mkDbRoundReachable :: DbQuizId -> RoundNumber -> Double -> DbRoundReachable
mkDbRoundReachable qid rn = DbRoundReachable qid (unwrap rn)

mkDbRoundReached :: DbQuizId -> RoundNumber -> TeamNumber -> Double -> DbRoundReached
mkDbRoundReached qid rn tn = DbRoundReached qid (unwrap rn) (unwrap tn)

mkDbUser :: UserName -> UserSalt -> UserHash -> DbUser
mkDbUser n s h = DbUser (unwrap n) (unwrap s) (unwrap h)

mkRoundQuestions :: DbQuizId -> RoundNumber -> NumberOfQuestions -> DbRoundQuestions
mkRoundQuestions qid rn nq = DbRoundQuestions qid (unwrap rn) (unwrap nq)

dbLabelsToLabels :: DbLabels -> Labels
dbLabelsToLabels dbLabels =
  mkLabels
    (dbLabelsRoundLabel dbLabels)
    (dbLabelsTeamLabel dbLabels)
    (dbLabelsOwnPointsLabel dbLabels)
    (dbLabelsMaxReachedLabel dbLabels)
    (dbLabelsMaxReachableLabel dbLabels)
    (dbLabelsBackToChartView dbLabels)
    (dbLabelsOwnPageLabel dbLabels)
    (dbLabelsViewPrevious dbLabels)
    (dbLabelsCumulativeLabel dbLabels)
    (dbLabelsIndividualRoundsLabel dbLabels)
    (dbLabelsProgressionLabel dbLabels)
    (dbLabelsPlacementLabel dbLabels)
    (dbLabelsPlaceLabel dbLabels)
    (dbLabelsPointsLabel dbLabels)
    (dbLabelsRoundWinnerLabel dbLabels)
    (dbLabelsPlaceInRoundLabel dbLabels)
    (dbLabelsPlaceAfterRoundLabel dbLabels)

labelsToDbLabels :: DbQuizId -> Labels -> DbLabels
labelsToDbLabels qid lbls =
  mkDbLabels
    qid
    (roundLabel lbls)
    (teamLabel lbls)
    (ownPointsLabel lbls)
    (maxReachedLabel lbls)
    (maxReachableLabel lbls)
    (backToChartView lbls)
    (ownPageLabel lbls)
    (viewPrevious lbls)
    (cumulativeLabel lbls)
    (individualRoundsLabel lbls)
    (progressionLabel lbls)
    (placementLabel lbls)
    (placeLabel lbls)
    (pointsLabel lbls)
    (roundWinnerLabel lbls)
    (placeInRoundLabel lbls)
    (placeAfterRoundLabel lbls)

-- todo: either remove logging or pipe it directly into a log file
type Statement m k = ReaderT SqlBackend m k

type ResourceMonad = NoLoggingT (ResourceT IO)

runSql :: Statement ResourceMonad a -> IO a
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
