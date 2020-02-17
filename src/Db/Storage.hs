{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Db.Storage where

import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Trans.Reader  (ReaderT)

import           Database.Persist            (Entity, Key, checkUnique,
                                              entityVal, insert, selectFirst,
                                              selectList, (==.))
import           Database.Persist.Postgresql (SqlBackend)

import           Data.List                   (intercalate)
import           Data.Time.Calendar          (Day)
import           Db.Connection               (DbLabels (dbLabelsQuizId), DbQuiz (dbQuizDate, dbQuizName, dbQuizPlace),
                                              DbQuizId,
                                              DbRoundReachable (dbRoundReachableQuizId, dbRoundReachableRoundNumber),
                                              DbRoundReached (dbRoundReachedQuizId, dbRoundReachedRoundNumber, dbRoundReachedTeamNumber),
                                              DbTeamNameCode (dbTeamNameCodeQuizId, dbTeamNameCodeTeamNumber),
                                              DbUser (dbUserUserName),
                                              EntityField (..),
                                              dbLabelsToLabels, insertOrReplace,
                                              labelsToDbLabels, mkDbQuiz,
                                              mkDbRoundReachable,
                                              mkDbRoundReached,
                                              mkDbTeamNameCode, mkFilter,
                                              runSql)
import           Db.DbRatings                (Ratings, ratingsFromDb)
import           Db.DbTypes                  (Activity (..), Code, Place,
                                              QuizDate, QuizName, RoundNumber,
                                              TeamName, TeamNumber,
                                              Unwrappable (unwrap, wrap),
                                              fallbackLabels)
import           Labels                      (Labels (..), mkLabels)

type Statement m k = ReaderT SqlBackend m k

setTeamRoundPoints :: DbQuizId -> RoundNumber -> TeamNumber -> Double -> IO (Key DbRoundReached)
setTeamRoundPoints qid rn tn p = runSql (setTeamRoundPointsStatement qid rn tn p)

setTeamRoundPointsStatement ::
     MonadIO m => DbQuizId -> RoundNumber -> TeamNumber -> Double -> Statement m (Key DbRoundReached)
setTeamRoundPointsStatement qid rn tn p = repsertRoundReached (mkDbRoundReached qid rn tn p)

setReachable :: DbQuizId -> RoundNumber -> Double -> IO (Key DbRoundReachable)
setReachable qid rn p = runSql (setReachableStatement qid rn p)

setReachableStatement :: MonadIO m => DbQuizId -> RoundNumber -> Double -> Statement m (Key DbRoundReachable)
setReachableStatement qid rn p = repsertRoundReachable (mkDbRoundReachable qid rn p)

setTeam :: DbQuizId -> TeamNumber -> Code -> TeamName -> Activity -> IO (Key DbTeamNameCode)
setTeam qid tn c tname a = runSql (setTeamStatement qid tn c tname a)

setTeamStatement ::
     MonadIO m => DbQuizId -> TeamNumber -> Code -> TeamName -> Activity -> Statement m (Key DbTeamNameCode)
setTeamStatement qid tn c tname a = repsertTeamNameCode (mkDbTeamNameCode qid tn c tname a)

setLabels :: DbQuizId -> Labels -> IO (Key DbLabels)
setLabels qid lbls = runSql (setLabelsStatement qid lbls)

setLabelsStatement :: MonadIO m => DbQuizId -> Labels -> Statement m (Key DbLabels)
setLabelsStatement qid lbls = repsertLabels (labelsToDbLabels qid lbls)

createQuiz :: Place -> QuizDate -> QuizName -> IO (Key DbQuiz)
createQuiz p d n = runSql (createQuizStatement p d n)

-- | Creates a statement for the creation of a new, hence active, quiz.
--   If a quiz with the same place, date, and time exists,
--   an exception is raised.
createQuizStatement :: MonadIO m => Place -> QuizDate -> QuizName -> Statement m (Key DbQuiz)
createQuizStatement p d n = do
  isUnique <- checkUnique newQuiz
  maybe (insert newQuiz) (const (error errorMsg)) isUnique
  where
    newQuiz = mkDbQuiz p d n Active
    errorMsg =
      unwords
        [ "Quiz with"
        , intercalate
            ","
            (zipWith (\k v -> concat [k, "=", v]) ["place", "date", "name"] [unwrap p, show (unwrap d :: Day), unwrap n])
        , "already exists."
        ]

lockQuiz :: Place -> QuizDate -> QuizName -> IO (Key DbQuiz)
lockQuiz p d n = runSql (lockQuizStatement p d n)

lockQuizStatement :: MonadIO m => Place -> QuizDate -> QuizName -> Statement m (Key DbQuiz)
lockQuizStatement p d n = repsertQuiz (mkDbQuiz p d n Inactive)

findAllQuizzes :: IO [Entity DbQuiz]
findAllQuizzes = runSql findAllQuizzesStatement

findAllQuizzesStatement :: MonadIO m => Statement m [Entity DbQuiz]
findAllQuizzesStatement = selectList [] []

findRatings :: DbQuizId -> IO Ratings
findRatings = runSql . findRatingsStatement

findRatingsStatement :: MonadIO m => DbQuizId -> Statement m Ratings
findRatingsStatement qid = do
  reachables <- selectList [DbRoundReachableQuizId ==. qid] []
  reacheds <- selectList [DbRoundReachedQuizId ==. qid] []
  return (ratingsFromDb (map entityVal reachables) (map entityVal reacheds))

findLabels :: DbQuizId -> IO Labels
findLabels = runSql . findLabelsStatement

findLabelsStatement :: MonadIO m => DbQuizId -> Statement m Labels
findLabelsStatement qid =
  fmap (maybe fallbackLabels (dbLabelsToLabels . entityVal)) (selectFirst [DbLabelsQuizId ==. qid] [])

-- * Auxiliary functions
repsertQuiz :: MonadIO m => DbQuiz -> Statement m (Key DbQuiz)
repsertQuiz =
  insertOrReplace [mkFilter DbQuizPlace dbQuizPlace, mkFilter DbQuizDate dbQuizDate, mkFilter DbQuizName dbQuizName]

repsertLabels :: MonadIO m => DbLabels -> Statement m (Key DbLabels)
repsertLabels = insertOrReplace [mkFilter DbLabelsQuizId dbLabelsQuizId]

repsertTeamNameCode :: MonadIO m => DbTeamNameCode -> Statement m (Key DbTeamNameCode)
repsertTeamNameCode =
  insertOrReplace
    [mkFilter DbTeamNameCodeQuizId dbTeamNameCodeQuizId, mkFilter DbTeamNameCodeTeamNumber dbTeamNameCodeTeamNumber]

repsertRoundReachable :: MonadIO m => DbRoundReachable -> Statement m (Key DbRoundReachable)
repsertRoundReachable =
  insertOrReplace
    [ mkFilter DbRoundReachableQuizId dbRoundReachableQuizId
    , mkFilter DbRoundReachableRoundNumber dbRoundReachableRoundNumber
    ]

repsertRoundReached :: MonadIO m => DbRoundReached -> Statement m (Key DbRoundReached)
repsertRoundReached =
  insertOrReplace
    [ mkFilter DbRoundReachedQuizId dbRoundReachedQuizId
    , mkFilter DbRoundReachedRoundNumber dbRoundReachedRoundNumber
    , mkFilter DbRoundReachedTeamNumber dbRoundReachedTeamNumber
    ]

repsertUser :: MonadIO m => DbUser -> Statement m (Key DbUser)
repsertUser = insertOrReplace [mkFilter DbUserUserName dbUserUserName]
