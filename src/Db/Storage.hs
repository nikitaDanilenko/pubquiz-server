{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Db.Storage where

import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Trans.Reader  (ReaderT)

import           Data.Time.Calendar          (Day)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Db.Connection               (DbLabels (..), DbQuiz (..),
                                              DbQuizId, DbRoundReachable (..),
                                              EntityField (..), insertOrReplace,
                                              mkFilter, runSql)
import           Db.DbTypes                  (Activity, Code, Place, QuizName,
                                              RoundNumber, TeamName, TeamNumber,
                                              unRoundNumber)
import           Labels                      (Labels)
import           Pages.PointComputation      (RoundRating)

setTeamRoundPoints :: DbQuizId -> RoundNumber -> TeamNumber -> Double -> IO ()
setTeamRoundPoints = undefined

setReachable :: DbQuizId -> RoundNumber -> Double -> IO (Key DbRoundReachable)
setReachable qid rn p = undefined --insertOrReplace

setTeam :: DbQuizId -> TeamNumber -> Code -> TeamName -> Activity -> IO ()
setTeam = undefined

setLabels :: DbQuizId -> Labels -> IO ()
setLabels = undefined

createQuiz :: Place -> Day -> QuizName -> IO ()
createQuiz = undefined

lockQuiz :: DbQuizId -> IO ()
lockQuiz = undefined

findAllQuizzes :: IO [DbQuiz]
findAllQuizzes = undefined

findRoundRating :: DbQuizId -> TeamNumber -> RoundNumber -> IO RoundRating
findRoundRating = undefined

-- | Returns the round ratings for a fixed team for the given list of rounds.
--   Each round rating corresponds to a line in the detailed table of points
--   available at the team point page.
findRoundRatings :: DbQuizId -> TeamNumber -> [RoundNumber] -> IO [RoundRating]
findRoundRatings qid tn = mapM (findRoundRating qid tn)

-- | Returns the current point per round progression for each team in the quiz.
--   This is to say that the numbers represent the points in each individual round,
--   rather than the cumulative result.
findAllCurrentPointsPerRound :: DbQuizId -> IO [(RoundNumber, [(TeamNumber, Double)])]
findAllCurrentPointsPerRound = undefined

-- | Returns the cumulative point progression for each team in the quiz.
--   This is to say that each number represents the cumulative points for the given
--   round and team.
--   Usually, this means that the numbers at the same position are non-decreasing.
findAllCurrentPointsPerRoundCumulative :: DbQuizId -> IO [(RoundNumber, [(TeamNumber, Double)])]
findAllCurrentPointsPerRoundCumulative = undefined

-- * Auxiliary functions
repsertQuiz :: MonadIO m => DbQuiz -> ReaderT SqlBackend m (Key DbQuiz)
repsertQuiz =
  insertOrReplace [mkFilter DbQuizPlace dbQuizPlace, mkFilter DbQuizDate dbQuizDate, mkFilter DbQuizName dbQuizName]

repsertLabels :: MonadIO m => DbLabels -> ReaderT SqlBackend m (Key DbLabels)
repsertLabels = insertOrReplace [mkFilter DbLabelsQuizId dbLabelsQuizId]

repsertRoundReached :: MonadIO m => DbRoundReachable -> ReaderT SqlBackend m (Key DbRoundReachable)
repsertRoundReached =
  insertOrReplace
    [ mkFilter DbRoundReachableQuizId dbRoundReachableQuizId
    , mkFilter DbRoundReachableRoundNumber dbRoundReachableRoundNumber
    ]
