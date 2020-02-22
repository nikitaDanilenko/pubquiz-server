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
import qualified Data.Text                   as T
import           Data.Time.Calendar          (Day)
import           Db.Connection               (DbLabels (dbLabelsQuizId), DbQuiz (dbQuizDate, dbQuizName, dbQuizPlace),
                                              DbQuizId,
                                              DbRoundReachable (dbRoundReachableQuizId, dbRoundReachableRoundNumber),
                                              DbRoundReached (dbRoundReachedQuizId, dbRoundReachedRoundNumber, dbRoundReachedTeamNumber),
                                              DbSessionKey (DbSessionKey),
                                              DbTeamNameCode (dbTeamNameCodeQuizId, dbTeamNameCodeTeamNumber),
                                              DbUser (DbUser, dbUserUserName),
                                              EntityField (..),
                                              dbLabelsToLabels,
                                              dbSessionKeyUserHash,
                                              dbSessionKeyUserName,
                                              dbUserUserHash, dbUserUserSalt,
                                              insertOrReplace, labelsToDbLabels,
                                              mkDbQuiz, mkDbRoundReachable,
                                              mkDbRoundReached,
                                              mkDbTeamNameCode, mkFilter,
                                              runSql, DbQuiz)
import           Db.DbConversion             (QuizInfo,
                                              QuizPDN (date, name, place),
                                              Ratings, SavedUser,
                                              dbUserToSavedUser, ratingsFromDb,
                                              savedUserToDbUser, userHash,
                                              userName, userSalt, mkQuizInfo)
import           General.Labels              (Labels (..), fallbackLabels,
                                              mkLabels)
import           General.Types               (Activity (..), Code, Place,
                                              QuizDate, QuizName, RoundNumber,
                                              TeamName, TeamNumber,
                                              Unwrappable (unwrap, wrap),
                                              UserHash, UserName, UserSalt)

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

setSessionKey :: UserName -> UserHash -> IO (Key DbSessionKey)
setSessionKey un uh = runSql (setSessionKeyStatement un uh)

setSessionKeyStatement :: MonadIO m => UserName -> UserHash -> Statement m (Key DbSessionKey)
setSessionKeyStatement un uh = repsertSessionKey (DbSessionKey (unwrap un) (unwrap uh))

setUser :: SavedUser -> IO (Key DbUser)
setUser = runSql . setUserStatement

setUserStatement :: MonadIO m => SavedUser -> Statement m (Key DbUser)
setUserStatement = repsertUser . savedUserToDbUser

createQuiz :: QuizPDN -> IO (Key DbQuiz)
createQuiz = runSql . createQuizStatement

-- | Creates a statement for the creation of a new, hence active, quiz.
--   If a quiz with the same place, date, and time exists,
--   an exception is raised.
createQuizStatement :: MonadIO m => QuizPDN -> Statement m (Key DbQuiz)
createQuizStatement ndp = do
  isUnique <- checkUnique newQuiz
  maybe (insert newQuiz) (const (error errorMsg)) isUnique
  where
    newQuiz = mkDbQuiz p d n Active
    errorMsg =
      unwords
        [ "Quiz with"
        , intercalate
            ","
            (zipWith
               (\k v -> concat [k, "=", v])
               ["place", "date", "name"]
               [T.unpack (unwrap p), show (unwrap d :: Day), T.unpack (unwrap n)])
        , "already exists."
        ]
    (p, d, n) = (place ndp, date ndp, name ndp)

lockQuiz :: Place -> QuizDate -> QuizName -> IO (Key DbQuiz)
lockQuiz p d n = runSql (lockQuizStatement p d n)

lockQuizStatement :: MonadIO m => Place -> QuizDate -> QuizName -> Statement m (Key DbQuiz)
lockQuizStatement p d n = repsertQuiz (mkDbQuiz p d n Inactive)

findAllActiveQuizzes :: IO [QuizInfo]
findAllActiveQuizzes = runSql findAllActiveQuizzesStatement

findAllActiveQuizzesStatement :: MonadIO m => Statement m [QuizInfo]
findAllActiveQuizzesStatement = fmap (fmap mkQuizInfo) (selectList [DbQuizActive ==. True] [])

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

findUser :: UserName -> IO (Maybe SavedUser)
findUser = runSql . findUserStatement

findUserStatement :: MonadIO m => UserName -> Statement m (Maybe SavedUser)
findUserStatement userName =
  fmap (fmap (dbUserToSavedUser . entityVal)) (selectFirst [DbUserUserName ==. unwrap userName] [])

findSessionKey :: UserName -> IO (Maybe UserHash)
findSessionKey = runSql . findSessionKeyStatement

findSessionKeyStatement :: MonadIO m => UserName -> Statement m (Maybe UserHash)
findSessionKeyStatement un =
  fmap (fmap (wrap . dbSessionKeyUserHash . entityVal)) (selectFirst [DbSessionKeyUserName ==. unwrap un] [])

findQuizInfo :: DbQuizId -> IO (Maybe QuizInfo)
findQuizInfo = runSql . findQuizInfoStatement

findQuizInfoStatement :: MonadIO m => DbQuizId -> Statement m (Maybe QuizInfo)
findQuizInfoStatement qid = fmap (fmap mkQuizInfo) (selectFirst [DbQuizId ==. qid] [])

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

repsertSessionKey :: MonadIO m => DbSessionKey -> Statement m (Key DbSessionKey)
repsertSessionKey = insertOrReplace [mkFilter DbSessionKeyUserName dbSessionKeyUserName]
