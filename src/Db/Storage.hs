{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Db.Storage where

import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Trans.Reader  (ReaderT)

import           Database.Persist            (Entity (Entity), Key,
                                              SelectOpt (Asc), checkUnique,
                                              entityVal, insert, selectFirst,
                                              selectList, update, (=.), (==.))
import           Database.Persist.Postgresql (SqlBackend)

import           Constants                   (serverSheetsFolderIO)
import           Control.Applicative         (liftA2, liftA3)
import           Data.Functor                (void)
import           Data.List                   (intercalate)
import           Data.Maybe                  (isJust)
import qualified Data.Text                   as T
import           Data.Time.Calendar          (Day)
import           Db.Connection               (DbLabels (dbLabelsQuizId), DbQuiz (dbQuizDate, dbQuizName, dbQuizPlace),
                                              DbQuiz (DbQuiz), DbQuizId,
                                              DbRoundQuestions,
                                              DbRoundReachable (dbRoundReachableQuizId, dbRoundReachableRoundNumber),
                                              DbRoundReached (dbRoundReachedQuizId, dbRoundReachedRoundNumber, dbRoundReachedTeamNumber),
                                              DbSessionKey (DbSessionKey),
                                              DbTeamNameCode (dbTeamNameCodeQuizId, dbTeamNameCodeTeamNumber),
                                              DbUser (DbUser, dbUserUserName),
                                              EntityField (..), Statement,
                                              dbLabelsToLabels,
                                              dbRoundQuestionsQuestions,
                                              dbRoundQuestionsQuizId,
                                              dbRoundQuestionsRoundNumber,
                                              dbSessionKeyUserHash,
                                              dbSessionKeyUserName,
                                              dbUserUserHash, dbUserUserSalt,
                                              insertOrReplace, labelsToDbLabels,
                                              mkDbQuiz, mkDbRoundReachable,
                                              mkDbRoundReached, mkFilter,
                                              mkRoundQuestions, runSql)
import           Db.DbConversion             (Header,
                                              QuestionsInQuiz (QuestionsInQuiz),
                                              QuestionsInRound (QuestionsInRound),
                                              QuizIdentifier (date, name, place),
                                              QuizInfo,
                                              QuizRatings (QuizRatings, header, ratings),
                                              QuizSettings, Ratings,
                                              RoundRating (points, reachableInRound),
                                              SavedUser, TeamInfo, TeamQuery,
                                              TeamRating (TeamRating, rating, teamNumber),
                                              TeamTable,
                                              TeamTableInfo (TeamTableInfo),
                                              dbRoundQuestionsToQuestionsInRound,
                                              dbTeamNameCodeToTeamInfo,
                                              dbUserToSavedUser, mkQuizInfo,
                                              mkQuizSettings, mkTeamTable,
                                              questionsInRoundNumberOfQuestions,
                                              questionsInRoundRoundNumber,
                                              ratingsFromDb, savedUserToDbUser,
                                              teamInfoName, teamInfoNumber,
                                              teamInfoToDbTeamNameCode,
                                              teamQueryQuizId,
                                              teamQueryTeamCode,
                                              teamQueryTeamNumber, userHash,
                                              userName, userSalt)
import           General.Labels              (Labels (..), mkLabels)
import           General.Types               (Activity (..), Code,
                                              NumberOfQuestions, Place,
                                              QuizDate, QuizName, RoundNumber,
                                              TeamName, TeamNumber,
                                              Unwrappable (unwrap, wrap),
                                              UserHash, UserName, UserSalt,
                                              fallback)
import           GHC.Natural                 (intToNatural)

setTeamRating :: DbQuizId -> RoundNumber -> TeamRating -> IO (Key DbRoundReached)
setTeamRating qid rn tr = runSql (setTeamRatingStatement qid rn tr)

setTeamRatingStatement :: MonadIO m => DbQuizId -> RoundNumber -> TeamRating -> Statement m (Key DbRoundReached)
setTeamRatingStatement qid rn tr = repsertRoundReached (mkDbRoundReached qid rn (teamNumber tr) (rating tr))

setReachable :: DbQuizId -> RoundNumber -> Double -> IO (Key DbRoundReachable)
setReachable qid rn p = runSql (setReachableStatement qid rn p)

setReachableStatement :: MonadIO m => DbQuizId -> RoundNumber -> Double -> Statement m (Key DbRoundReachable)
setReachableStatement qid rn p = repsertRoundReachable (mkDbRoundReachable qid rn p)

setTeamInfo :: DbQuizId -> TeamInfo -> IO (Key DbTeamNameCode)
setTeamInfo qid ti = runSql (setTeamInfoStatement qid ti)

setTeamInfoStatement :: MonadIO m => DbQuizId -> TeamInfo -> Statement m (Key DbTeamNameCode)
setTeamInfoStatement qid ti = repsertTeamNameCode (teamInfoToDbTeamNameCode qid ti)

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

setRatings :: DbQuizId -> Ratings -> IO ()
setRatings qid rs = runSql (setRatingsStatement qid rs)

setRatingsStatement :: MonadIO m => DbQuizId -> Ratings -> Statement m ()
setRatingsStatement qid ratings =
  mapM_ (uncurry (setRoundRatingStatement qid)) (unwrap ratings :: [(RoundNumber, RoundRating)])
  where
    setRoundRatingStatement qid rd rr = do
      setReachableStatement qid rd (reachableInRound rr)
      mapM_ (setTeamRatingStatement qid rd) (points rr)

setHeader :: DbQuizId -> Header -> IO ()
setHeader qid header = runSql (setHeaderStatement qid header)

setHeaderStatement :: MonadIO m => DbQuizId -> Header -> Statement m ()
setHeaderStatement qid header = mapM_ (setTeamInfoStatement qid) (unwrap header :: [TeamInfo])

setQuizRatings :: DbQuizId -> QuizRatings -> IO ()
setQuizRatings qid quizRatings = runSql (setQuizRatingsStatement qid quizRatings)

setQuizRatingsStatement :: MonadIO m => DbQuizId -> QuizRatings -> Statement m ()
setQuizRatingsStatement qid quizRatings = do
  setHeaderStatement qid (header quizRatings)
  setRatingsStatement qid (ratings quizRatings)

setQuizIdentifier :: DbQuizId -> QuizIdentifier -> IO DbQuizId
setQuizIdentifier qid quizIdentifier = runSql (setQuizIdentifierStatement qid quizIdentifier)

setQuizIdentifierStatement :: MonadIO m => DbQuizId -> QuizIdentifier -> Statement m DbQuizId
setQuizIdentifierStatement qid quizIdentifier =
  repsertQuiz (mkDbQuiz (place quizIdentifier) (date quizIdentifier) (name quizIdentifier) Active)

setRoundQuestions :: DbQuizId -> RoundNumber -> NumberOfQuestions -> IO (Key DbRoundQuestions)
setRoundQuestions qid rn nq = runSql (setRoundQuestionsStatement qid rn nq)

setRoundQuestionsStatement ::
     MonadIO m => DbQuizId -> RoundNumber -> NumberOfQuestions -> Statement m (Key DbRoundQuestions)
setRoundQuestionsStatement qid rn nq = repsertRoundQuestions (mkRoundQuestions qid rn nq)

setQuestionsInQuiz :: DbQuizId -> QuestionsInQuiz -> IO ()
setQuestionsInQuiz qid inQuiz = runSql (setQuestionsInQuizStatement qid inQuiz)

setQuestionsInQuizStatement :: MonadIO m => DbQuizId -> QuestionsInQuiz -> Statement m ()
setQuestionsInQuizStatement qid inQuiz =
  mapM_
    (\qir -> setRoundQuestionsStatement qid (questionsInRoundRoundNumber qir) (questionsInRoundNumberOfQuestions qir))
    (unwrap inQuiz :: [QuestionsInRound])

setMissingTeamRatingsToZero :: DbQuizId -> IO ()
setMissingTeamRatingsToZero = runSql . setMissingTeamRatingsToZeroStatement

setMissingTeamRatingsToZeroStatement :: MonadIO m => DbQuizId -> Statement m ()
setMissingTeamRatingsToZeroStatement qid = do
  teamNumbers <-
    fmap
      (fmap (wrap . dbTeamNameCodeTeamNumber . entityVal))
      (selectList [DbTeamNameCodeQuizId ==. qid] [Asc DbTeamNameCodeTeamNumber])
  roundNumbers <-
    fmap
      (fmap (wrap . dbRoundReachableRoundNumber . entityVal))
      (selectList [DbRoundReachableQuizId ==. qid] [Asc DbRoundReachableRoundNumber])
  mapM_
    (uncurry (initialiseWithZero qid))
    [(teamNumber, roundNumber) | teamNumber <- teamNumbers, roundNumber <- roundNumbers]
  where
    initialiseWithZero :: MonadIO m => DbQuizId -> TeamNumber -> RoundNumber -> Statement m ()
    initialiseWithZero qid teamNumber roundNumber = do
      points <-
        selectFirst
          [ DbRoundReachedQuizId ==. qid
          , DbRoundReachedRoundNumber ==. unwrap roundNumber
          , DbRoundReachedTeamNumber ==. unwrap teamNumber
          ]
          []
      case points of
        Nothing -> void (setTeamRatingStatement qid roundNumber (TeamRating {teamNumber = teamNumber, rating = 0}))
        Just _ -> pure ()

createQuiz :: QuizIdentifier -> IO QuizInfo
createQuiz = runSql . createQuizStatement

-- | Creates a statement for the creation of a new, hence active, quiz.
--   If a quiz with the same place, date, and time exists,
--   an exception is raised.
createQuizStatement :: MonadIO m => QuizIdentifier -> Statement m QuizInfo
createQuizStatement identifier = checkUnique newQuiz >>= maybe success (const (error errorMsg))
  where
    success = do
      sheetsFolder <- liftIO serverSheetsFolderIO
      fmap (mkQuizInfo sheetsFolder . flip Entity newQuiz) (insert newQuiz)
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
    (p, d, n) = (place identifier, date identifier, name identifier)

lockQuiz :: DbQuizId -> IO ()
lockQuiz = runSql . lockQuizStatement

lockQuizStatement :: MonadIO m => DbQuizId -> Statement m ()
lockQuizStatement qid = do
  mQuiz <- selectFirst [DbQuizId ==. qid] []
  case mQuiz of
    Nothing -> pure ()
    Just _  -> update qid [DbQuizActive =. False]

findAllActiveQuizzes :: IO [QuizInfo]
findAllActiveQuizzes = runSql findAllActiveQuizzesStatement

findAllActiveQuizzesStatement :: MonadIO m => Statement m [QuizInfo]
findAllActiveQuizzesStatement = do
  sheetsFolder <- liftIO serverSheetsFolderIO
  fmap (fmap (mkQuizInfo sheetsFolder)) (selectList [DbQuizActive ==. True] [])

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
  fmap (maybe (fallback :: Labels) (dbLabelsToLabels . entityVal)) (selectFirst [DbLabelsQuizId ==. qid] [])

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
findQuizInfoStatement qid = do
  sheetsFolder <- liftIO serverSheetsFolderIO
  fmap (fmap (mkQuizInfo sheetsFolder)) (selectFirst [DbQuizId ==. qid] [])

findQuizRatings :: DbQuizId -> IO QuizRatings
findQuizRatings = runSql . findQuizRatingsStatement

findQuizRatingsStatement :: MonadIO m => DbQuizId -> Statement m QuizRatings
findQuizRatingsStatement qid = liftA2 QuizRatings (findHeaderStatement qid) (findRatingsStatement qid)

findHeader :: DbQuizId -> IO Header
findHeader = runSql . findHeaderStatement

findHeaderStatement :: MonadIO m => DbQuizId -> Statement m Header
findHeaderStatement qid =
  fmap (wrap . fmap (dbTeamNameCodeToTeamInfo . entityVal)) (selectList [DbTeamNameCodeQuizId ==. qid] [])

findTeamTable :: DbQuizId -> TeamNumber -> IO TeamTable
findTeamTable qid tn = runSql (findTeamTableStatement qid tn)

findTeamTableStatement :: MonadIO m => DbQuizId -> TeamNumber -> Statement m TeamTable
findTeamTableStatement qid tn =
  liftA3
    mkTeamTable
    (selectList [DbRoundReachedQuizId ==. qid, DbRoundReachedTeamNumber ==. unwrap tn] [])
    (selectList [DbRoundReachableQuizId ==. qid] [])
    (selectList [DbRoundReachedQuizId ==. qid] [])

findTeamTableInfoStatement :: MonadIO m => DbQuizId -> TeamNumber -> Statement m TeamTableInfo
findTeamTableInfoStatement qid tn = do
  header <- findHeaderStatement qid
  teamTable <- findTeamTableStatement qid tn
  let unwrappedHeader = unwrap header
      name = teamInfoName (head (filter (\ti -> teamInfoNumber ti == tn) unwrappedHeader))
  pure (TeamTableInfo teamTable name (intToNatural (length unwrappedHeader)) tn)

findTeamTableInfo :: DbQuizId -> TeamNumber -> IO TeamTableInfo
findTeamTableInfo qid tn = runSql (findTeamTableInfoStatement qid tn)

findQuestionsInQuiz :: DbQuizId -> IO QuestionsInQuiz
findQuestionsInQuiz = runSql . findQuestionsInQuizStatement

findQuestionsInQuizStatement :: MonadIO m => DbQuizId -> Statement m QuestionsInQuiz
findQuestionsInQuizStatement qid =
  fmap
    (QuestionsInQuiz . map (dbRoundQuestionsToQuestionsInRound . entityVal))
    (selectList [DbRoundQuestionsQuizId ==. qid] [])

findQuizSettings :: DbQuizId -> IO QuizSettings
findQuizSettings = runSql . findQuizSettingsStatement

findQuizSettingsStatement :: MonadIO m => DbQuizId -> Statement m QuizSettings
findQuizSettingsStatement qid =
  liftA3 mkQuizSettings (findQuestionsInQuizStatement qid) (findHeaderStatement qid) (findLabelsStatement qid)

existsTeam :: TeamQuery -> IO Bool
existsTeam = runSql . existsTeamStatement

existsTeamStatement :: MonadIO m => TeamQuery -> Statement m Bool
existsTeamStatement tq =
  fmap
    isJust
    (selectFirst
       [ DbTeamNameCodeQuizId ==. teamQueryQuizId tq
       , DbTeamNameCodeTeamNumber ==. unwrap (teamQueryTeamNumber tq)
       , DbTeamNameCodeTeamCode ==. unwrap (teamQueryTeamCode tq)
       ]
       [])

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

repsertRoundQuestions :: MonadIO m => DbRoundQuestions -> Statement m (Key DbRoundQuestions)
repsertRoundQuestions =
  insertOrReplace
    [ mkFilter DbRoundQuestionsQuizId dbRoundQuestionsQuizId
    , mkFilter DbRoundQuestionsRoundNumber dbRoundQuestionsRoundNumber
    ]
