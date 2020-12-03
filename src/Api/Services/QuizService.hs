{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Services.QuizService
  ( quizServiceInit
  , QuizService
  ) where

import           Api.Requests.CreateQuizRequest        (CreateQuizRequest (CreateQuizRequest))
import           Api.Requests.QuizIdRequest            (quizIdRequestQuizId)
import           Api.Requests.QuizUpdateRequest        (QuizUpdateRequest (QuizUpdateRequest))
import           Api.Requests.UpdateQuizRatingsRequest (UpdateQuizRatingsRequest (UpdateQuizRatingsRequest))
import           Api.Services.SnapUtil                 (anyResponseCode,
                                                        errorWithCode,
                                                        okJsonResponse,
                                                        readBody, readVerified)
import           Constants                             (allActiveApi, allApi,
                                                        getLabelsApi,
                                                        getQuizInfoApi,
                                                        getQuizRatingsApi,
                                                        getQuizSettingsApi,
                                                        lockApi, newApi,
                                                        quizPath, serverPathIO,
                                                        serverQuizzesFolderIO,
                                                        teamTableApi,
                                                        updateQuizApi,
                                                        updateQuizRatingsApi)
import           Control.Arrow                         ((&&&))
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Trans.Except            (ExceptT (ExceptT))
import qualified Data.ByteString.Char8                 as B
import           Data.Maybe                            (maybe)
import qualified Data.Text                             as T (unpack)
import           Db.Connection                         (DbQuizId, runSql)
import           Db.DbConversion                       (Credentials, Header,
                                                        QuizIdentifier,
                                                        QuizInfo, QuizRatings,
                                                        QuizSettings,
                                                        TeamInfo (teamInfoCode, teamInfoNumber),
                                                        active,
                                                        adjustHeaderToSize,
                                                        date, fullSheetPath,
                                                        labels,
                                                        mkDefaultTeamInfos,
                                                        numberOfTeams,
                                                        qrOnlyPath,
                                                        questionsInQuiz, quizId,
                                                        teamQueryQuizId,
                                                        teamQueryTeamNumber)
import           Db.Storage                            (QuizInfoSheetType (Download, LocalFile),
                                                        createQuizStatement,
                                                        findAllActiveQuizzes,
                                                        findAllQuizzes,
                                                        findHeaderStatement,
                                                        findLabels,
                                                        findQuizInfo,
                                                        findQuizRatings,
                                                        findQuizSettings,
                                                        findTeamTableInfo,
                                                        lockQuiz,
                                                        setHeaderStatement,
                                                        setLabelsStatement,
                                                        setMissingTeamRatingsToZeroStatement,
                                                        setQuestionsInQuizStatement,
                                                        setQuizIdentifierStatement,
                                                        setQuizRatings)
import           General.EitherT.Extra                 (exceptFromMaybeF,
                                                        exceptValueOr)
import           General.Labels                        (teamLabel)
import           General.Types                         (Activity (Active, Inactive),
                                                        Wrapped (unwrap), wrap)
import           GHC.Natural                           (naturalToInt)
import           Sheet.SheetMaker                      (createSheetWith,
                                                        safeRemoveFile)
import           Snap.Core                             (Method (GET, POST),
                                                        method)
import           Snap.Snaplet                          (Handler, SnapletInit,
                                                        addRoutes, makeSnaplet)
import           Utils                                 (randomDistinctHexadecimal,
                                                        (+>))

data QuizService =
  QuizService

quizRoutes :: [(B.ByteString, Handler b QuizService ())]
quizRoutes =
  [ allActiveApi +> method GET sendAvailableActiveHandler
  , allApi +> method GET sendAllAvailableHandler
  , getQuizRatingsApi +> method GET getQuizRatingsHandler
  , getLabelsApi +> method GET getLabelsHandler
  , updateQuizApi +> method POST updateQuizHandler
  , updateQuizRatingsApi +> method POST updateQuizRatingsHandler
  , lockApi +> method POST lockHandler
  , newApi +> method POST newQuiz
  , teamTableApi +> method GET teamTableInfoHandler
  , getQuizInfoApi +> method GET quizInfoHandler
  , getQuizSettingsApi +> method GET quizSettingsHandler
  ]

sendAvailableActiveHandler :: Handler b QuizService ()
sendAvailableActiveHandler = sendAvailableWith findAllActiveQuizzes

sendAllAvailableHandler :: Handler b QuizService ()
sendAllAvailableHandler = sendAvailableWith findAllQuizzes

sendAvailableWith :: IO [QuizInfo] -> Handler b QuizService ()
sendAvailableWith activeInfoListIO = do
  quizInfoList <- liftIO activeInfoListIO
  okJsonResponse quizInfoList

getQuizRatingsHandler :: Handler b QuizService ()
getQuizRatingsHandler = exceptValueOr transformer (errorWithCode 400)
  where
    transformer = do
      quizIdRequest <- readBody
      quizRatings <- liftIO (findQuizRatings (quizIdRequestQuizId quizIdRequest))
      okJsonResponse quizRatings

getLabelsHandler :: Handler b QuizService ()
getLabelsHandler = exceptValueOr transformer (errorWithCode 400)
  where
    transformer = do
      quizIdRequest <- readBody
      labels <- liftIO (findLabels (quizIdRequestQuizId quizIdRequest))
      okJsonResponse labels

updateQuizRatingsHandler :: Handler b QuizService ()
updateQuizRatingsHandler = exceptValueOr transformer (errorWithCode 406)
  where
    transformer = do
      updateQuizRatingsRequest <- readVerified
      liftIO (updateQuizData updateQuizRatingsRequest)
      anyResponseCode 200

updateQuizData :: UpdateQuizRatingsRequest -> IO ()
updateQuizData (UpdateQuizRatingsRequest qid quizRatings) =
  ifActiveDo Download qid (pure ()) (\_ -> setQuizRatings qid quizRatings)

updateQuizHandler :: Handler b QuizService ()
updateQuizHandler = exceptValueOr transformer (errorWithCode 500)
  where
    transformer = do
      quizUpdateRequest <- readVerified
      liftIO (updateIdentifierAndSettings quizUpdateRequest)
      anyResponseCode 200

teamCodeLength :: Int
teamCodeLength = 6

newQuiz :: Handler b QuizService ()
newQuiz = exceptValueOr transformer (errorWithCode 406)
  where
    transformer = do
      CreateQuizRequest quizIdentifier quizSettings  <- readVerified
      let gs = numberOfTeams quizSettings
      endings <- liftIO (randomDistinctHexadecimal (naturalToInt gs) teamCodeLength)
      let header = wrap (mkDefaultTeamInfos 1 (teamLabel (labels quizSettings)) endings)
      quizInfo <-
        liftIO $
        runSql $ do
          newQuizInfo <- createQuizStatement quizIdentifier
          let qid = quizId newQuizInfo
          setHeaderStatement qid header
          setQuestionsInQuizStatement qid (questionsInQuiz quizSettings)
          pure newQuizInfo
      liftIO (createSheetWithSettings (quizId quizInfo) quizIdentifier quizSettings header)
      okJsonResponse quizInfo

updateIdentifierAndSettings :: QuizUpdateRequest -> IO ()
updateIdentifierAndSettings (QuizUpdateRequest qid idf quizSettings) =
  ifActiveDo LocalFile qid (pure ()) $ \quizInfo ->
    runSql $ do
      setQuizIdentifierStatement qid idf
      let ls = labels quizSettings
      header <- findHeaderStatement qid
      setLabelsStatement qid ls
      adjustedHeader <- liftIO (adjustHeaderToSize (numberOfTeams quizSettings) teamCodeLength (teamLabel ls) header)
      setHeaderStatement qid adjustedHeader
      setMissingTeamRatingsToZeroStatement qid
      setQuestionsInQuizStatement qid (questionsInQuiz quizSettings)
      liftIO (removeFiles quizInfo)
      liftIO (createSheetWithSettings qid idf quizSettings adjustedHeader)

createSheetWithSettings :: DbQuizId -> QuizIdentifier -> QuizSettings -> Header -> IO ()
createSheetWithSettings qid identifier quizSettings header = do
  serverPath <- serverPathIO
  serverFolder <- serverQuizzesFolderIO
  createSheetWith
    (unwrap (teamLabel (labels quizSettings)))
    (questionsInQuiz quizSettings)
    serverPath
    serverFolder
    (map (teamInfoNumber &&& teamInfoCode) (unwrap header))
    (unwrap (date identifier))
    qid

ifActiveDo :: QuizInfoSheetType -> DbQuizId -> IO a -> (QuizInfo -> IO a) -> IO a
ifActiveDo qist qid dft action = findQuizInfo qist qid >>= maybe dft checkActive
  where
    checkActive quizInfo =
      case active quizInfo of
        Active   -> action quizInfo
        Inactive -> dft

lockHandler :: Handler b QuizService ()
lockHandler = exceptValueOr transformer (errorWithCode 406)
  where
    transformer = do
      quizIdRequest <- readVerified
      let qid = quizIdRequestQuizId quizIdRequest
      quizInfo <- exceptFromMaybeF (liftIO (findQuizInfo LocalFile qid)) "No quiz with given id."
      liftIO (lockQuiz qid)
      liftIO (removeFiles quizInfo)
      anyResponseCode 201

removeFiles :: QuizInfo -> IO ()
removeFiles quizInfo = mapM_ (safeRemoveFile . T.unpack) [fullSheetPath quizInfo, qrOnlyPath quizInfo]

teamTableInfoHandler :: Handler b QuizService ()
teamTableInfoHandler = exceptValueOr transformer (errorWithCode 500)
  where
    transformer = do
      tq <- readBody
      teamTableInfo <- liftIO (findTeamTableInfo (teamQueryQuizId tq) (teamQueryTeamNumber tq))
      okJsonResponse teamTableInfo

quizInfoHandler :: Handler b QuizService ()
quizInfoHandler = exceptValueOr transformer (errorWithCode 500)
  where
    transformer = do
      quizIdRequest <- readBody
      let qid = quizIdRequestQuizId quizIdRequest
      quizInfo <- exceptFromMaybeF (liftIO (findQuizInfo Download qid)) "No such quiz id"
      okJsonResponse quizInfo

quizSettingsHandler :: Handler b QuizService ()
quizSettingsHandler = exceptValueOr transformer (errorWithCode 500)
  where
    transformer = do
      quizIdRequest <- readBody
      quizSettings <- liftIO (findQuizSettings (quizIdRequestQuizId quizIdRequest))
      okJsonResponse quizSettings

quizServiceInit :: SnapletInit b QuizService
quizServiceInit =
  makeSnaplet quizPath "Quiz Service" Nothing $ do
    addRoutes quizRoutes
    return QuizService
