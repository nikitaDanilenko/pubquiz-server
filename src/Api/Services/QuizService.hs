{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Services.QuizService
  ( quizServiceInit
  , QuizService
  ) where

import           Control.Applicative    (liftA2, liftA3)
import           Control.Arrow          ((&&&))
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as B
import qualified Data.ByteString.Lazy   as L
import           Data.Maybe             (fromMaybe, maybe)
import qualified Data.Text              as T (unpack)
import           Snap.Core              (Method (GET, POST), method,
                                         modifyResponse, writeLBS)
import           Snap.Snaplet           (Handler, SnapletInit, addRoutes,
                                         makeSnaplet)

import           Api.Services.HashCheck (authenticate, failIfUnverified)
import           Api.Services.SnapUtil  (encodeOrEmpty, fKey, fValue,
                                         getJSONParam, getJSONPostParam,
                                         getJSONPostParamWithPure,
                                         setResponseCodeJSON, strictEncodeF)
import           Constants              (actionParam, allActiveApi,
                                         credentialsParam, getLabelsApi,
                                         getQuizInfoApi, getQuizRatingsApi,
                                         getQuizSettingsApi, lockApi, newApi,
                                         quizIdParam, quizIdentifierParam,
                                         quizPath, quizRatingsParam,
                                         quizSettingsParam, serverPathIO,
                                         serverQuizzesFolderIO, sheetsFolderIO,
                                         teamQueryParam, teamTableApi,
                                         updateQuizApi, updateQuizRatingsApi, allApi)
import           Data.Aeson             (encode)
import           Db.Connection          (DbQuizId, runSql)
import           Db.DbConversion        (Credentials, Header, QuizIdentifier,
                                         QuizInfo, QuizRatings, QuizSettings,
                                         TeamInfo (teamInfoCode, teamInfoNumber),
                                         active, adjustHeaderToSize, date,
                                         fallbackSettings, fullSheetPath,
                                         labels, mkDefaultTeamInfos,
                                         numberOfTeams, qrOnlyPath,
                                         questionsInQuiz, quizId,
                                         quizIdentifier, teamQueryQuizId,
                                         teamQueryTeamNumber)
import           Db.Storage             (createQuizStatement,
                                         findAllActiveQuizzes,
                                         findHeaderStatement, findLabels,
                                         findQuizInfo, findQuizRatings,
                                         findQuizSettings, findTeamTableInfo,
                                         lockQuiz, setHeaderStatement,
                                         setLabelsStatement,
                                         setMissingTeamRatingsToZeroStatement,
                                         setQuestionsInQuizStatement,
                                         setQuizIdentifierStatement,
                                         setQuizRatings, setTeamInfo, findAllQuizzes)
import           General.Labels         (teamLabel)
import           General.Types          (Action (CreateQuizA, LockA, UpdateSettingsA),
                                         Activity (Active, Inactive),
                                         Wrapped (unwrap), wrap)
import           GHC.Natural            (naturalToInt)
import           Sheet.SheetMaker       (createSheetWith, safeRemoveFile)
import           Utils                  (randomDistinctHexadecimal, (+>))

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
  writeLBS (encode quizInfoList)
  modifyResponse (setResponseCodeJSON 200)

getQuizRatingsHandler :: Handler b QuizService ()
getQuizRatingsHandler = do
  mQuizId <- getJSONParam quizIdParam
  case mQuizId of
    Nothing -> modifyResponse (setResponseCodeJSON 400)
    Just qid -> do
      quizRatings <- liftIO (findQuizRatings qid)
      writeLBS (encode quizRatings)
      modifyResponse (setResponseCodeJSON 200)

getLabelsHandler :: Handler b QuizService ()
getLabelsHandler = do
  mQuizId <- getJSONParam quizIdParam
  case mQuizId of
    Nothing -> writeLBS "Invalid quiz id" >> modifyResponse (setResponseCodeJSON 400)
    Just qid -> do
      labels <- liftIO (findLabels qid)
      writeLBS (encode labels)
      modifyResponse (setResponseCodeJSON 200)

updateQuizRatingsHandler :: Handler b QuizService ()
updateQuizRatingsHandler = do
  mQuizRatings <- getJSONPostParamWithPure quizRatingsParam
  mQuizId <- getJSONPostParamWithPure quizIdParam
  mCredentials <- getJSONPostParam credentialsParam
  verified <- authenticate mCredentials [(quizIdParam, fKey mQuizId), (quizRatingsParam, fKey mQuizRatings)]
  failIfUnverified verified (updateQuizDataLifted (fValue mQuizId) (fValue mQuizRatings))

updateQuizDataLifted :: Maybe DbQuizId -> Maybe QuizRatings -> Handler b QuizService ()
updateQuizDataLifted mQid mQuizRatings =
  maybe
    (do writeLBS (L.fromStrict (mkUpdateErrorMessage mQid mQuizRatings))
        modifyResponse (setResponseCodeJSON 406))
    liftIO
    (liftA2 updateQuizData mQid mQuizRatings)

mkUpdateErrorMessage :: Maybe DbQuizId -> Maybe QuizRatings -> B.ByteString
mkUpdateErrorMessage mQid mQuizRatings =
  B.unlines
    ("Malfolmed request:" :
     map
       (\(k, v) -> B.intercalate "=" [k, v])
       [(quizIdParam, encodeOrEmpty mQid), (quizRatingsParam, encodeOrEmpty mQuizRatings)])

updateQuizData :: DbQuizId -> QuizRatings -> IO ()
updateQuizData qid quizRatings = ifActiveDo qid (pure ()) (\_ -> setQuizRatings qid quizRatings)

updateQuizHandler :: Handler b QuizService ()
updateQuizHandler = do
  mQuizId <- getJSONPostParamWithPure quizIdParam
  mQuizSettings <- getJSONPostParamWithPure quizSettingsParam
  mQuizIdentifier <- getJSONPostParamWithPure quizIdentifierParam
  mCredentials <- getJSONPostParam credentialsParam
  verified <-
    authenticate
      mCredentials
      [ (quizIdParam, fKey mQuizId)
      , (quizIdentifierParam, fKey mQuizIdentifier)
      , (quizSettingsParam, fKey mQuizSettings)
      , (actionParam, strictEncodeF (Just UpdateSettingsA))
      ]
  failIfUnverified verified $
    liftIO
      (fromMaybe
         (pure ())
         (liftA3 updateIdentifierAndSettings (fValue mQuizId) (fValue mQuizIdentifier) (fValue mQuizSettings)))

teamCodeLength :: Int
teamCodeLength = 6

newQuiz :: Handler b QuizService ()
newQuiz = do
  mQuizIdentifier <- getJSONPostParamWithPure quizIdentifierParam
  mCredentials <- getJSONPostParam credentialsParam
  mSettings <- getJSONPostParamWithPure quizSettingsParam
  verified <-
    authenticate
      mCredentials
      [ (quizIdentifierParam, fKey mQuizIdentifier)
      , (quizSettingsParam, fKey mSettings)
      , (actionParam, strictEncodeF (Just CreateQuizA))
      ]
  failIfUnverified verified $
    case mQuizIdentifier of
      Nothing -> writeLBS "Could not read quiz info." >> modifyResponse (setResponseCodeJSON 406)
      Just (_, quizIdentifier) -> do
        let quizSettings = fromMaybe fallbackSettings (fValue mSettings)
            gs = numberOfTeams quizSettings
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
        writeLBS (encode quizInfo)
        modifyResponse (setResponseCodeJSON 200)

updateIdentifierAndSettings :: DbQuizId -> QuizIdentifier -> QuizSettings -> IO ()
updateIdentifierAndSettings qid idf quizSettings =
  ifActiveDo qid (pure ()) $ \quizInfo ->
    runSql $ do
      setQuizIdentifierStatement qid idf
      let ls = labels quizSettings
      header <- findHeaderStatement qid
      setLabelsStatement qid ls
      adjustedHeader <- liftIO (adjustHeaderToSize (numberOfTeams quizSettings) teamCodeLength (teamLabel ls) header)
      setHeaderStatement qid adjustedHeader
      setMissingTeamRatingsToZeroStatement qid
      setQuestionsInQuizStatement qid (questionsInQuiz quizSettings)
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

ifActiveDo :: DbQuizId -> IO a -> (QuizInfo -> IO a) -> IO a
ifActiveDo qid dft action = findQuizInfo qid >>= maybe dft checkActive
  where
    checkActive quizInfo =
      case active quizInfo of
        Active   -> action quizInfo
        Inactive -> dft

lockHandler :: Handler b QuizService ()
lockHandler = do
  mQuizId <- getJSONPostParamWithPure quizIdParam
  mCredentials <- getJSONPostParam credentialsParam
  verified <- authenticate mCredentials [(quizIdParam, fKey mQuizId), (actionParam, strictEncodeF (Just LockA))]
  failIfUnverified verified $
    case mQuizId of
      Just (_, qid) -> do
        mQuizInfo <- liftIO (findQuizInfo qid)
        case mQuizInfo of
          Just quizInfo -> do
            liftIO (lockQuiz qid)
            mapM_ (liftIO . safeRemoveFile . T.unpack) [fullSheetPath quizInfo, qrOnlyPath quizInfo]
            modifyResponse (setResponseCodeJSON 201)
          Nothing -> errorInfo "No quiz with given id."
      Nothing -> errorInfo "No valid quiz id in parameter."

errorInfo :: L.ByteString -> Handler b QuizService ()
errorInfo str = do
  writeLBS str
  modifyResponse (setResponseCodeJSON 404)

teamTableInfoHandler :: Handler b QuizService ()
teamTableInfoHandler = do
  mTeamQuery <- getJSONParam teamQueryParam
  case mTeamQuery of
    Nothing -> errorInfo "No such team found"
    Just tq -> do
      teamTableInfo <- liftIO (findTeamTableInfo (teamQueryQuizId tq) (teamQueryTeamNumber tq))
      writeLBS (encode teamTableInfo)
      modifyResponse (setResponseCodeJSON 201)

quizInfoHandler :: Handler b QuizService ()
quizInfoHandler = do
  mQuizId <- getJSONParam quizIdParam
  case mQuizId of
    Nothing -> errorInfo "Not a valid quiz id"
    Just qid -> do
      mQuizInfo <- liftIO (findQuizInfo qid)
      case mQuizInfo of
        Nothing -> errorInfo "No quiz with this id found"
        Just quizInfo -> do
          writeLBS (encode quizInfo)
          modifyResponse (setResponseCodeJSON 201)

quizSettingsHandler :: Handler b QuizService ()
quizSettingsHandler = do
  mQuizId <- getJSONParam quizIdParam
  case mQuizId of
    Nothing -> errorInfo "Not a valid quiz id"
    Just qid -> do
      quizSettings <- liftIO (findQuizSettings qid)
      writeLBS (encode quizSettings)
      modifyResponse (setResponseCodeJSON 201)

quizServiceInit :: SnapletInit b QuizService
quizServiceInit =
  makeSnaplet quizPath "Quiz Service" Nothing $ do
    addRoutes quizRoutes
    return QuizService
