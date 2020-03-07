{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Services.QuizService
  ( quizServiceInit
  , QuizService
  ) where

import           Control.Applicative    (liftA2, liftA3)
import           Control.Arrow          (first, (&&&))
import           Control.Exception      (catch)
import           Control.Exception.Base (IOException)
import           Control.Monad          (filterM)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as B
import qualified Data.ByteString.Lazy   as L
import           Data.Maybe             (fromMaybe, maybe)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as E
import           Snap.Core              (Method (GET, POST), getParams,
                                         getPostParam, getQueryParam, method,
                                         modifyResponse, writeBS, writeLBS)
import           Snap.Snaplet           (Handler, SnapletInit, addRoutes,
                                         makeSnaplet)

import           System.Directory       (createDirectory, doesDirectoryExist,
                                         doesFileExist, getDirectoryContents)

import           Api.Services.HashCheck (authenticate, failIfUnverified)
import           Api.Services.SnapUtil  (encodeOrEmpty, fKey, fValue,
                                         getJSONParam, getJSONPostParam,
                                         getJSONPostParamWithPure,
                                         setResponseCodeJSON, strictEncodeF)
import           Constants              (actionParam, allApi, credentialsParam,
                                         getLabelsApi, getQuizRatingsApi,
                                         lockApi, newApi, quizIdParam,
                                         quizIdentifierParam, quizPath,
                                         quizRatingsParam, quizSettingsParam,
                                         serverQuizzesFolderIO, teamQueryParam,
                                         teamTableApi, updateApi,
                                         updateQuizSettingsApi, getQuizInfoApi)
import           Data.Aeson             (FromJSON, ToJSON, decode, encode,
                                         object, (.=))
import           Data.Functor           (void)
import           Data.Functor.Identity  (Identity (Identity))
import           Db.Connection          (DbQuizId, runSql)
import           Db.DbConversion        (Credentials, Header, QuizIdentifier,
                                         QuizInfo, QuizRatings, QuizSettings,
                                         Ratings,
                                         TeamInfo (TeamInfo, teamInfoCode, teamInfoNumber),
                                         active, adjustHeaderToSize, date,
                                         fallbackSettings, labels,
                                         mkDefaultTeamInfos, numberOfTeams,
                                         quizId, quizIdentifier, rounds,
                                         teamNumber, teamQueryQuizId,
                                         teamQueryTeamNumber)
import qualified Db.DbConversion        as D
import           Db.Storage             (createQuiz, createQuizStatement,
                                         findAllActiveQuizzes, findHeader,
                                         findHeaderStatement, findLabels,
                                         findQuizInfo, findQuizRatings,
                                         findRatings, findTeamTableInfo,
                                         lockQuiz, setHeader,
                                         setHeaderStatement, setLabels,
                                         setLabelsStatement, setQuizRatings,
                                         setRatings, setTeamInfo)
import qualified Db.Storage             as S
import           General.Labels         (Labels, defaultLabels, parameters,
                                         showAsBS, teamLabel)
import           General.Types          (Action (CreateQuizA, LockA, UpdateSettingsA),
                                         Activity (Active, Inactive),
                                         Code (Code), RoundNumber,
                                         TeamName (TeamName),
                                         TeamNumber (TeamNumber),
                                         Unwrappable (unwrap), UserHash,
                                         UserName, wrap)
import           GHC.Natural            (naturalToInt)
import           Pages.GeneratePage     (createWith)
import           Pages.QuizzesFrontpage (createFrontPage)
import           Sheet.SheetMaker       (Ending, createSheetWith)
import           Utils                  (randomDistinctHexadecimal, (+>))

data QuizService =
  QuizService

quizRoutes :: [(B.ByteString, Handler b QuizService ())]
quizRoutes =
  [ allApi +> method GET sendAvailableActive
  , getQuizRatingsApi +> method GET getSingleQuizRatings
  , getLabelsApi +> method GET getSingleLabels
  , updateQuizSettingsApi +> method POST updateQuizSettings
  , updateApi +> method POST updateQuiz
  , lockApi +> method POST lockQuizHandler
  , newApi +> method POST newQuiz
  , teamTableApi +> method GET teamTableInfoHandler
  , getQuizInfoApi +> method GET quizInfoHandler
  ]

-- todo: switch all writeBS uses to writeLBS
-- todo: adjust routes after legacy removal
-- todo: remove all legacy functions
sendAvailableActive :: Handler b QuizService ()
sendAvailableActive = do
  active <- liftIO findAllActiveQuizzes
  writeLBS (encode active)
  modifyResponse (setResponseCodeJSON 200)

getSingleQuizRatings :: Handler b QuizService ()
getSingleQuizRatings = do
  mQuizId <- getJSONParam quizIdParam
  case mQuizId of
    Nothing -> modifyResponse (setResponseCodeJSON 400)
    Just qid -> do
      quizRatings <- liftIO (findQuizRatings qid)
      writeLBS (encode quizRatings)
      modifyResponse (setResponseCodeJSON 200)

getSingleLabels :: Handler b QuizService ()
getSingleLabels = do
  mQuizId <- getJSONParam quizIdParam
  case mQuizId of
    Nothing -> writeBS (B.pack "Invalid quiz id") >> modifyResponse (setResponseCodeJSON 400)
    Just qid -> do
      labels <- liftIO (findLabels qid)
      writeLBS (encode labels)
      modifyResponse (setResponseCodeJSON 200)

updateQuiz :: Handler b QuizService ()
updateQuiz = do
  mQuizRatings <- getJSONPostParamWithPure quizRatingsParam
  mQuizId <- getJSONPostParamWithPure quizIdParam
  mCredentials <- getJSONPostParam credentialsParam
  verified <- authenticate mCredentials [(quizIdParam, fKey mQuizId), (quizRatingsParam, fKey mQuizRatings)]
  failIfUnverified verified (updateQuizDataLifted (fValue mQuizId) (fValue mQuizRatings))

updateQuizDataLifted :: Maybe DbQuizId -> Maybe QuizRatings -> Handler b QuizService ()
updateQuizDataLifted mQid mQuizRatings =
  maybe
    (do writeBS (mkUpdateErrorMessage mQid mQuizRatings)
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

updateQuizSettings :: Handler b QuizService ()
updateQuizSettings = do
  mQuizId <- getJSONPostParamWithPure quizIdParam
  mQuizSettings <- getJSONPostParamWithPure quizSettingsParam
  mCredentials <- getJSONPostParam credentialsParam
  verified <-
    authenticate
      mCredentials
      [ (quizIdParam, fKey mQuizId)
      , (quizSettingsParam, fKey mQuizSettings)
      , (actionParam, strictEncodeF (Just UpdateSettingsA))
      ]
  failIfUnverified verified $
    liftIO (fromMaybe (pure ()) (liftA2 updateLabelsAndSettings (fValue mQuizId) (fValue mQuizSettings)))

mkActualTeamNumber :: Maybe B.ByteString -> Int
mkActualTeamNumber = maybe 20 (read . B.unpack)

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
            quizInfo <- createQuizStatement quizIdentifier
            setHeaderStatement (quizId quizInfo) header
            pure quizInfo
        liftIO (createSheetWithSettings (quizId quizInfo) quizIdentifier quizSettings header)
        writeLBS (encode quizInfo)
        modifyResponse (setResponseCodeJSON 200)

updateLabelsAndSettings :: DbQuizId -> QuizSettings -> IO ()
updateLabelsAndSettings qid quizSettings =
  ifActiveDo qid (pure ()) $ \quizInfo ->
    runSql $ do
      let ls = labels quizSettings
      header <- findHeaderStatement qid
      setLabelsStatement qid ls
      adjustedHeader <- liftIO (adjustHeaderToSize (numberOfTeams quizSettings) teamCodeLength (teamLabel ls) header)
      setHeaderStatement qid adjustedHeader
      liftIO (createSheetWithSettings qid (quizIdentifier quizInfo) quizSettings adjustedHeader)

createSheetWithSettings :: DbQuizId -> QuizIdentifier -> QuizSettings -> Header -> IO ()
createSheetWithSettings qid identifier quizSettings header = do
  serverPath <- serverQuizzesFolderIO
  createSheetWith
    (unwrap (teamLabel (labels quizSettings)))
    (map naturalToInt (rounds quizSettings))
    serverPath
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

lockQuizHandler :: Handler b QuizService ()
lockQuizHandler = do
  mQuizId <- getJSONPostParamWithPure quizIdParam
  mCredentials <- getJSONPostParam credentialsParam
  verified <- authenticate mCredentials [(quizIdParam, fKey mQuizId), (actionParam, strictEncodeF (Just LockA))]
  failIfUnverified verified $ do
    liftIO (lockQuiz (fromMaybe (error "Empty key should be impossible") (fValue mQuizId)))
    modifyResponse (setResponseCodeJSON 201)

teamTableInfoHandler :: Handler b QuizService ()
teamTableInfoHandler = do
  mTeamQuery <- getJSONParam teamQueryParam
  case mTeamQuery of
    Nothing -> do
      writeLBS "No such team found"
      modifyResponse (setResponseCodeJSON 404)
    Just tq -> do
      teamTableInfo <- liftIO (findTeamTableInfo (teamQueryQuizId tq) (teamQueryTeamNumber tq))
      writeLBS (encode teamTableInfo)
      modifyResponse (setResponseCodeJSON 201)

quizInfoHandler :: Handler b QuizService ()
quizInfoHandler = do
  mQuizId <- getJSONParam quizIdParam
  case mQuizId of
    Nothing -> do
      writeLBS "Not a valid quiz id"
      modifyResponse (setResponseCodeJSON 404)
    Just qid -> do
      mQuizInfo <- liftIO (findQuizInfo qid)
      case mQuizInfo of
        Nothing -> do
          writeLBS "No quiz with this id found"
          modifyResponse (setResponseCodeJSON 404)
        Just quizInfo -> do
          writeLBS (encode quizInfo)
          modifyResponse (setResponseCodeJSON 201)

quizServiceInit :: SnapletInit b QuizService
quizServiceInit =
  makeSnaplet quizPath "Quiz Service" Nothing $ do
    addRoutes quizRoutes
    return QuizService
