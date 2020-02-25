{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Services.QuizService
  ( quizServiceInit
  , QuizService
  ) where

import           Control.Applicative    (liftA2, liftA3)
import           Control.Arrow          (first)
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
import           Api.Services.SnapUtil  (attemptDecode, encodeOrEmpty,
                                         getJSONPostParam,
                                         getJSONPostParamWithPure,
                                         setResponseCodeJSON,
                                         setResponseCodePlain, strictEncodeF, getJSONParam)
import           Constants              (actionParam, addSeparator,
                                         backToChartViewParam, credentialsParam,
                                         cumulativeParam, individualParam,
                                         labelsFile, labelsParam, locked,
                                         mainParam, maxReachableParam,
                                         maxReachedParam, numberOfTeamsParam,
                                         ownPageParam, ownPointsParam,
                                         placeParam, placementParam,
                                         pointsParam, prefix, progressionParam,
                                         quizIdParam, quizIdentifierParam, quizPath,
                                         quizRatingsParam, quizSettingsParam,
                                         quizzesFolderIO, roundParam,
                                         roundWinnerParam, roundsFile,
                                         roundsNumberParam, server,
                                         serverQuizPathIO, signatureParam,
                                         teamParam, userParam, viewQuizzesParam, allApi, getQuizRatingsApi, getLabelsApi, updateQuizSettingsApi, updateApi, lockApi, newApi)
import           Data.Aeson             (FromJSON, ToJSON, decode, encode,
                                         object, (.=))
import           Data.Functor           (void)
import           Data.Functor.Identity  (Identity (Identity))
import           Db.Connection          (DbQuizId, runSql)
import           Db.DbConversion        (Credentials,
                                         Header, QuizInfo,
                                         QuizIdentifier, QuizRatings, QuizSettings,
                                         Ratings,
                                         TeamInfo (TeamInfo, teamInfoActivity, teamInfoCode, teamInfoName, teamInfoNumber),
                                         active, fallbackSettings, fullQuizName,
                                         quizIdentifier, mkQuizInfo, numberOfTeams,
                                         quizId, user, labels, rounds, adjustHeaderToSize, mkDefaultTeamInfos)
import qualified Db.DbConversion        as D
import           Db.Storage             (createQuiz, findAllActiveQuizzes,
                                         findHeader, findLabels, findQuizInfo,
                                         findQuizRatings, findRatings, lockQuiz,
                                         setHeader, setLabels, setQuizRatings,
                                         setRatings, setTeamInfo, createQuizStatement, setHeaderStatement, setLabelsStatement, findHeaderStatement)
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

fKey :: Functor f => f (a, b) -> f a
fKey = fmap fst

fValue :: Functor f => f (a, b) -> f b
fValue = fmap snd

updateQuizDataLifted :: Maybe DbQuizId -> Maybe QuizRatings -> Handler b QuizService ()
updateQuizDataLifted mQid mQuizRatings =
  maybe
    (do writeBS (mkUpdateErrorMessage mQid mQuizRatings)
        modifyResponse (setResponseCodePlain 406))
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
    liftIO
      (fromMaybe
         (pure ())
         (liftA2 updateLabelsAndSettings (fValue mQuizId) (fValue mQuizSettings)))

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
        quizId <- liftIO $ runSql $ do
          qid <- createQuizStatement quizIdentifier
          setHeaderStatement qid header
          pure qid
        liftIO (createSheetWithSettings quizIdentifier quizSettings header)
        writeLBS (encode quizId)
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
      liftIO (createSheetWithSettings (quizIdentifier quizInfo) quizSettings adjustedHeader)

createSheetWithSettings :: QuizIdentifier -> QuizSettings -> Header -> IO ()
createSheetWithSettings identifier quizSettings header = do
  serverPath <- serverQuizPathIO
  createSheetWith
    (unwrap (teamLabel (labels quizSettings)))
    (map naturalToInt (rounds quizSettings))
    (T.unpack (fullQuizName identifier))
    serverPath
    (map (unwrap . teamInfoCode) (unwrap header))

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
    modifyResponse (setResponseCodePlain 201)

quizServiceInit :: SnapletInit b QuizService
quizServiceInit =
  makeSnaplet quizPath "Quiz Service" Nothing $ do
    addRoutes quizRoutes
    return QuizService
