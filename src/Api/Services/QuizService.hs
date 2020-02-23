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
                                         setResponseCodePlain, strictEncodeF)
import           Constants              (actionParam, addSeparator,
                                         backToChartViewParam, credentialsParam,
                                         cumulativeParam, headerParam,
                                         individualParam, labelsFile,
                                         labelsParam, locked, mainParam,
                                         maxReachableParam, maxReachedParam,
                                         numberOfTeamsParam, ownPageParam,
                                         ownPointsParam, placeParam,
                                         placementParam, pointsParam, prefix,
                                         progressionParam, quizIdParam,
                                         quizPDNParam, quizPath,
                                         quizSettingsParam, quizzesFolderIO,
                                         ratingsParam, roundParam,
                                         roundWinnerParam, roundsFile,
                                         roundsNumberParam, server,
                                         serverQuizPathIO, signatureParam,
                                         teamParam, userParam, viewQuizzesParam)
import           Data.Aeson             (FromJSON, ToJSON, decode, encode,
                                         object, (.=))
import           Data.Functor           (void)
import           Data.Functor.Identity  (Identity (Identity))
import           Db.Connection          (DbQuizId)
import           Db.DbConversion        (Credentials,
                                         Header (Header, teamInfos), QuizInfo,
                                         QuizPDN, QuizSettings, Ratings,
                                         TeamInfo (TeamInfo, teamInfoActivity, teamInfoCode, teamInfoName, teamInfoNumber),
                                         active, fallbackSettings, fullQuizName,
                                         identifier, mkQuizInfo, numberOfTeams,
                                         quizId, user)
import qualified Db.DbConversion        as D
import           Db.Storage             (createQuiz, findAllActiveQuizzes,
                                         findHeader, findLabels, findQuizInfo,
                                         findRatings, lockQuiz, setHeader,
                                         setLabels, setRatings, setTeamInfo)
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
import           Utils                  (randomDistinctHexadecimal,
                                         randomDistinctWithAdditional, (+>))

data QuizService =
  QuizService

quizRoutes :: [(B.ByteString, Handler b QuizService ())]
quizRoutes =
  [ "all" +> method GET sendAvailableActive
  , "getQuizRating" +> method GET getSingleQuizData
  , "getQuizInfo" +> method GET getSingleQuizInfo
  , "updateQuizSettings" +> method POST updateQuizSettings
  , "update" +> method POST updateQuiz
  , "lock" +> method POST lockQuizHandler
  , "new" +> method POST newQuiz
  ]

-- todo: switch all writeBS uses to writeLBS
-- todo: adjust routes after legacy removal
-- todo: remove all legacy functions
sendAvailableActive :: Handler b QuizService ()
sendAvailableActive = do
  active <- liftIO findAllActiveQuizzes
  writeLBS (encode active)
  modifyResponse (setResponseCodeJSON 200)

getSingleQuizData :: Handler b QuizService ()
getSingleQuizData = do
  mQuizId <- getJSONPostParam quizIdParam
  case mQuizId of
    Nothing -> modifyResponse (setResponseCodeJSON 400)
    Just qid -> do
      header <- liftIO (findHeader qid)
      ratings <- liftIO (findRatings qid)
      writeLBS (encode (object [E.decodeUtf8 headerParam .= header, E.decodeUtf8 ratingsParam .= ratings]))
      modifyResponse (setResponseCodeJSON 200)

getSingleQuizInfo :: Handler b QuizService ()
getSingleQuizInfo = do
  mQuizId <- getJSONPostParam quizIdParam
  case mQuizId of
    Nothing -> writeBS (B.pack "Invalid quiz id") >> modifyResponse (setResponseCodeJSON 400)
    Just qid -> do
      mQuizInfo <- liftIO (findQuizInfo qid)
      case mQuizInfo of
        Nothing ->
          writeBS (B.unwords [B.pack "No info for quiz id", L.toStrict (encode qid)]) >>
          modifyResponse (setResponseCodeJSON 400)
        Just quizInfo -> writeLBS (encode quizInfo) >> modifyResponse (setResponseCodeJSON 200)

updateQuiz :: Handler b QuizService ()
updateQuiz = do
  mNewHeader <- getJSONPostParamWithPure headerParam
  mNewContent <- getJSONPostParamWithPure ratingsParam
  mQuizId <- getJSONPostParamWithPure quizIdParam
  mCredentials <- getJSONPostParam credentialsParam
  verified <- authenticate mCredentials [(quizIdParam, fKey mQuizId), (ratingsParam, fKey mNewContent)]
  failIfUnverified verified (updateQuizDataLifted (fValue mQuizId) (fValue mNewHeader) (fValue mNewContent))

fKey :: Functor f => f (a, b) -> f a
fKey = fmap fst

fValue :: Functor f => f (a, b) -> f b
fValue = fmap snd

updateQuizDataLifted :: Maybe DbQuizId -> Maybe Header -> Maybe Ratings -> Handler b QuizService ()
updateQuizDataLifted mQid mHeader mRatings =
  maybe
    (do writeBS (mkUpdateErrorMessage mQid mHeader mRatings)
        modifyResponse (setResponseCodePlain 406))
    liftIO
    (liftA3 updateQuizData mQid mHeader mRatings)

mkUpdateErrorMessage :: Maybe DbQuizId -> Maybe Header -> Maybe Ratings -> B.ByteString
mkUpdateErrorMessage mQid mHeader mRatings =
  B.unlines
    ("Malfolmed request:" :
     map
       (\(k, v) -> B.intercalate "=" [k, v])
       [(quizIdParam, encodeOrEmpty mQid), (headerParam, encodeOrEmpty mHeader), (ratingsParam, encodeOrEmpty mRatings)])

updateQuizData :: DbQuizId -> Header -> Ratings -> IO ()
updateQuizData qid header ratings =
  ifActiveDo qid (pure ()) $ \quizInfo -> do
    setHeader qid header
    setRatings qid ratings

updateQuizSettings :: Handler b QuizService ()
updateQuizSettings = do
  mQuizId <- getJSONPostParamWithPure quizIdParam
  mRounds <- getJSONPostParamWithPure roundsNumberParam
  mNumberOfTeams <- getJSONPostParamWithPure numberOfTeamsParam
  mLabels <- getJSONPostParamWithPure labelsParam
  mCredentials <- getJSONPostParam credentialsParam
  verified <-
    authenticate
      mCredentials
      [ (quizIdParam, fKey mQuizId)
      , (roundsNumberParam, fKey mRounds)
      , (numberOfTeamsParam, fKey mNumberOfTeams)
      , (labelsParam, fKey mLabels)
      , (actionParam, strictEncodeF (Just UpdateSettingsA))
      ]
  failIfUnverified verified $
    liftIO
      (fromMaybe
         (pure ())
         (pure updateLabelsAndSettings <*> fValue mQuizId <*> fValue mLabels <*> fValue mNumberOfTeams <*>
          fValue mRounds))

mkActualTeamNumber :: Maybe B.ByteString -> Int
mkActualTeamNumber = maybe 20 (read . B.unpack)

adjustEndings :: [Ending] -> Int -> IO [Ending]
adjustEndings es n =
  if l >= n
    then return (take n es)
    else randomDistinctWithAdditional (n - l) teamCodeLength es
  where
    l = length es

teamCodeLength :: Int
teamCodeLength = 6

newQuiz :: Handler b QuizService ()
newQuiz = do
  mQuizPDN <- getJSONPostParamWithPure quizPDNParam
  mCredentials <- getJSONPostParam credentialsParam
  mSettings <- getJSONPostParamWithPure quizSettingsParam
  verified <-
    authenticate
      mCredentials
      [ (quizPDNParam, fKey mQuizPDN)
      , (quizSettingsParam, fKey mSettings)
      , (actionParam, strictEncodeF (Just CreateQuizA))
      ]
  failIfUnverified verified $
    case mQuizPDN of
      Nothing -> writeLBS "Could not read quiz info." >> modifyResponse (setResponseCodeJSON 406)
      Just (_, quizPDN) -> do
        let settings = fromMaybe fallbackSettings (fValue mSettings)
            gs = numberOfTeams settings
        endings <- liftIO (randomDistinctHexadecimal (naturalToInt gs) teamCodeLength)
        let header =
              Header
                (zipWith
                   (\n e ->
                      TeamInfo
                        { teamInfoCode = wrap e
                        , teamInfoName = wrap (unwrap (teamLabel (D.labels settings)) :: T.Text)
                        , teamInfoNumber = wrap n
                        , teamInfoActivity = Active
                        })
                   [1 .. gs]
                   (map T.pack endings))
        quizId <- liftIO (createQuiz quizPDN)
        liftIO (setHeader quizId header)
        writeLBS (encode quizId)
        modifyResponse (setResponseCodeJSON 200)

updateLabelsAndSettings :: DbQuizId -> Labels -> TeamNumber -> [RoundNumber] -> IO ()
updateLabelsAndSettings qid lbls tn rns =
  ifActiveDo qid (pure ()) $ \quizInfo -> do
    setLabels qid lbls
    endings <- fmap (fmap (unwrap . teamInfoCode) . teamInfos) (findHeader qid)
    adjustedEndings <- adjustEndings endings (naturalToInt (unwrap tn))
    serverPath <- serverQuizPathIO
    createSheetWith
      (unwrap (teamLabel lbls))
      (map (naturalToInt . unwrap) rns)
      (T.unpack (fullQuizName (identifier quizInfo)))
      serverPath
      adjustedEndings

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
