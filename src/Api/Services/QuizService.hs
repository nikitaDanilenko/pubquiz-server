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
import           Snap.Core              (Method (GET, POST), getPostParam,
                                         getQueryParam, method, modifyResponse,
                                         writeBS, writeLBS)
import           Snap.Snaplet           (Handler, SnapletInit, addRoutes,
                                         makeSnaplet)

import           System.Directory       (createDirectory, doesDirectoryExist,
                                         doesFileExist, getDirectoryContents)

import           Api.Services.HashCheck (authenticate, failIfUnverified)
import           Api.Services.SnapUtil  (attemptDecode, getJSONPostParam,
                                         setResponseCodeJSON,
                                         setResponseCodePlain)
import           Constants              (actionParam, addSeparator,
                                         backToChartViewParam, createQuizAction,
                                         credentialsParam, cumulativeParam,
                                         headerParam, individualParam,
                                         labelUpdateAction, labelsFile,
                                         labelsParam, lockAction, locked,
                                         mainParam, maxReachableParam,
                                         maxReachedParam, numberOfTeamsParam,
                                         ownPageParam, ownPointsParam,
                                         placeParam, placementParam,
                                         pointsParam, prefix, progressionParam,
                                         quizIdParam, quizPDNParam, quizPath,
                                         quizSettingsParam, quizzesFolderIO,
                                         roundParam, roundWinnerParam,
                                         roundsFile, roundsNumberParam,
                                         ratingsParam, server, serverQuizPathIO,
                                         signatureParam, teamParam, userParam,
                                         viewQuizzesParam)
import           Data.Aeson             (FromJSON, ToJSON, decode, encode,
                                         object, (.=))
import           Data.Functor           (void)
import           Data.Functor.Identity  (Identity (Identity))
import           Db.Connection          (DbQuizId)
import           Db.DbConversion        (Credentials, Header (teamInfos),
                                         QuizInfo, QuizSettings, Ratings,
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
import           General.Types          (Activity (Active, Inactive),
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
  , "getQuizData" +> method GET getSingleQuizData
  , "getQuizLabels" +> method GET getSingleQuizLabels
  , "updateQuizSettings" +> method POST updateQuizSettings
  , "update" +> method POST updateQuiz
  , "lock" +> method POST lockQuizHandler
  , "new" +> method POST newQuizLegacy
  ]

-- todo: switch all writeBS uses to writeLBS
-- todo: adjust routes after legacy removal
-- todo: remove all legacy functions
sendAvailableActive :: Handler b QuizService ()
sendAvailableActive = do
  active <- liftIO findAllActiveQuizzes
  writeLBS (encode active)
  modifyResponse (setResponseCodeJSON 200)

-- todo: remove or adjust
getSingleWithData :: (B.ByteString -> Handler b QuizService ()) -> Handler b QuizService ()
getSingleWithData fetchAction =
  getQueryParam quizPDNParam >>= maybe (modifyResponse (setResponseCodePlain 400)) fetchAction

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

getSingleQuizLabels :: Handler b QuizService ()
getSingleQuizLabels = do
  mQuizId <- getJSONPostParam quizIdParam
  case mQuizId of
    Nothing -> writeBS (B.pack "Invalid quiz id") >> modifyResponse (setResponseCodeJSON 400)
    Just qid -> do
      lbls <- liftIO (findLabels qid)
      writeLBS (encode lbls)
      modifyResponse (setResponseCodeJSON 200)

data QUS =
  QUS
    { quizIdCandidate    :: Maybe DbQuizId
    , userCandidate      :: Maybe UserName
    , signatureCandidate :: Maybe UserHash
    }

getQUS :: Handler b QuizService QUS
getQUS = liftA3 QUS (getJSONPostParam quizIdParam) (getJSONPostParam userParam) (getJSONPostParam signatureParam)

updateQuiz :: Handler b QuizService ()
updateQuiz = do
  mNewHeader <- getJSONPostParam headerParam
  mNewContent <- getJSONPostParam ratingsParam
  mQuizId <- getJSONPostParam quizIdParam
  mCredentials <- getJSONPostParam credentialsParam
  verified <- authenticate mCredentials [(quizIdParam, strictEncode mQuizId), (ratingsParam, strictEncode mNewContent)]
  failIfUnverified verified (updateQuizDataLifted mQuizId mNewHeader mNewContent)

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
  mQuizId <- getJSONPostParam quizIdParam
  mRounds <- getJSONPostParam roundsNumberParam
  mNumberOfTeams <- getJSONPostParam numberOfTeamsParam
  mLabels <- getJSONPostParam labelsParam
  mCredentials <- getJSONPostParam credentialsParam
  verified <-
    authenticate
      mCredentials
      [ (quizIdParam, strictEncode mQuizId)
      , (roundsNumberParam, strictEncode mRounds)
      , (numberOfTeamsParam, strictEncode mNumberOfTeams)
      , (labelsParam, strictEncode mLabels)
      , (actionParam, Just labelUpdateAction)
      ]
  failIfUnverified verified $
    liftIO (fromMaybe (pure ()) (pure updateLabelsAndSettings <*> mQuizId <*> mLabels <*> mNumberOfTeams <*> mRounds))

strictEncode :: (Functor f, ToJSON a) => f a -> f B.ByteString
strictEncode = fmap (L.toStrict . encode)

encodeOrEmpty :: ToJSON a => Maybe a -> B.ByteString
encodeOrEmpty = fromMaybe "" . strictEncode

readCurrentEndings :: B.ByteString -> Handler b QuizService [Ending]
readCurrentEndings name = do
  mText <- liftIO (readQuizFile name)
  return (maybe [] (concatMap (words . B.unpack) . take 1 . B.lines) mText)

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

respondToUpdate :: IO Bool -> Handler b QuizService ()
respondToUpdate io = do
  isOpen <- liftIO io
  if isOpen
    then modifyResponse (setResponseCodePlain 200)
    else modifyResponse (setResponseCodePlain 406) >>
         writeBS "Requested quiz is locked or the update contains invalid symbols."

newQuizLegacy :: Handler b QuizService ()
newQuizLegacy = undefined

newQuiz :: Handler b QuizService ()
newQuiz = do
  mQuizPDNRaw <- getPostParam quizPDNParam
  let Identity mQuizPDN = attemptDecode (Identity mQuizPDNRaw)
  mCredentials <- attemptDecode (getPostParam credentialsParam)
  mSettings <- attemptDecode (getPostParam quizSettingsParam)
  verified <- authenticate mCredentials [(quizPDNParam, mQuizPDNRaw), (actionParam, Just createQuizAction)]
  failIfUnverified verified $
    case mQuizPDN of
      Nothing -> writeLBS "Could not read quiz info." >> modifyResponse (setResponseCodeJSON 406)
      Just quizPDN -> do
        let settings = fromMaybe fallbackSettings mSettings
            gs = numberOfTeams settings
        endings <- liftIO (randomDistinctHexadecimal (naturalToInt gs) teamCodeLength)
        let teamInfos =
              zipWith
                (\n e ->
                   TeamInfo
                     { teamInfoCode = wrap e
                     , teamInfoName = wrap (unwrap (teamLabel (D.labels settings)) :: T.Text)
                     , teamInfoNumber = wrap n
                     , teamInfoActivity = Active
                     })
                [1 .. gs]
                (map T.pack endings)
        quizId <- liftIO (createQuiz quizPDN)
        liftIO (mapM_ (setTeamInfo quizId) teamInfos)

defaultRounds :: [Int]
defaultRounds = replicate 4 8

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
  mQuizId <- getJSONPostParam quizIdParam
  mCredentials <- getJSONPostParam credentialsParam
  verified <- authenticate mCredentials [(quizIdParam, strictEncode mQuizId), (actionParam, Just lockAction)]
  failIfUnverified verified $ do
    liftIO (lockQuiz (fromMaybe (error "Empty key should be impossible") mQuizId))
    modifyResponse (setResponseCodePlain 201)

readQuizFile :: B.ByteString -> IO (Maybe B.ByteString)
readQuizFile quizLocation =
  (do filePath <- filePathIO
      file <- B.readFile filePath
      return (Just file)) `catch`
  handle
  where
    handle :: IOException -> IO (Maybe B.ByteString)
    handle _ = filePathIO >>= \filePath -> putStrLn (filePath ++ " does not exist.") >> return Nothing
    filePathIO :: IO String
    filePathIO = mkFullPathIO quizLocation roundsFile

readLabelsFile :: B.ByteString -> IO Labels
readLabelsFile quizLocation = do
  filePath <- filePathIO
  exists <- doesFileExist filePath
  if exists
    then do
      file <- B.readFile filePath
      let lbls = read (B.unpack file)
      return lbls `catch` handle
    else return defaultLabels
  where
    handle :: IOException -> IO Labels
    handle _ = do
      path <- filePathIO
      putStrLn
        (unwords [path, "does not exist or its contents cannot be parsed as labels.", "Returning default labels."])
      return defaultLabels
    filePathIO :: IO String
    filePathIO = mkFullPathIO quizLocation labelsFile

mkFullPathIO :: B.ByteString -> FilePath -> IO String
mkFullPathIO quizLocation filePath = do
  quizzesFolder <- quizzesFolderIO
  return (addSeparator [quizzesFolder, B.unpack quizLocation, filePath])

createOrFail :: B.ByteString -> [Ending] -> IO Bool
createOrFail path endings = do
  quizzesFolder <- quizzesFolderIO
  let fullPath = addSeparator [quizzesFolder, B.unpack path]
  b <- doesDirectoryExist fullPath
  if b || not (isValidTextWith validInternalQuizNameChars path)
    then return False
    else do
      createDirectory fullPath
      B.writeFile (addSeparator [fullPath, roundsFile]) (B.pack (unwords endings))
      return True

writeEndings :: String -> [Ending] -> IO ()
writeEndings path endings = do
  ls <- fmap B.lines (B.readFile path)
  let remainder = drop 1 ls
      es = B.pack (unwords endings)
  B.writeFile path (B.unlines (es : ls))

validInternalQuizNameChars :: String
validInternalQuizNameChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "-_"

isValidTextWith :: String -> B.ByteString -> Bool
isValidTextWith vcs = B.all (`elem` vcs)

quizServiceInit :: SnapletInit b QuizService
quizServiceInit =
  makeSnaplet quizPath "Quiz Service" Nothing $ do
    addRoutes quizRoutes
    return QuizService
