{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Services.QuizService
  ( quizServiceInit
  , QuizService
  ) where

import           Control.Applicative       (liftA2, liftA3)
import           Control.Arrow             (first)
import           Control.Exception         (catch)
import           Control.Exception.Base    (IOException)
import           Control.Monad             (filterM)
import           Control.Monad.IO.Class    (liftIO)
import qualified Data.ByteString.Char8     as B
import qualified Data.ByteString.Lazy      as L
import           Data.Maybe                (fromMaybe, maybe)
import qualified Data.Text                 as T
import           Snap.Core                 (Method (GET, POST), getPostParam,
                                            getQueryParam, method,
                                            modifyResponse, writeBS, writeLBS)
import           Snap.Snaplet              (Handler, SnapletInit, addRoutes,
                                            makeSnaplet)

import           System.Directory          (createDirectory, doesDirectoryExist,
                                            doesFileExist, getDirectoryContents)

import           Api.Services.HashCheck    (authenticate,
                                            authenticateWithCredentials,
                                            failIfUnverified)
import           Api.Services.SnapUtil     (setResponseCodeJSON,
                                            setResponseCodePlain)
import           Constants                 (actionParam, addSeparator,
                                            backToChartViewParam, createQuiz,
                                            credentialsParam, cumulativeParam,
                                            individualParam, labelUpdate,
                                            labels, labelsFile, lock, locked,
                                            mainParam, maxReachableParam,
                                            maxReachedParam, numberOfTeamsParam,
                                            ownPageParam, ownPointsParam,
                                            placeParam, placementParam,
                                            pointsParam, prefix,
                                            progressionParam, quizInfoParam,
                                            quizPath, quizSettingsParam,
                                            quizzesFolderIO, roundParam,
                                            roundWinnerParam, rounds,
                                            roundsFile, roundsNumberParam,
                                            server, serverQuizPathIO,
                                            signatureParam, teamParam,
                                            userParam, viewQuizzesParam)
import           Data.Aeson                (FromJSON, decode, encode)
import           Data.Functor.Identity     (Identity (Identity))
import           Db.Connection             (DbQuizId)
import           Db.DbConversion           (Credentials, QuizInfo, QuizSettings,
                                            fallbackSettings, identifier,
                                            mkQuizInfo, numberOfTeams, quizId,
                                            user)
import qualified Db.DbConversion           as D
import           Db.Storage                (findAllActiveQuizzes, findLabels,
                                            setTeam)
import qualified Db.Storage                as S
import           General.Labels            (Labels, defaultLabels, parameters,
                                            showAsBS, teamLabel)
import           General.Types             (Activity (Active), Code (Code),
                                            TeamName (TeamName),
                                            TeamNumber (TeamNumber),
                                            Unwrappable (unwrap))
import           GHC.Natural               (naturalToInt)
import           Pages.GeneratePage        (createWith)
import           Pages.QuizzesFrontpage    (createFrontPage)
import           Sheet.SheetMaker          (Ending, createSheetWith)
import           Utils                     (randomDistinctAlphaNumeric,
                                            randomDistinctWithAdditional, (+>))

data QuizService =
  QuizService

quizRoutes :: [(B.ByteString, Handler b QuizService ())]
quizRoutes =
  [ "all" +> method GET sendAvailableLegacy
  , "allNew" +> method GET sendAvailableActive
  , "getQuizData" +> method GET getSingleQuizData
  , "getQuizLabels" +> method GET getSingleQuizLabelsLegacy
  , "updateQuizSettings" +> method POST updateQuizSettings
  , "update" +> method POST updateQuiz
  , "lock" +> method POST lockQuiz
  , "new" +> method POST newQuizLegacy
  ]

-- todo: switch all writeBS uses to writeLBS
-- todo: adjust routes after legacy removal
-- todo: remove all legacy functions
-- Finds the list of unlocked quizzes and returns it in bulk.
sendAvailableLegacy :: Handler b QuizService ()
sendAvailableLegacy = do
  nonLockedQuizzes <- liftIO getNonLockedQuizzesLegacy
  writeBS (B.pack (unlines nonLockedQuizzes))
  modifyResponse (setResponseCodePlain 200)

sendAvailableActive :: Handler b QuizService ()
sendAvailableActive = do
  active <- liftIO getActiveQuizzes
  writeLBS (encode active)
  modifyResponse (setResponseCodeJSON 200)

-- todo: remove or adjust
getSingleWithData :: (B.ByteString -> Handler b QuizService ()) -> Handler b QuizService ()
getSingleWithData fetchAction =
  getQueryParam quizInfoParam >>= maybe (modifyResponse (setResponseCodePlain 400)) fetchAction

getSingleQuizData :: Handler b QuizService ()
getSingleQuizData = getSingleWithData perQuiz
  where
    perQuiz :: B.ByteString -> Handler b QuizService ()
    perQuiz q =
      liftIO (readQuizFile q) >>=
      maybe (modifyResponse (setResponseCodePlain 404)) (\c -> writeBS c >> modifyResponse (setResponseCodePlain 200))

getSingleQuizLabelsLegacy :: Handler b QuizService ()
getSingleQuizLabelsLegacy = getSingleWithData perQuiz
  where
    perQuiz :: B.ByteString -> Handler b QuizService ()
    perQuiz q = do
      lbls <- liftIO (readLabelsFile q)
      let response = B.intercalate "\n" (map B.pack (parameters lbls))
      writeBS response
      modifyResponse (setResponseCodePlain 200)

getSingleWithDataJSON :: (B.ByteString -> Handler b QuizService ()) -> Handler b QuizService ()
getSingleWithDataJSON fetchAction =
  getQueryParam quizInfoParam >>= maybe (modifyResponse (setResponseCodeJSON 400)) fetchAction

getSingleQuizLabels :: Handler b QuizService ()
getSingleQuizLabels = getSingleWithDataJSON perQuiz
  where
    perQuiz :: B.ByteString -> Handler b QuizService ()
    perQuiz q =
      case decode (L.fromStrict q) of
        Nothing -> writeBS (B.unwords [B.pack "Invalid quiz id:", q]) >> modifyResponse (setResponseCodeJSON 400)
        Just qid -> do
          lbls <- liftIO (findLabels qid)
          writeLBS (encode lbls)
          modifyResponse (setResponseCodeJSON 200)

data QUS =
  QUS
    { quizCandidate      :: Maybe B.ByteString
    , userCandidate      :: Maybe B.ByteString
    , signatureCandidate :: Maybe B.ByteString
    }

getQUS :: Handler b QuizService QUS
getQUS = liftA3 QUS (getPostParam quizInfoParam) (getPostParam userParam) (getPostParam signatureParam)

updateQuiz :: Handler b QuizService ()
updateQuiz = do
  mNewContent <- getPostParam rounds
  QUS mQuiz mUser mSignature <- getQUS
  verified <- authenticate mUser mSignature [(quizInfoParam, mQuiz), (rounds, mNewContent)]
  failIfUnverified verified (updateQuizData mQuiz mNewContent)

updateQuizData :: Maybe B.ByteString -> Maybe B.ByteString -> Handler b QuizService ()
updateQuizData mQuiz mNewContent =
  case liftA2 updateWholeQuiz mQuiz (fmap Left mNewContent) of
    Nothing -> do
      writeBS
        (B.concat
           [ "Malfolmed request:\n"
           , quizInfoParam
           , "="
           , B.pack (show mQuiz)
           , "\n"
           , rounds
           , "="
           , B.pack (show mNewContent)
           ])
      modifyResponse (setResponseCodePlain 406)
    Just io -> respondToUpdate io

updateQuizSettings :: Handler b QuizService ()
updateQuizSettings = do
  QUS mQuiz mUser mSignature <- getQUS
  mRounds <- getPostParam roundsNumberParam
  mNumberOfTeams <- getPostParam numberOfTeamsParam
  verified <-
    authenticate
      mUser
      mSignature
      [ (quizInfoParam, mQuiz)
      , (roundsNumberParam, mRounds)
      , (numberOfTeamsParam, mNumberOfTeams)
      , (actionParam, Just labelUpdate)
      ]
  failIfUnverified verified $
    case mQuiz of
      Nothing -> writeBS "No name given." >> modifyResponse (setResponseCodePlain 406)
      Just name -> do
        endings <- readCurrentEndings name
        let desiredTeams = mkActualTeamNumber mNumberOfTeams
        otherEndings <- liftIO (adjustEndings endings desiredTeams)
        fetchRoundsAndLabelsAndMakeSheet name mRounds otherEndings
        liftIO createFrontPage
        case liftA2 updateWholeQuiz mQuiz (Just (Right ())) of
          Nothing -> writeBS "Unknown error during label update."
          Just io -> respondToUpdate io

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
newQuizLegacy = do
  QUS mQuiz mUser mSignature <- getQUS
  mRounds <- getPostParam roundsNumberParam
  mNumberOfTeams <- getPostParam numberOfTeamsParam
  verified <- authenticate mUser mSignature [(quizInfoParam, mQuiz), (actionParam, Just createQuiz)]
  failIfUnverified verified $
    case mQuiz of
      Nothing -> writeBS "No name given." >> modifyResponse (setResponseCodePlain 406)
      Just name -> do
        let gs = mkActualTeamNumber mNumberOfTeams
              {- todo: Each ending (code) is at least six symbols long, but this may change.
                 If the number drops to five, one needs to account for the possibility
                 of the string "index" occurring, which would break either the group with
                 that label or the main page.
                 A similar warning holds true, should there be more additional pages,
                 whose names might overlap with randomly generated strings. -}
        endings <- liftIO (randomDistinctAlphaNumeric gs teamCodeLength)
        success <- liftIO (createOrFail name endings)
        if success
          then do
            fetchRoundsAndLabelsAndMakeSheet name mRounds endings
            writeBS (B.unwords ["Created quiz", name])
            modifyResponse (setResponseCodePlain 201)
          else do
            writeBS (B.unwords ["Quiz", name, "already exists or its labels contain invalid symbols"])
            modifyResponse (setResponseCodePlain 406)

attemptDecode :: (Functor f, FromJSON a) => f (Maybe B.ByteString) -> f (Maybe a)
attemptDecode = fmap (>>= decode . L.fromStrict)

newQuiz :: Handler b QuizService ()
newQuiz = do
  mQuizInfoRaw <- getPostParam quizInfoParam
  let Identity mQuizInfo = attemptDecode (Identity mQuizInfoRaw)
  mCredentials <- attemptDecode (getPostParam credentialsParam)
  mSettings <- attemptDecode (getPostParam quizSettingsParam)
  verified <- authenticateWithCredentials mCredentials [(quizInfoParam, mQuizInfoRaw), (actionParam, Just createQuiz)]
  failIfUnverified verified $
    case mQuizInfo of
      Nothing -> writeLBS "Could not read quiz info." >> modifyResponse (setResponseCodeJSON 406)
      Just quizInfo -> do
        let settings = fromMaybe fallbackSettings mSettings
            gs = numberOfTeams settings
        endings <- liftIO (randomDistinctAlphaNumeric (naturalToInt gs) teamCodeLength)
        let teamCodeNames =
              zipWith
                (\n e -> (TeamNumber n, Code e, TeamName (unwrap (teamLabel (D.labels settings)))))
                [1 .. gs]
                (map T.pack endings)
        liftIO (S.createQuiz (identifier quizInfo))
        liftIO (mapM (\(t, c, n) -> setTeam (quizId quizInfo) t c n Active) teamCodeNames)
        pure ()

defaultRounds :: [Int]
defaultRounds = replicate 4 8

readRounds :: String -> IO [Int]
readRounds text = fmap read (pure text) `catch` handle
  where
    handle :: IOException -> IO [Int]
    handle _ = pure defaultRounds

fetchRoundsAndLabelsAndMakeSheet :: B.ByteString -> Maybe B.ByteString -> [Ending] -> Handler b QuizService ()
fetchRoundsAndLabelsAndMakeSheet name mRounds endings = do
  rs <- liftIO (maybe (pure defaultRounds) (readRounds . B.unpack) mRounds)
  fullLabelsPath <- liftIO (mkFullPathIO name labelsFile)
  lbls <- fetchLabelsLegacy fullLabelsPath
  liftIO
    (do serverPath <- serverQuizPathIO
        let fullServerPath = addSeparator [server, serverPath]
            uName = B.unpack name
        createSheetWith (T.unpack $ unwrap $ teamLabel lbls) rs uName fullServerPath endings
        updateWholeQuiz name (Left (B.pack (unwords endings)))
        createFrontPage)

-- todo: better with JSON directly.
fetchLabelsLegacy :: String -> Handler b QuizService Labels
fetchLabelsLegacy fullPath = do
  params <-
    mapM
      getPostParam
      [ roundParam
      , teamParam
      , ownPointsParam
      , maxReachedParam
      , maxReachableParam
      , backToChartViewParam
      , mainParam
      , ownPageParam
      , viewQuizzesParam
      , cumulativeParam
      , individualParam
      , progressionParam
      , placementParam
      , placeParam
      , pointsParam
      , roundWinnerParam
      ]
  {- This is a workaround: The labels are first written as text to provide proper encoding of
     special characters (which bytestrings do properly).
     Afterwards, we read the file regularly and use the common string representation
     of the individual characters.
     This may lead to numeric representation like '\195',
     but since these labels will be made HTML-safe elsewhere,
     such a representation will still be properly translated to the corresponding HTML code.
  -}
  liftIO (B.writeFile fullPath (showAsBS (map (fromMaybe B.empty) params)))
  text <- liftIO (readFile fullPath)
  return (read text)

updateWholeQuiz :: B.ByteString -> Either B.ByteString () -> IO Bool
updateWholeQuiz quizLocation content = do
  isOpen <- isQuizOpen (B.unpack quizLocation)
  if isOpen && isValidTextWith validInternalQuizNameChars quizLocation
    then do
      quizzesFolder <- quizzesFolderIO
      let mkFull :: String -> String
          mkFull relative = addSeparator [quizzesFolder, B.unpack quizLocation, relative]
          fullQuizDir = mkFull ""
          fullQuizPath = mkFull roundsFile
          fullLabelPath = mkFull labelsFile
      case content of
        Left c  -> B.writeFile fullQuizPath c
        Right _ -> return ()
      createWith (map (first B.unpack) [(prefix, fullQuizDir), (rounds, fullQuizPath), (labels, fullLabelPath)])
      return True
    else return False

lockQuiz :: Handler b QuizService ()
lockQuiz = do
  QUS mQuiz mUser mSignature <- getQUS
  verified <- authenticate mUser mSignature [(quizInfoParam, mQuiz), (actionParam, Just lock)]
  let act =
        maybe
          (pure ())
          (\q -> quizzesFolderIO >>= \quizzesFolder -> writeFile (addSeparator [quizzesFolder, B.unpack q, locked]) "")
          mQuiz
  failIfUnverified verified (liftIO act)
  modifyResponse (setResponseCodePlain 201)

getNonLockedQuizzesLegacy :: IO [String]
getNonLockedQuizzesLegacy = do
  quizzesFolder <- liftIO quizzesFolderIO
  quizzes <- getDirectoryContents quizzesFolder
  let withFull = map (\q -> (q, addSeparator [quizzesFolder, q])) quizzes
  qs <- filterM (\ff -> liftA2 (&&) (areLabelsPresent (snd ff)) (isQuizOpen (snd ff))) withFull
  return (map fst qs)

getActiveQuizzes :: IO [QuizInfo]
getActiveQuizzes = fmap (map mkQuizInfo) findAllActiveQuizzes

-- todo: remove this function
areLabelsPresent :: String -> IO Bool
areLabelsPresent folder = doesFileExist (addSeparator [folder, labelsFile])

-- todo: remove this function
isQuizOpen :: String -> IO Bool
isQuizOpen folder = fmap not (doesFileExist (addSeparator [folder, locked]))

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
