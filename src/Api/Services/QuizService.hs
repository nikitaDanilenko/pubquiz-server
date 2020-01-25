{-# Language OverloadedStrings, FlexibleInstances #-}

module Api.Services.QuizService ( quizServiceInit, QuizService ) where

import Control.Applicative                  ( liftA2 )
import Control.Arrow                        ( first )
import Control.Exception                    ( catch )
import Control.Exception.Base               ( IOException )
import Control.Monad                        ( filterM )
import Control.Monad.IO.Class               ( liftIO )
import qualified Data.ByteString.Char8 as B 
import Data.Maybe                           ( maybe, fromMaybe )
import Snap.Core                            ( method, Method ( GET, POST ), writeBS, modifyResponse,
                                              getPostParam, getQueryParam)
import Snap.Snaplet                         ( Handler, SnapletInit, addRoutes, makeSnaplet )

import System.Directory                     ( doesFileExist, getDirectoryContents, 
                                              doesDirectoryExist, createDirectory )

import Api.Services.HashCheck               ( failIfUnverified, authenticate )
import Api.Services.SnapUtil                ( setResponseCodePlain )
import Constants                            ( quizzesFolderIO, locked, addSeparator, quizParam,
                                              roundsFile, labelsFile, rounds, labels, prefix,
                                              ownPointsParam, maxReachedParam, maxReachableParam, 
                                              backToChartViewParam, mainParam, ownPageParam, 
                                              serverQuizPathIO, quizPath, signatureParam, userParam,
                                              actionParam, createQuiz, lock, roundsNumberParam,
                                              server, numberOfTeamsParam, viewQuizzesParam,
                                              cumulativeParam, individualParam, progressionParam ,
                                              placementParam, placeParam, pointsParam,
                                              roundWinnerParam, labelUpdate, roundParam, teamParam )
import Pages.GeneratePage                   ( createWith )
import Pages.QuizzesFrontpage               ( createFrontPage )
import Labels                               ( Labels, teamLabel, showAsBS, parameters, 
                                              defaultLabels )
import Sheet.SheetMaker                     ( createSheetWith, Ending )
import Utils                                ( (+>), randomDistinctAlphaNumeric, randomDistinctWithAdditional )

data QuizService = QuizService

quizRoutes :: [(B.ByteString, Handler b QuizService ())]
quizRoutes = [
    "all" +> method GET sendAvailable,
    "getQuizData" +> method GET getSingleQuizData,
    "getQuizLabels" +> method GET getSingleQuizLabels,
    "updateLabels" +> method POST updateLabels,
    "update" +> method POST updateQuiz,
    "lock" +> method POST lockQuiz,
    "new" +> method POST newQuiz
    ]

-- Finds the list of unlocked quizzes and returns it in bulk.
sendAvailable :: Handler b QuizService ()
sendAvailable = do
    nonLockedQuizzes <- liftIO getNonLockedQuizzes
    writeBS (B.pack (unlines nonLockedQuizzes))
    modifyResponse (setResponseCodePlain 200)

getSingleWithData :: (B.ByteString -> Handler b QuizService ()) -> Handler b QuizService ()
getSingleWithData fetchAction =
  getQueryParam quizParam >>= maybe (modifyResponse (setResponseCodePlain 400)) fetchAction

getSingleQuizData :: Handler b QuizService ()
getSingleQuizData = getSingleWithData perQuiz where
    
  perQuiz :: B.ByteString -> Handler b QuizService ()
  perQuiz q = liftIO (readQuizFile q) 
                  >>= maybe (modifyResponse (setResponseCodePlain 404)) 
                            (\c -> writeBS c >> modifyResponse (setResponseCodePlain 200))
        
getSingleQuizLabels :: Handler b QuizService ()
getSingleQuizLabels = getSingleWithData perQuiz where

  perQuiz :: B.ByteString -> Handler b QuizService ()
  perQuiz q = do
    lbls <- liftIO (readLabelsFile q)
    let response = B.intercalate "\n" (map B.pack (parameters lbls))
    writeBS response
    modifyResponse (setResponseCodePlain 200)

data QUS = QUS { 
    quizCandidate :: Maybe B.ByteString,
    userCandidate :: Maybe B.ByteString,
    signatureCandidate :: Maybe B.ByteString            
  }

getQUS :: Handler b QuizService QUS
getQUS = do
  mQuiz <- getPostParam quizParam
  mUser <- getPostParam userParam
  mSignature <- getPostParam signatureParam
  return (QUS mQuiz mUser mSignature)

updateQuiz :: Handler b QuizService ()
updateQuiz = do
    mNewContent <- getPostParam rounds
    QUS mQuiz mUser mSignature <- getQUS
    verified <- authenticate mUser mSignature [(quizParam, mQuiz), (rounds, mNewContent)]
    failIfUnverified verified (updateQuizData mQuiz mNewContent)

updateQuizData :: Maybe B.ByteString -> Maybe B.ByteString -> Handler b QuizService ()
updateQuizData mQuiz mNewContent = case liftA2 updateWholeQuiz mQuiz (fmap Left mNewContent) of
  Nothing -> do writeBS (B.concat ["Malfolmed request:\n", 
                                   quizParam, "=", B.pack (show mQuiz), "\n", 
                                   rounds, "=", B.pack (show mNewContent)])
                modifyResponse (setResponseCodePlain 406)
  Just io -> respondToUpdate io

updateLabels :: Handler b QuizService ()
updateLabels = do
  QUS mQuiz mUser mSignature <- getQUS
  mRounds <- getPostParam roundsNumberParam
  mNumberOfTeams <- getPostParam numberOfTeamsParam
  verified <- authenticate mUser mSignature [(quizParam, mQuiz), (actionParam, Just labelUpdate)]
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
  else
    randomDistinctWithAdditional (n - l) teamCodeLength es
  where l = length es

teamCodeLength :: Int
teamCodeLength = 6

respondToUpdate :: IO Bool -> Handler b QuizService ()
respondToUpdate io = do 
  isOpen <- liftIO io 
  if isOpen then
    modifyResponse (setResponseCodePlain 200)
  else 
    modifyResponse (setResponseCodePlain 406) >>
    writeBS "Requested quiz is locked or the update contains invalid symbols."

newQuiz :: Handler b QuizService ()
newQuiz = do
    QUS mQuiz mUser mSignature <- getQUS
    mRounds <- getPostParam roundsNumberParam
    mNumberOfTeams <- getPostParam numberOfTeamsParam
    verified <- authenticate mUser mSignature [(quizParam, mQuiz),
                                               (actionParam, Just createQuiz)]
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
                writeBS (B.unwords ["Quiz", 
                                    name, 
                                    "already exists or its labels contain invalid symbols"])
                modifyResponse (setResponseCodePlain 406)

defaultRounds :: [Int]
defaultRounds = replicate 4 8

readRounds :: String -> IO [Int]
readRounds text = fmap read (pure text) `catch` handle where
  handle :: IOException -> IO [Int]
  handle _ = pure defaultRounds

fetchRoundsAndLabelsAndMakeSheet :: B.ByteString 
                                 -> Maybe B.ByteString 
                                 -> [Ending] 
                                 -> Handler b QuizService ()
fetchRoundsAndLabelsAndMakeSheet name mRounds endings = do
  rs <- liftIO (maybe (pure defaultRounds) (readRounds . B.unpack) mRounds)
  fullLabelsPath <- liftIO (mkFullPathIO name labelsFile)
  lbls <- fetchLabels fullLabelsPath
  liftIO (do serverPath <- serverQuizPathIO
             let fullServerPath = addSeparator [server, serverPath]
                 uName = B.unpack name
             createSheetWith (teamLabel lbls) rs uName fullServerPath endings
             updateWholeQuiz name (Left (B.pack (unwords endings)))
             createFrontPage)

-- todo: better with JSON directly.
fetchLabels :: String -> Handler b QuizService Labels
fetchLabels fullPath = do
  params <- mapM getPostParam [roundParam, teamParam, ownPointsParam, maxReachedParam,
                               maxReachableParam, backToChartViewParam, mainParam, ownPageParam,
                               viewQuizzesParam, cumulativeParam, individualParam, progressionParam,
                               placementParam, placeParam, pointsParam, roundWinnerParam
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
    if isOpen && 
       isValidTextWith validInternalQuizNameChars quizLocation then do
        quizzesFolder <- quizzesFolderIO
        let mkFull :: String -> String
            mkFull relative = addSeparator [quizzesFolder, B.unpack quizLocation, relative]

            fullQuizDir = mkFull ""
            fullQuizPath = mkFull roundsFile
            fullLabelPath = mkFull labelsFile
        case content of
          Left c -> B.writeFile fullQuizPath c
          Right _ -> return ()
        createWith (map (first B.unpack) [(prefix, fullQuizDir), 
                                          (rounds, fullQuizPath),
                                          (labels, fullLabelPath)])
        return True
    else return False

lockQuiz :: Handler b QuizService ()
lockQuiz = do
    QUS mQuiz mUser mSignature <- getQUS
    verified <- authenticate mUser mSignature [(quizParam, mQuiz), (actionParam, Just lock)]
    let act = maybe (pure ()) 
                    (\q -> quizzesFolderIO >>= 
                            \quizzesFolder -> 
                                writeFile (addSeparator [quizzesFolder, B.unpack q, locked]) "") 
                                          mQuiz
    failIfUnverified verified (liftIO act)
    modifyResponse (setResponseCodePlain 201)

getNonLockedQuizzes :: IO [String]
getNonLockedQuizzes = do
    quizzesFolder <- liftIO quizzesFolderIO
    quizzes <- getDirectoryContents quizzesFolder
    let withFull = map (\q -> (q, addSeparator [quizzesFolder, q])) quizzes
    qs <- filterM (\ff -> liftA2 (&&) (areLabelsPresent (snd ff)) (isQuizOpen (snd ff))) withFull
    return (map fst qs)

areLabelsPresent :: String -> IO Bool
areLabelsPresent folder = doesFileExist (addSeparator [folder, labelsFile])

isQuizOpen :: String -> IO Bool
isQuizOpen folder = fmap not (doesFileExist (addSeparator [folder, locked]))

readQuizFile :: B.ByteString -> IO (Maybe B.ByteString)
readQuizFile quizLocation = (do 
    filePath <- filePathIO
    file <- B.readFile filePath
    return (Just file)) `catch` handle where
    
    handle :: IOException -> IO (Maybe B.ByteString)
    handle _ = filePathIO >>= \filePath -> putStrLn (filePath ++ " does not exist.") 
                          >> return Nothing

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
    else
      return defaultLabels

  where

  handle :: IOException -> IO Labels
  handle _ = do
    path <- filePathIO
    putStrLn (unwords [path, "does not exist or its contents cannot be parsed as labels.",
                       "Returning default labels."])
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
    if b || not (isValidTextWith validInternalQuizNameChars path) then return False else do
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