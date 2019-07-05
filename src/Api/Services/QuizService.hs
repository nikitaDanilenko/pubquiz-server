{-# Language OverloadedStrings, FlexibleInstances #-}

module Api.Services.QuizService ( quizServiceInit, QuizService ) where

import Control.Applicative                  ( liftA2 )
import Control.Exception                    ( catch )
import Control.Exception.Base               ( IOException )
import Control.Monad                        ( filterM )
import Control.Monad.IO.Class               ( liftIO )
import qualified Data.ByteString.Char8 as B 
import Data.Function                        ( on )
import Data.Maybe                           ( maybe )
import Snap.Core                            ( method, Method ( GET, POST ), writeBS, modifyResponse,
                                              setResponseCode, getPostParam, getQueryParam )
import Snap.Snaplet                         ( Handler, SnapletInit, addRoutes, makeSnaplet )

import System.Directory                     ( doesFileExist, getDirectoryContents, 
                                              doesDirectoryExist, createDirectory )

import Api.Services.HashCheck               ( failIfUnverified, authenticate )
import Constants                            ( quizzesFolderIO, locked, addSeparator, quizParam,
                                              roundsFile, labelsFile, colorsFile, rounds, labels,
                                              colors, prefix, roundParam, teamParam,
                                              ownPointsParam, maxReachedParam, maxReachableParam, 
                                              backToChartViewParam, mainParam, ownPageParam, 
                                              serverQuizPathIO, quizPath, signatureParam, userParam,
                                              actionParam, createQuiz, lock, roundsNumberParam,
                                              server, numberOfTeamsParam, viewQuizzesParam,
                                              cumulativeParam, individualParam, progressionParam )
import Pages.GeneratePage                   ( createWith )
import Pages.QuizzesFrontpage               ( createFrontPage )
import Labels                               ( Labels, mkLabels, teamLabel )
import Sheet.SheetMaker                     ( createSheetWith, Ending )
import Utils                                ( (+>), randomDistinctAlphaNumeric )

data QuizService = QuizService

quizRoutes :: [(B.ByteString, Handler b QuizService ())]
quizRoutes = [
    "all" +> method GET sendAvailable,
    "getQuizData" +> method GET getSingleQuizData,
    "update" +> method POST updateQuiz,
    "lock" +> method POST lockQuiz,
    "new" +> method POST newQuiz
    ]

-- Finds the list of unlocked quizzes and returns it in bulk.
sendAvailable :: Handler b QuizService ()
sendAvailable = do
    nonLockedQuizzes <- liftIO getNonLockedQuizzes
    writeBS (B.pack (unlines nonLockedQuizzes))
    modifyResponse (setResponseCode 200)

getSingleQuizData :: Handler b QuizService ()
getSingleQuizData = 
    getQueryParam quizParam >>= maybe (modifyResponse (setResponseCode 400)) perQuiz where
    
    perQuiz :: B.ByteString -> Handler b QuizService ()
    perQuiz q = liftIO (readQuizFile q) 
                    >>= maybe (modifyResponse (setResponseCode 404)) 
                              (\c -> writeBS c >> modifyResponse (setResponseCode 200))
        
updateQuiz :: Handler b QuizService ()
updateQuiz = do
    mQuiz <- getPostParam quizParam
    mNewContent <- getPostParam rounds
    mUser <- getPostParam userParam
    mSignature <- getPostParam signatureParam
    verified <- authenticate mUser mSignature [(quizParam, mQuiz), (rounds, mNewContent)]
    failIfUnverified verified $
      let act = liftA2 (updateFile `on` B.unpack) mQuiz mNewContent
      in case act of
        Nothing -> do writeBS (B.concat ["Malfolmed request:\n", 
                                         quizParam, "=", B.pack (show mQuiz), "\n", 
                                         rounds, "=", B.pack (show mNewContent)])
                      modifyResponse (setResponseCode 406)
        Just io -> do isOpen <- liftIO io 
                      if isOpen then
                        modifyResponse (setResponseCode 200)
                      else 
                        modifyResponse (setResponseCode 406) >>
                        writeBS "Requested quiz is locked or the update contains invalid symbols."
    
newQuiz :: Handler b QuizService ()
newQuiz = do
    mQuiz <- getPostParam quizParam
    lbls <- fetchLabels
    mRounds <- getPostParam roundsNumberParam
    mUser <- getPostParam userParam
    mSignature <- getPostParam signatureParam
    mNumberOfTeams <- getPostParam numberOfTeamsParam
    verified <- authenticate mUser mSignature [(quizParam, mQuiz),
                                               (actionParam, Just createQuiz)]
    failIfUnverified verified $
      case mQuiz of
          Nothing -> writeBS "No name given." >> modifyResponse (setResponseCode 406)
          Just name -> do 
              let uName = B.unpack name
                  gs = maybe 20 (read . B.unpack) mNumberOfTeams
              endings <- liftIO (randomDistinctAlphaNumeric gs 6)
              success <- liftIO (createOrFail uName endings)
              if success 
               then do
                let rs = maybe 4 (read . B.unpack) mRounds
                liftIO (do writeLabels name lbls 
                           serverPath <- serverQuizPathIO
                           let fullServerPath = addSeparator [server, serverPath]
                           createSheetWith (teamLabel lbls) rs uName fullServerPath endings
                           updateFile (B.unpack name) (unwords endings)
                           createFrontPage)
                writeBS (B.unwords ["Created quiz", name]) 
                modifyResponse (setResponseCode 201)
               else do
                writeBS (B.unwords ["Quiz", 
                                    name, 
                                    "already exists or its labels contain invalid symbols"])
                modifyResponse (setResponseCode 406)

-- todo: better with JSON directly.
fetchLabels :: Handler b QuizService Labels
fetchLabels = do
  params <- mapM getPostParam [roundParam, teamParam, ownPointsParam, maxReachedParam,
                               maxReachableParam, backToChartViewParam, mainParam, ownPageParam,
                               viewQuizzesParam, cumulativeParam, individualParam, progressionParam]
  let r : t : opts : mred : mrable : b : m : opg : vq : c : i : p : _ = map (maybe "" (B.unpack)) params
      lbls = mkLabels r t opts mred mrable b m opg vq c i p
  return lbls

writeLabels :: B.ByteString -> Labels -> IO ()
writeLabels quizLocation lbls = do
  fullPath <- mkFullPathIO quizLocation labelsFile
  writeFile fullPath (show lbls)

updateFile :: String -> String -> IO Bool
updateFile quizLocation content = do
    isOpen <- isQuizOpen quizLocation
    if isOpen && 
       isValidStringWith validInternalQuizNameChars quizLocation then do
        quizzesFolder <- quizzesFolderIO
        let mkFull :: String -> String
            mkFull relative = addSeparator [quizzesFolder, quizLocation, relative]

            fullQuizDir = mkFull ""
            fullQuizPath = mkFull roundsFile
            fullLabelPath = mkFull labelsFile
            fullColorsPath = mkFull colorsFile
        writeFile fullQuizPath content
        createWith (map (\(k, v) -> (B.unpack k, v)) [(prefix, fullQuizDir), 
                                                      (rounds, fullQuizPath),
                                                      (labels, fullLabelPath),
                                                      (colors, fullColorsPath)])
        return True
    else return False

lockQuiz :: Handler b QuizService ()
lockQuiz = do
    mQuiz <- getPostParam quizParam
    mUser <- getPostParam userParam
    mSignature <- getPostParam signatureParam
    verified <- authenticate mUser mSignature [(quizParam, mQuiz), (actionParam, Just lock)]
    let act = maybe (pure ()) 
                    (\q -> quizzesFolderIO >>= 
                            \quizzesFolder -> 
                                writeFile (addSeparator [quizzesFolder, B.unpack q, locked]) "") 
                                          mQuiz
    failIfUnverified verified (liftIO act)
    modifyResponse (setResponseCode 201)

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

mkFullPathIO :: B.ByteString -> FilePath -> IO String
mkFullPathIO quizLocation filePath = do
  quizzesFolder <- quizzesFolderIO
  return (addSeparator [quizzesFolder, B.unpack quizLocation, filePath])

createOrFail :: FilePath -> [Ending] -> IO Bool
createOrFail path endings = do
    quizzesFolder <- quizzesFolderIO
    let fullPath = addSeparator [quizzesFolder, path]
    b <- doesDirectoryExist fullPath
    if b || not (isValidStringWith validInternalQuizNameChars path) then return False else do
        createDirectory fullPath
        writeFile (addSeparator [fullPath, roundsFile]) (unwords endings)
        return True

validInternalQuizNameChars :: [Char]
validInternalQuizNameChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "-_"

isValidStringWith :: [Char] -> String -> Bool
isValidStringWith vcs = all (\c -> c `elem` vcs)

quizServiceInit :: SnapletInit b QuizService
quizServiceInit = do
    makeSnaplet quizPath "Quiz Service" Nothing $ do
        addRoutes quizRoutes
        return QuizService