{-# Language OverloadedStrings, FlexibleInstances, PackageImports #-}

module Api.Services.QuizService where

import Control.Applicative                  ( liftA2 )
import Control.Exception                    ( catch )
import Control.Exception.Base               ( IOException )
import Control.Monad                        ( filterM )
import Control.Monad.IO.Class               ( liftIO )
import qualified Data.ByteString.Char8 as B 
import Data.Function                        ( on )
import Data.Maybe                           ( maybe )
import Snap.Core                            ( method, Method ( GET, POST ), writeBS, modifyResponse,
                                              setResponseCode, getPostParam )
import Snap.Snaplet                         ( Handler, SnapletInit, addRoutes, makeSnaplet )
import Snap.Util.CORS                       ( applyCORS, defaultOptions )
import System.Directory                     ( doesFileExist, getDirectoryContents )
import System.Process                       ( callProcess )

import Constants                            ( quizzesFolderIO, locked, addSeparator, lock, quiz,
                                              roundsFile, labelsFile, colorsFile, rounds, labels,
                                              colors, pageGenerator, prefix )

data QuizService = QuizService

quizRoutes :: [(B.ByteString, Handler b QuizService ())]
quizRoutes = map (\(name, handler) -> (name, mkCORS handler)) [
    ("all", method GET sendAvailable),
    ("getQuizData", method GET getSingleQuizData),
    ("update", method POST updateQuiz),
    ("lock", method POST lockQuiz)
    ]

mkCORS :: Handler b QuizService () -> Handler b QuizService ()
mkCORS = applyCORS defaultOptions

-- Finds the list of unlocked quizzes and returns it in bulk.
sendAvailable :: Handler b QuizService ()
sendAvailable = do
    nonLockedQuizzes <- liftIO getNonLockedQuizzes
    writeBS (B.pack (unlines nonLockedQuizzes))
    modifyResponse (setResponseCode 200)

getSingleQuizData :: Handler b QuizService ()
getSingleQuizData = getPostParam quiz >>= maybe (modifyResponse (setResponseCode 400)) perQuiz where
    
    perQuiz :: B.ByteString -> Handler b QuizService ()
    perQuiz q = liftIO (readQuizFile q) 
                    >>= maybe (modifyResponse (setResponseCode 404)) 
                              (\c -> writeBS c >> modifyResponse (setResponseCode 200))
        
updateQuiz :: Handler b QuizService ()
updateQuiz = do
    mQuiz <- getPostParam quiz
    mNewContent <- getPostParam rounds
    let act = liftA2 (updateFile `on` B.unpack) mQuiz mNewContent
    case act of
        Nothing -> do writeBS (B.concat ["Malfolmed request:\n", 
                                         quiz, "=", B.pack (show mQuiz), "\n", 
                                         rounds, "=", B.pack (show mNewContent)])
                      modifyResponse (setResponseCode 406)
        Just io -> do isOpen <- liftIO io 
                      if isOpen then
                        modifyResponse (setResponseCode 200)
                      else writeBS "Requested quiz is locked."

mkKV :: String -> String -> String
mkKV key value = concat [key, "=", value]

updateFile :: String -> String -> IO Bool
updateFile quizPath content = do
    isOpen <- isQuizOpen quizPath
    if isOpen then do
        quizzesFolder <- quizzesFolderIO
        let mkFull :: String -> String
            mkFull relative = addSeparator [quizzesFolder, quizPath, relative]

            fullQuizDir = mkFull ""
            fullQuizPath = mkFull roundsFile
            fullLabelPath = mkFull labelsFile
            fullColorsPath = mkFull colorsFile
        writeFile fullQuizPath content
        callProcess pageGenerator 
                    (map (\(k, v) -> mkKV (B.unpack k) v) [(prefix, fullQuizDir), 
                                                           (rounds, fullQuizPath),
                                                           (labels, fullLabelPath),
                                                           (colors, fullColorsPath)])
        return True
    else return False

lockQuiz :: Handler b QuizService ()
lockQuiz = do
    mQuiz <- getPostParam lock
    let act = maybe (pure ()) 
                    (\q -> quizzesFolderIO >>= 
                            \quizzesFolder -> 
                                writeFile (addSeparator [quizzesFolder, B.unpack q, locked]) "") 
                                          mQuiz
    liftIO act
    modifyResponse (setResponseCode 201)

getNonLockedQuizzes :: IO [String]
getNonLockedQuizzes = do
    quizzesFolder <- liftIO quizzesFolderIO
    quizzes <- getDirectoryContents quizzesFolder
    let proper = filter (not . (\x -> x `elem` [".", ".."])) quizzes -- Drops "." and "..".
    filterM isQuizOpen proper

isQuizOpen :: String -> IO Bool
isQuizOpen folder = do
    quizzesFolder <- quizzesFolderIO
    ex <- doesFileExist (addSeparator [quizzesFolder, folder, locked])
    return (not ex)

readQuizFile :: B.ByteString -> IO (Maybe B.ByteString)
readQuizFile quizPath = (do 
    filePath <- filePathIO 
    file <- B.readFile filePath
    return (Just file)) `catch` handle where
    
    handle :: IOException -> IO (Maybe B.ByteString)
    handle _ = filePathIO >>= \filePath -> putStrLn (filePath ++ " does not exist.") 
                          >> return Nothing

    filePathIO :: IO String
    filePathIO = do
        quizzesFolder <- quizzesFolderIO
        return (addSeparator [quizzesFolder, B.unpack quizPath, roundsFile])

quizServiceInit :: SnapletInit b QuizService
quizServiceInit = do
    makeSnaplet "quiz" "Quiz Service" Nothing $ do
        addRoutes quizRoutes
        return QuizService