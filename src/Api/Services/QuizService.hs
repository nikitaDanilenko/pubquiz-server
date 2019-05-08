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
import System.Directory                     ( doesFileExist, getDirectoryContents )
import System.Process                       ( callProcess )

import Constants                            ( quizzesFolder, locked, addSeparator, lock, quiz,
                                              roundsFile, rounds, pageGenerator, prefix )

data QuizService = QuizService

quizRoutes :: [(B.ByteString, Handler b QuizService ())]
quizRoutes = [
    ("all", method GET sendAvailable),
    ("getQuizData", method GET getSingleQuizData),
    ("update", method POST updateQuiz),
    ("lock", method POST lockQuiz)
    ]

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
        Just _ -> modifyResponse (setResponseCode 200)

mkKV :: String -> String -> String
mkKV key value = concat [key, "=", value]

updateFile :: String -> String -> IO ()
updateFile quizPath content = 
    writeFile fullQuizPath content >>
    callProcess pageGenerator 
                (map (\(k, v) -> mkKV (B.unpack k) v) [(prefix, quizPath), (rounds, fullQuizPath)])

    where fullQuizPath = (addSeparator [quizPath, roundsFile])

lockQuiz :: Handler b QuizService ()
lockQuiz = do
    mQuiz <- getPostParam lock
    let act = maybe (pure ()) 
                    (\q -> writeFile (addSeparator [quizzesFolder, B.unpack q, locked]) "") mQuiz
    liftIO act
    modifyResponse (setResponseCode 201)

getNonLockedQuizzes :: IO [String]
getNonLockedQuizzes = do
    quizzes <- getDirectoryContents quizzesFolder
    let proper = drop 2 quizzes -- Drops "." and "..".
    filterM isQuizOpen proper

isQuizOpen :: String -> IO Bool
isQuizOpen folder = fmap not (doesFileExist (addSeparator [quizzesFolder, folder, locked]))

readQuizFile :: B.ByteString -> IO (Maybe B.ByteString)
readQuizFile quizPath = fmap Just (B.readFile filePath) `catch` handle where
    filePath = (addSeparator [quizzesFolder, B.unpack quizPath, roundsFile])
    
    handle :: IOException -> IO (Maybe B.ByteString)
    handle _ = putStrLn (filePath ++ " does not exist.") >> return Nothing

quizServiceInit :: SnapletInit b QuizService
quizServiceInit = do
    makeSnaplet "quiz" "Quiz Service" Nothing $ do
        addRoutes quizRoutes
        return QuizService