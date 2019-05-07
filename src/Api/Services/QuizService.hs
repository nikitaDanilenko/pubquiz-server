{-# Language OverloadedStrings, FlexibleInstances, PackageImports #-}

module Api.Services.QuizService where

import Control.Exception                    ( catch )
import Control.Exception.Base               ( IOException )
import Control.Monad                        ( filterM )
import Control.Monad.IO.Class               ( liftIO )
import qualified Data.ByteString.Char8 as B 
import Data.Maybe                           ( maybe )
import Snap.Core                            ( method, Method ( GET, POST ), writeBS, modifyResponse,
                                              setResponseCode, getPostParam )
import Snap.Snaplet                         ( Handler, SnapletInit, addRoutes, makeSnaplet )
import System.Directory                     ( doesFileExist, getDirectoryContents )

import Constants                            ( quizzesFolder, locked, addSeparator, lock, quiz,
                                              roundsFile )

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
    liftIO (writeFile "foo.txt" (unlines nonLockedQuizzes))
    writeBS (B.pack (unlines nonLockedQuizzes))
    modifyResponse (setResponseCode 200)

getSingleQuizData :: Handler b QuizService ()
getSingleQuizData = getPostParam quiz >>= maybe (modifyResponse (setResponseCode 400)) perQuiz where
    
    perQuiz :: B.ByteString -> Handler b QuizService ()
    perQuiz q = liftIO (readQuizFile q) 
                    >>= maybe (modifyResponse (setResponseCode 404)) 
                              (\c -> writeBS c >> modifyResponse (setResponseCode 200))
        
updateQuiz :: Handler b QuizService ()
updateQuiz = undefined

lockQuiz :: Handler b QuizService ()
lockQuiz = do
    quiz <- getPostParam lock
    let act = maybe (pure ()) (\q -> writeFile (addSeparator [quizzesFolder, B.unpack q]) "") quiz
    liftIO act
    modifyResponse (setResponseCode 201)

getNonLockedQuizzes :: IO [String]
getNonLockedQuizzes = do
    quizzes <- getDirectoryContents quizzesFolder
    let proper = drop 2 quizzes -- Drops "." and "..".
    filterM isQuizOpen proper

isQuizOpen :: String -> IO Bool
isQuizOpen folder = fmap not (doesFileExist (addSeparator [folder, locked]))

readQuizFile :: B.ByteString -> IO (Maybe B.ByteString)
readQuizFile quiz = fmap Just (B.readFile filePath) `catch` handle where
    filePath = (addSeparator [quizzesFolder, B.unpack quiz, B.unpack roundsFile])
    
    handle :: IOException -> IO (Maybe B.ByteString)
    handle _ = putStrLn (filePath ++ " does not exist.") >> return Nothing

quizServiceInit :: SnapletInit b QuizService
quizServiceInit = do
    makeSnaplet "quiz" "Quiz Service" Nothing $ do
        addRoutes quizRoutes
        return QuizService