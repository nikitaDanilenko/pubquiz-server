{-# Language OverloadedStrings, FlexibleInstances, PackageImports #-}

module Api.Services.QuizService where

import Control.Monad                        ( filterM )
import Control.Monad.IO.Class               ( liftIO )
import qualified Data.ByteString.Char8 as B 
import Data.Maybe                           ( maybe )
import Snap.Core                            ( method, Method ( GET, POST ), writeBS, modifyResponse,
                                              setResponseCode, getPostParam )
import Snap.Snaplet                         ( Handler )
import System.Directory                     ( doesFileExist, getDirectoryContents )

import Constants                            ( quizzesFolder, locked, addSeparator, lock )

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
getSingleQuizData = undefined

updateQuiz :: Handler b QuizService ()
updateQuiz = undefined

lockQuiz :: Handler b QuizService ()
lockQuiz = do
    quiz <- getPostParam lock
    let act = maybe (pure ()) (\q -> writeFile (addSeparator [quizzesFolder, B.unpack q]) "") quiz
    liftIO act
    modifyResponse (setResponseCode 200)

getNonLockedQuizzes :: IO [String]
getNonLockedQuizzes = do
    quizzes <- getDirectoryContents quizzesFolder
    let proper = drop 2 quizzes -- Drops "." and "..".
    filterM isQuizOpen proper

isQuizOpen :: String -> IO Bool
isQuizOpen folder = doesFileExist (addSeparator [folder, locked])