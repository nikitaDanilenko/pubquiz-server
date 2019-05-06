{-# Language OverloadedStrings, FlexibleInstances, PackageImports #-}

module Api.Services.QuizService where

import qualified Data.ByteString.Char8 as B 
import Snap.Core                            ( method, Method ( GET, POST ) )
import Snap.Snaplet                         ( Handler )

data QuizService = QuizService

quizRoutes :: [(B.ByteString, Handler b QuizService ())]
quizRoutes = [
    ("all", method GET sendAvailable),
    ("update", method POST updateQuiz),
    ("lock", method POST lockQuiz)
    ]

sendAvailable :: Handler b QuizService ()
sendAvailable = undefined

updateQuiz :: Handler b QuizService ()
updateQuiz = undefined

lockQuiz :: Handler b QuizService ()
lockQuiz = undefined