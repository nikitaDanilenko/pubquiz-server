{-# Language OverloadedStrings, TemplateHaskell #-}

module Api.Core where

import           Control.Lens                ( makeLenses )
import qualified Data.ByteString.Char8 as B
import           Snap.Core
import           Snap.Snaplet

import           Api.Services.EstablishSecret
import           Api.Services.QuizService

data Api = Api { _secretService :: Snaplet SecretService,
                 _quizService :: Snaplet QuizService }

makeLenses ''Api

apiRoutes :: [(B.ByteString, Handler b Api ())]
apiRoutes = [("status", method GET respondOk)]

respondOk :: Handler b Api ()
respondOk = modifyResponse $ setResponseCode 200

apiInit :: SnapletInit b Api
apiInit = makeSnaplet "api" "Core Api" Nothing $ do
    secretService <- nestSnaplet "secrets" secretService secretServiceInit
    quizService <- nestSnaplet "quiz" quizService quizServiceInit
    addRoutes apiRoutes
    return $ Api secretService quizService