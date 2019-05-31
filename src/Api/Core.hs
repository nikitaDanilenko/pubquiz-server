{-# Language OverloadedStrings, TemplateHaskell #-}

module Api.Core ( Api, apiInit ) where

import           Control.Lens                 ( makeLenses )
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T               ( Text )
import qualified Data.Text.Encoding as E      ( encodeUtf8 )
import           Snap.Core
import           Snap.Snaplet

import           Api.Services.EstablishSecret ( SecretService, secretServiceInit )
import           Api.Services.QuizService     ( QuizService, quizServiceInit ) 
import           Api.Services.UserService     ( UserService, userServiceInit )
import           Constants                    ( apiPath, secretPath, quizPath, userPath )

data Api = Api { _secretService :: Snaplet SecretService,
                 _quizService :: Snaplet QuizService ,
                 _userService :: Snaplet UserService
               }

makeLenses ''Api

apiRoutes :: [(B.ByteString, Handler b Api ())]
apiRoutes = [("status", method GET respondOk)]

respondOk :: Handler b Api ()
respondOk = modifyResponse $ setResponseCode 200

asBS :: T.Text -> B.ByteString
asBS = E.encodeUtf8

apiInit :: SnapletInit b Api
apiInit = makeSnaplet apiPath "Core Api" Nothing $ do
    secretServiceInstance <- nestSnaplet (asBS secretPath) secretService secretServiceInit
    quizServiceInstance <- nestSnaplet (asBS quizPath) quizService quizServiceInit
    userServiceInstance <- nestSnaplet (asBS userPath) userService userServiceInit
    addRoutes apiRoutes
    return $ Api secretServiceInstance quizServiceInstance userServiceInstance