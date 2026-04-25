{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE TypeOperators            #-}

module Api.Auth where

import           Api.BackOffice.Types   (AuthenticatedUser (..))
import           Config                 (Organizer (..))
import           Control.Monad          (unless)
import           Control.Monad.IO.Class (liftIO)
import           Crypto.BCrypt          (validatePassword)
import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy   as LBS
import           Data.List              (find)
import           Data.Text              (Text)
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Data.Time              (NominalDiffTime, addUTCTime,
                                         getCurrentTime)
import           GHC.Generics           (Generic)
import           Servant                (Handler, JSON, Post, Proxy (..),
                                         ReqBody, Server, err401, err500,
                                         throwError, (:>))
import           Servant.Auth.Server    (JWTSettings, makeJWT)

data LoginRequest = LoginRequest
  { username :: Text
  , password :: Text
  }
  deriving (Show, Eq, Generic, FromJSON)

newtype LoginResponse = LoginResponse
  { token :: Text
  }
  deriving (Show, Eq, Generic, ToJSON)

type AuthApi =
  "backoffice" :> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse

authApi :: Proxy AuthApi
authApi = Proxy

verifyPassword :: Text -> Text -> Bool
verifyPassword plaintext hash =
  validatePassword (encodeUtf8 hash) (encodeUtf8 plaintext)

-- Login handler
authServer :: [Organizer] -> JWTSettings -> NominalDiffTime -> Server AuthApi
authServer organizers jwtSettings expirationSeconds = login
 where
  login :: LoginRequest -> Handler LoginResponse
  login req = do
    organizer <- maybe (throwError err401) pure $ find (\o -> o.name == req.username) organizers
    unless (verifyPassword req.password organizer.passwordHash) $ throwError err401
    now <- liftIO getCurrentTime
    let expiry = addUTCTime expirationSeconds now
        user = AuthenticatedUser { isAdmin = organizer.isAdmin }
    tokenCandidate <- liftIO $ makeJWT user jwtSettings (Just expiry)
    token <- either (const $ throwError err500) pure tokenCandidate
    pure $ LoginResponse (decodeUtf8 $ LBS.toStrict token)
