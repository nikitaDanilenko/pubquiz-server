{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE TypeOperators            #-}

module Api.Auth where

import           Api.BackOffice.Routes  (AuthenticatedUser (..))
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
import           Servant
import           Servant.Auth.Server

-- Login request
data LoginRequest = LoginRequest
  { username :: Text
  , password :: Text
  }
  deriving (Show, Eq, Generic, FromJSON)

-- Login response (JWT token)
newtype LoginResponse = LoginResponse
  { token :: Text
  }
  deriving (Show, Eq, Generic, ToJSON)

-- Auth API (public - for obtaining JWT)
type AuthApi =
  "auth" :> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse

authApi :: Proxy AuthApi
authApi = Proxy

-- Verify password using bcrypt
verifyPassword :: Text -> Text -> Bool
verifyPassword plaintext hash =
  validatePassword (encodeUtf8 hash) (encodeUtf8 plaintext)

-- Login handler
authServer :: [Organizer] -> JWTSettings -> NominalDiffTime -> Server AuthApi
authServer organizers jwtSettings expirationSeconds = login
 where
  login :: LoginRequest -> Handler LoginResponse
  login req = do
    org <- maybe (throwError err401) pure $ find (\o -> o.name == req.username) organizers
    unless (verifyPassword req.password org.passwordHash) $ throwError err401
    now <- liftIO getCurrentTime
    let expiry = addUTCTime expirationSeconds now
        user = AuthenticatedUser { isAdmin = org.isAdmin }
    tokenCandidate <- liftIO $ makeJWT user jwtSettings (Just expiry)
    token <- either (const $ throwError err500) pure tokenCandidate
    pure $ LoginResponse (decodeUtf8 $ LBS.toStrict token)
