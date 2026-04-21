{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api.Auth where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant
import Servant.Auth.Server

-- Login request
data LoginRequest = LoginRequest
  { username :: Text,
    password :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON LoginRequest

-- Login response (JWT token)
data LoginResponse = LoginResponse
  { token :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON LoginResponse

-- Auth API (public - for obtaining JWT)
type AuthApi =
  "auth" :> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse

authApi :: Proxy AuthApi
authApi = Proxy

-- Handler (placeholder)
authServer :: CookieSettings -> JWTSettings -> Server AuthApi
authServer cookieSettings jwtSettings = login
  where
    login :: LoginRequest -> Handler LoginResponse
    login = undefined
