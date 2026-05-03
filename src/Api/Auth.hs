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
import           Data.Aeson             (FromJSON)
import           Data.List              (find)
import           Data.Text              (Text)
import           Data.Text.Encoding     (encodeUtf8)
import           Data.Time              (NominalDiffTime)
import           GHC.Generics           (Generic)
import           Servant                (Handler, Header, Headers, JSON,
                                         NoContent (..), Post, Proxy (..),
                                         ReqBody, Server, StdMethod (POST),
                                         Verb, err401, err500, throwError, (:>))
import           Servant.Auth.Server    (CookieSettings, JWTSettings, SetCookie,
                                         acceptLogin)

data LoginRequest = LoginRequest
  { username :: Text
  , password :: Text
  }
  deriving (Show, Eq, Generic, FromJSON)

-- Auth Cookie + CSRF cookie
type LoginHeaders = '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]

-- The response type is explicit, because we need the same workaround as in BackOfficeApi for the OpenAPI generation.
-- At the same time, we need to have the headers in the type, too.
type AuthApi =
  "backoffice" :> "login" :> ReqBody '[JSON] LoginRequest :> Verb 'POST 204 '[JSON] (Headers LoginHeaders NoContent)

authApi :: Proxy AuthApi
authApi = Proxy

verifyPassword :: Text -> Text -> Bool
verifyPassword plaintext hash =
  validatePassword (encodeUtf8 hash) (encodeUtf8 plaintext)

authServer :: [Organizer] -> CookieSettings -> JWTSettings -> NominalDiffTime -> Server AuthApi
authServer organizers cookieSettings jwtSettings _expirationSeconds = login
 where
  login :: LoginRequest -> Handler (Headers LoginHeaders NoContent)
  login req = do
    organizer <- maybe (throwError err401) pure $ find (\o -> o.name == req.username) organizers
    unless (verifyPassword req.password organizer.passwordHash) $ throwError err401
    let user = AuthenticatedUser { isAdmin = organizer.isAdmin }
    applyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user
    maybe (throwError err500) (\addCookies -> pure $ addCookies NoContent) applyCookies
