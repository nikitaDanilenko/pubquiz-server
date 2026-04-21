{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Api.Auth              (AuthApi, authServer)
import           Api.BackOffice.Routes (BackOfficeApi, backOfficeServer)
import           Api.Public.Routes     (PublicApi, publicServer)
import           Data.Pool             (Pool)
import           Database.Persist.Sql  (SqlBackend)
import           Servant
import           Servant.Auth.Server

-- Combined API
type Api =
  PublicApi
    :<|> BackOfficeApi
    :<|> AuthApi

api :: Proxy Api
api = Proxy

-- Combined server
server :: Pool SqlBackend -> CookieSettings -> JWTSettings -> Server Api
server pool cs jwts =
  publicServer
    :<|> backOfficeServer pool
    :<|> authServer cs jwts
