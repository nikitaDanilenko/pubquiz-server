{-# LANGUAGE TypeOperators #-}

module Api where

import           Api.Auth              (AuthApi, authServer)
import           Api.BackOffice.Routes (BackOfficeApi, backOfficeServer)
import           Api.Health            (HealthApi, healthServer)
import           Api.OpenApi           (OpenApiApi, openApiServer)
import           Api.Public.Routes     (PublicApi, publicServer)
import           Config                (Organizer)
import           Data.Pool             (Pool)
import           Data.Time             (NominalDiffTime)
import           Database.Persist.Sql  (SqlBackend)
import           Servant
import           Servant.Auth.Server

type Api =
  OpenApiApi
    :<|> HealthApi
    :<|> PublicApi
    :<|> BackOfficeApi
    :<|> AuthApi

api :: Proxy Api
api = Proxy

server :: Pool SqlBackend -> [Organizer] -> CookieSettings -> JWTSettings -> NominalDiffTime -> Server Api
server pool organizers cookieSettings jwtSettings jwtExpiration =
  openApiServer
    :<|> healthServer
    :<|> publicServer pool
    :<|> backOfficeServer pool
    :<|> authServer organizers cookieSettings jwtSettings jwtExpiration
