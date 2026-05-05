{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main where

import           Api                         (api, server)
import           Config                      (Config (..), CookieConfig (..),
                                              DatabaseConfig (..),
                                              JwtConfig (..), loadConfig)
import           Control.Monad.Logger        (runStdoutLoggingT)
import           Control.Monad.Reader        (runReaderT)
import           Data.Pool                   (Pool, withResource)
import           Data.Text                   (Text, pack, unpack)
import           Data.Text.Encoding          (encodeUtf8)
import           Data.Time                   (nominalDiffTimeToSeconds,
                                              secondsToDiffTime,
                                              secondsToNominalDiffTime)
import           Database.Persist.Postgresql (createPostgresqlPool)
import           Database.Persist.Sql        (SqlBackend, runMigration)
import           Db.Schema                   (migrateAll)
import           Network.Wai.Handler.Warp    (run)
import           Servant                     (Context (..), serveWithContext)
import           Servant.Auth.Server         (CookieSettings (..),
                                              IsSecure (..), SameSite (..),
                                              defaultCookieSettings,
                                              defaultJWTSettings, fromSecret)

main :: IO ()
main = do
  config <- loadConfig
  pool <- createPool config.database

  -- Run migrations
  withResource pool $ runReaderT $ runMigration migrateAll

  -- Create JWT settings from secret
  let jwtKey = fromSecret (encodeUtf8 config.jwt.secret)
      jwtSettings = defaultJWTSettings jwtKey
      jwtExpiration = secondsToNominalDiffTime (fromIntegral config.jwt.expirationSeconds)
      cookieMaxAgeSeconds = floor $ nominalDiffTimeToSeconds jwtExpiration
      cookieSettings = defaultCookieSettings
        { cookieMaxAge = Just $ secondsToDiffTime cookieMaxAgeSeconds
        , cookieIsSecure = if config.cookie.secure then Secure else NotSecure
        , cookieSameSite = parseSameSite config.cookie.sameSite
        , cookieXsrfSetting = Nothing
        }
      ctx = cookieSettings :. jwtSettings :. EmptyContext
      appPort = fromIntegral config.port

  putStrLn $ unwords ["Starting server on port", show appPort]
  run appPort $ serveWithContext api ctx (server pool config.organizers cookieSettings jwtSettings jwtExpiration)

createPool :: DatabaseConfig -> IO (Pool SqlBackend)
createPool dbConfig = runStdoutLoggingT $ do
  let connStr = mconcat
        [ "host=", unpack dbConfig.host
        , " dbname=", unpack dbConfig.name
        , " user=", unpack dbConfig.user
        , " password=", unpack dbConfig.password
        , " port=", show dbConfig.port
        ]
  createPostgresqlPool (encodeUtf8 $ pack connStr) 10

parseSameSite :: Text -> SameSite
parseSameSite "lax"  = SameSiteLax
parseSameSite "none" = AnySite
parseSameSite _      = SameSiteStrict
