{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Services.EstablishSecret
  ( SecretService
  , secretServiceInit
  ) where

import           Api.Requests.SecretRequest    (SecretRequest (SecretRequest))
import           Api.Services.SavedUserHandler (Password, mkHash)
import           Api.Services.SnapUtil         (errorWithCode, okJsonResponse,
                                                parsedJson, readBody)
import           Constants                     (oneWayHashSize, secretApi)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except    (ExceptT (ExceptT))
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy          as L
import qualified Data.Text                     as T
import           Db.DbConversion               (SavedUser, userHash, userName,
                                                userSalt)
import           Db.Storage                    (findUser, setSessionKey)
import           General.EitherT.Extra         (exceptValueOr)
import           General.Types                 (UserHash, UserName, wrap)
import           Snap.Core
import           Snap.Snaplet
import           Utils                         (randomStringIO, (+>))

data SecretService =
  SecretService

secretRoutes :: [(B.ByteString, Handler b SecretService ())]
secretRoutes = [secretApi +> method POST createSecret]

createSecret :: Handler b SecretService ()
createSecret = exceptValueOr transformer (errorWithCode 401)
  where
    transformer = do
      parsed <- readBody
      let SecretRequest userName password = parsedJson parsed
      ExceptT $
        fmap
          (\r ->
             if r
               then Right ()
               else Left authenticationError)
          (liftIO (verifyUser userName password))
      oneWayHash <- liftIO (createAndWriteHash userName)
      okJsonResponse oneWayHash

authenticationError :: L.ByteString
authenticationError = L.fromStrict (B.pack "Failed to authenticate: user name or password is wrong.")

verifyUser :: UserName -> Password -> IO Bool
verifyUser user pass = do
  mUser <- findUser user
  case mUser of
    Nothing   -> pure False
    Just user -> pure (verifyPassword pass user)

verifyPassword :: Password -> SavedUser -> Bool
verifyPassword password savedUser = hash == userHash savedUser
  where
    hash = mkHash (userName savedUser) password (userSalt savedUser)

createAndWriteHash :: UserName -> IO UserHash
createAndWriteHash user = do
  oneWayHash <- fmap (wrap . T.pack . take oneWayHashSize) randomStringIO
  setSessionKey user oneWayHash
  pure oneWayHash

secretServiceInit :: SnapletInit b SecretService
secretServiceInit =
  makeSnaplet "secret" "Secret Service" Nothing $ do
    addRoutes secretRoutes
    return SecretService
