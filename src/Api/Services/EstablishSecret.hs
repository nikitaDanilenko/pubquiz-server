{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Services.EstablishSecret
  ( SecretService
  , secretServiceInit
  ) where

import           Control.Monad.IO.Class
import           Crypto.PubKey.RSA             (PublicKey, generate)
import qualified Data.ByteString.Char8         as B
import           Data.Maybe                    (fromMaybe)
import qualified Data.Text                     as T
import           Snap.Core                     hiding (pass)
import           Snap.Snaplet

import           Api.Services.SavedUserHandler (Password, mkHash)
import           Api.Services.SnapUtil         (attemptDecode)
import           Constants                     (keySize, oneWayHashSize,
                                                passwordParam, publicExponent,
                                                sessionKeysFileIO, userFileIO,
                                                userParam)
import           Control.Applicative           (liftA2)
import           Data.Aeson                    (encode)
import           Db.DbConversion               (SavedUser, userHash, userName,
                                                userSalt)
import           Db.Storage                    (findUser, setSessionKey)
import           General.Types                 (UserHash, UserName, wrap)
import           Utils                         (randomStringIO,
                                                readOrCreateEmptyBS, (+>))

data SecretService =
  SecretService

secretRoutes :: [(B.ByteString, Handler b SecretService ())]
secretRoutes = ["/" +> method POST createSecret]

createSecret :: Handler b SecretService ()
createSecret = do
  mUser <- attemptDecode (getPostParam userParam)
  mPass <- getPostParam passwordParam
  valid <- liftIO $ fromMaybe (pure False) (liftA2 verifyUser mUser mPass)
  if valid
    then do
      oneWayHash <- liftIO (createAndWriteHash (fromMaybe (error "Non-existing user (should never occur)") mUser))
      writeLBS (encode oneWayHash)
      modifyResponse (setResponseCode 201)
    else do
      writeBS authenticationError
      modifyResponse (setResponseCode 401)

authenticationError :: B.ByteString
authenticationError = B.pack "Failed to authenticate: user name or password is wrong."

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
