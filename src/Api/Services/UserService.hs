{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Services.UserService where

import           Control.Monad.IO.Class        (liftIO)
import qualified Data.ByteString.Char8         as B
import qualified Data.Text.Encoding            as E
import           Snap.Core                     (Method (POST), getPostParam,
                                                method, modifyResponse,
                                                setResponseCode, writeBS)
import           Snap.Snaplet                  (Handler, SnapletInit, addRoutes,
                                                makeSnaplet)

import           Api.Services.HashCheck        (authenticate,
                                                failIfUnverified)
import           Api.Services.SavedUserHandler (Status (..), mkAndSaveUser)
import           Api.Services.SnapUtil         (attemptDecode)
import           Constants                     (credentialsParam, newUserParam,
                                                passwordParam, userParam,
                                                userPath)
import           General.Types                 (Password, UserName, unwrap)
import           Utils                         ((+>))

data UserService =
  UserService

userRoutes :: [(B.ByteString, Handler b UserService ())]
userRoutes = ["createUser" +> method POST createUser]

userServiceInit :: SnapletInit b UserService
userServiceInit =
  makeSnaplet userPath "User Service" Nothing $ do
    addRoutes userRoutes
    return UserService

createUser :: Handler b UserService ()
createUser = do
  mUser <- attemptDecode (getPostParam userParam) :: Handler b UserService (Maybe UserName)
  mNewUser <- attemptDecode (getPostParam newUserParam) :: Handler b UserService (Maybe UserName)
  mCredentials <- attemptDecode (getPostParam credentialsParam)
  mNewPass <- attemptDecode (getPostParam passwordParam) :: Handler b UserService (Maybe Password)
  verified <-
    authenticate mCredentials [(newUserParam, unwrap mNewUser), (passwordParam, unwrap mNewPass)]
  failIfUnverified verified $
    case (mNewUser, mNewPass) of
      (Just user, Just pass) -> do
        status <- liftIO (mkAndSaveUser user pass)
        case status of
          Success -> do
            writeBS (B.unwords ["Created user", unwrap user])
            modifyResponse (setResponseCode 201)
          Exists u -> do
            writeBS (B.unwords ["User", unwrap u, "already exists. Nothing changed."])
            modifyResponse (setResponseCode 406)
      _ -> do
        writeBS "User or password missing. Nothing changed."
        modifyResponse (setResponseCode 406)
