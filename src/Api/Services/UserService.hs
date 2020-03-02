{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Services.UserService where

import           Control.Monad.IO.Class        (liftIO)
import qualified Data.ByteString.Char8         as B
import           Snap.Core                     (Method (POST), method,
                                                modifyResponse, setResponseCode,
                                                writeBS)
import           Snap.Snaplet                  (Handler, SnapletInit, addRoutes,
                                                makeSnaplet)

import           Api.Services.HashCheck        (authenticate, failIfUnverified)
import           Api.Services.SavedUserHandler (Status (..), mkAndSaveUser)
import           Api.Services.SnapUtil         (fKey, getJSONPostParam,
                                                getJSONPostParamWithPure)
import           Constants                     (createUserApi, credentialsParam,
                                                userCreationParam, userPath)
import           General.Types                 (UserCreation, unwrap,
                                                userCreationUser)
import           Utils                         ((+>))

data UserService =
  UserService

userRoutes :: [(B.ByteString, Handler b UserService ())]
userRoutes = [createUserApi +> method POST createUser]

userServiceInit :: SnapletInit b UserService
userServiceInit =
  makeSnaplet userPath "User Service" Nothing $ do
    addRoutes userRoutes
    return UserService

createUser :: Handler b UserService ()
createUser = do
  mCredentials <- getJSONPostParam credentialsParam
  mUserCreationWithText <- getJSONPostParamWithPure userCreationParam
  verified <- authenticate mCredentials [(userCreationParam, fKey mUserCreationWithText)]
  failIfUnverified verified $
    case mUserCreationWithText of
      Just (_, userCreation) -> do
        status <- liftIO (mkAndSaveUser userCreation)
        case status of
          Success -> do
            writeBS (B.unwords ["Created user", unwrap (userCreationUser userCreation)])
            modifyResponse (setResponseCode 201)
          Exists u -> do
            writeBS (B.unwords ["User", unwrap u, "already exists. Nothing changed."])
            modifyResponse (setResponseCode 406)
      _ -> do
        writeBS "User or password missing. Nothing changed."
        modifyResponse (setResponseCode 406)
