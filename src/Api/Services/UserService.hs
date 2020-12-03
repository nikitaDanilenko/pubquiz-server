{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Services.UserService where

import           Control.Monad.IO.Class        (liftIO)
import qualified Data.ByteString.Char8         as B
import           Snap.Core                     (Method (POST), method)
import           Snap.Snaplet                  (Handler, SnapletInit, addRoutes,
                                                makeSnaplet)

import           Api.Services.HashCheck        (authenticate)
import           Api.Services.SavedUserHandler (mkAndSaveUser)
import           Api.Services.SnapUtil         (readBody, readCredentials, jsonResponseCode, errorWithCode, Parsed (Parsed))
import           Constants                     (createUserApi, userPath)
import           Control.Monad.Trans.Except    (ExceptT (ExceptT))
import           General.EitherT.Extra         (exceptValueOr)
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
createUser = exceptValueOr transformer (errorWithCode 500)
  where
    transformer = do
      credentials <- readCredentials
      Parsed userCreationBS userCreation <- readBody
      authenticate credentials userCreationBS
      ExceptT (liftIO (mkAndSaveUser userCreation))
      jsonResponseCode 201
