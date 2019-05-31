{-# Language OverloadedStrings, FlexibleInstances #-}

module Api.Services.UserService where

import Control.Monad.IO.Class               ( liftIO )
import qualified Data.ByteString.Char8 as B 
import Snap.Core                            ( method, Method ( POST ), writeBS, modifyResponse,
                                              setResponseCode, getPostParam )
import Snap.Snaplet                         ( Handler, SnapletInit, addRoutes, makeSnaplet )

import Api.Services.SavedUser               ( mkAndSaveUser, Status ( .. ) )
import Constants                            ( userParam, passwordParam, userPath )
import Utils                                ( (+>) )

data UserService = UserService

userRoutes :: [(B.ByteString, Handler b UserService ())]
userRoutes = ["createUser" +> method POST createUser]

userServiceInit :: SnapletInit b UserService
userServiceInit = do
    makeSnaplet userPath "User Service" Nothing $ do
        addRoutes userRoutes
        return UserService

createUser :: Handler b UserService ()
createUser = do
  mUser <- getPostParam userParam
  mPass <- getPostParam passwordParam
  case (mUser, mPass) of
    (Just user, Just pass) -> do
      status <- liftIO (mkAndSaveUser user pass)
      case status of
        Success -> do writeBS (B.unwords ["Created user", user]) 
                      modifyResponse (setResponseCode 201)
        Exists u -> do writeBS (B.unwords ["User", u, "already exists. Nothing changed."])
                       modifyResponse (setResponseCode 406)
        Failure f -> do writeBS (B.unwords ["Error", f]) 
                        modifyResponse (setResponseCode 500)
    _ -> do writeBS "User or password missing. Nothing changed."
            modifyResponse (setResponseCode 406)        