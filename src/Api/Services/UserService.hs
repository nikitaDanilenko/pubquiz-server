{-# Language OverloadedStrings, FlexibleInstances #-}

module Api.Services.UserService where

import qualified Data.ByteString.Char8 as B 

import Api.Services.SavedUser               ( mkAndSaveUser, Status ( .. ) )
import Constants                            ( userParam, passwordParam )

data UserService = UserService

userRoutes :: [(B.ByteString, Handler b UserService ())]
userRoutes = ["createUser" +> method POST createUser]

quizServiceInit :: SnapletInit b QuizService
quizServiceInit = do
    makeSnaplet "users" "User Service" Nothing $ do
        addRoutes userRoutes
        return UserService

createUser :: Handler b QuizService ()
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