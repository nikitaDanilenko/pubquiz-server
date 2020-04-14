{-# LANGUAGE OverloadedStrings #-}

module Api.Services.SavedUserHandler
  ( mkHash
  , mkAndSaveUser
  , Status(..)
  , Password
  ) where

import qualified Data.Text              as T
import qualified Data.Text.Encoding     as E

import           Constants              (saltSize)
import           Db.DbConversion        (SavedUser (SavedUser))
import           Db.Storage             (findUser, setUser)
import           General.Types          (Password, UserCreation (UserCreation),
                                         UserHash, UserName, UserSalt, unwrap,
                                         wrap)
import           Utils                  (Hashed, mkHashed, randomStringIO)

mkUser :: UserName -> Password -> IO SavedUser
mkUser user pass = do
  randomChars <- randomStringIO
  let salt = wrap (take saltSize randomChars)
      hashValue = mkHash user pass salt
      savedUser = SavedUser user salt hashValue
  return savedUser

mkAndSaveUser :: UserCreation -> IO Status
mkAndSaveUser (UserCreation user pass) = do
  mDbUser <- findUser user
  case mDbUser of
    Just _ -> do
      putStrLn (unwords ["User", unwrap user, "already exists.", "Nothing changed."])
      pure (Exists user)
    Nothing -> do
      newUser <- mkUser user pass
      setUser newUser
      pure Success

data Status
  = Success
  | Exists UserName

mkUserHashed :: UserName -> Password -> UserSalt -> Hashed
mkUserHashed user pass salt = mkHashed (E.encodeUtf8 (T.concat [unwrap user, unwrap pass, unwrap salt]))

mkHash :: UserName -> Password -> UserSalt -> UserHash
mkHash user pass salt = wrap (show (mkUserHashed user pass salt))
