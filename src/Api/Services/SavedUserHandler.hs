{-# LANGUAGE OverloadedStrings #-}

module Api.Services.SavedUserHandler
  ( mkHash
  , mkAndSaveUser
  , Password
  ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as L
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as E

import           Constants             (saltSize)
import           Db.DbConversion       (SavedUser (SavedUser))
import           Db.Storage            (findUser, setUser)
import           General.Types         (Password, UserCreation (UserCreation),
                                        UserHash, UserName, UserSalt, unwrap,
                                        wrap)
import           Utils                 (Hashed, mkHashed, randomStringIO)

mkUser :: UserName -> Password -> IO SavedUser
mkUser user pass = do
  randomChars <- randomStringIO
  let salt = wrap (take saltSize randomChars)
      hashValue = mkHash user pass salt
      savedUser = SavedUser user salt hashValue
  return savedUser

mkAndSaveUser :: UserCreation -> IO (Either L.ByteString ())
mkAndSaveUser (UserCreation user pass) = do
  mDbUser <- findUser user
  case mDbUser of
    Just _ -> pure (Left (L.fromStrict (B.unwords ["User", unwrap user, "already exists.", "Nothing changed."])))
    Nothing -> do
      newUser <- mkUser user pass
      setUser newUser
      pure (Right ())

mkUserHashed :: UserName -> Password -> UserSalt -> Hashed
mkUserHashed user pass salt = mkHashed (E.encodeUtf8 (T.concat [unwrap user, unwrap pass, unwrap salt]))

mkHash :: UserName -> Password -> UserSalt -> UserHash
mkHash user pass salt = wrap (show (mkUserHashed user pass salt))
