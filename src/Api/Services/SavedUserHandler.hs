{-# LANGUAGE OverloadedStrings #-}

module Api.Services.SavedUserHandler (mkHash, mkAndSaveUser) where

import           Control.Exception      (catch)
import           Control.Exception.Base (IOException)
import qualified Data.ByteString.Char8  as B (ByteString, concat, pack, unpack)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as E

import           Constants              (saltSize, userFileIO)
import           Db.DbConversion        (SavedUser (SavedUser), userName)
import           Db.Storage             (findUser, setUser)
import           General.Types          (UserHash, UserName, UserSalt, unwrap,
                                         wrap)
import           Utils                  (Hashed, mkHashed, randomStringIO,
                                         readOrCreateEmpty)

type Password = B.ByteString

mkUser :: UserName -> Password -> IO SavedUser
mkUser user pass = do
  randomChars <- randomStringIO
  let salt = wrap (T.pack (take saltSize randomChars))
      hashValue = mkHash user pass salt
      savedUser = SavedUser user salt hashValue
  return savedUser

mkAndSaveUser :: UserName -> Password -> IO Status
mkAndSaveUser user pass = do
  mDbUser <- findUser user
  case mDbUser of
    Just _ -> do
      putStrLn (unwords ["User", T.unpack (unwrap user), "already exists.", "Nothing changed."])
      pure (Exists user)
    Nothing -> do
      newUser <- mkUser user pass
      setUser newUser
      pure Success

data Status
  = Success
  | Exists UserName

mkUserHashed :: UserName -> Password -> UserSalt -> Hashed
mkUserHashed user pass salt = mkHashed (E.encodeUtf8 (T.concat [unwrap user, E.decodeUtf8 pass, unwrap salt]))

mkHash :: UserName -> Password -> UserSalt -> UserHash
mkHash user pass salt = wrap (T.pack (show (mkUserHashed user pass salt)))