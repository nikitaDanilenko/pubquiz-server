{-# Language OverloadedStrings, PackageImports #-}

module Api.Services.SavedUser where

import Control.Exception                    ( catch )
import Control.Exception.Base               ( IOException )
import "cryptonite" Crypto.Hash             ( Digest, hash )
import "cryptonite" Crypto.Hash.Algorithms  ( SHA3_512 )
import qualified Data.ByteString.Char8 as B ( ByteString, pack, concat, unpack )

import Constants                            ( userFile, saltSize )
import Utils                                ( readOrCreate, randomStringIO )

type Hashed = Digest SHA3_512

type UserName = B.ByteString
type Password = B.ByteString
type Salt = B.ByteString
type HashValue = B.ByteString

data SavedUser = Saved { 
    userName :: UserName, 
    userSalt :: Salt, 
    userHash :: HashValue
} deriving (Show, Read)

instance Eq SavedUser where
    s1 == s2 = userName s1 == userName s2

mkUser :: UserName -> Password -> IO SavedUser
mkUser user pass = do
    randomChars <- randomStringIO
    let salt = B.pack (take saltSize randomChars)
        hashValue = mkHash user pass salt
        savedUser = Saved user salt hashValue
    return savedUser

mkAndSaveUser :: UserName -> Password -> IO Status
mkAndSaveUser user pass = do
    text <- readOrCreate userFile
    let ls = lines text
        users = map (read :: String -> SavedUser) ls
        exists = any (\u -> userName u == user) users
    if exists 
        then let u = B.unpack user
             in do putStrLn (unwords ["User", u, "already exists.", "Nothing changed."])
                   return (Exists user)
        else do
            newUser <- mkUser user pass
            let newLs = show newUser : ls
                newText = unlines newLs
            (writeFile userFile newText >> return Success) `catch` handleWriteFailure

handleWriteFailure :: IOException -> IO Status
handleWriteFailure _ = 
    return (Failure "Something went wrong while writing to user file. Nothing changed.")

data Status = Success | Exists B.ByteString | Failure B.ByteString

mkHashed :: UserName -> Password -> Salt -> Hashed
mkHashed user pass salt = hash (B.concat [user, pass, salt])

mkHash :: UserName -> Password -> Salt -> HashValue
mkHash user pass salt = B.pack (show (mkHashed user pass salt))