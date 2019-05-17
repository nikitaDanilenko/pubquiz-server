{-# Language OverloadedStrings, PackageImports #-}

module Api.Services.SavedUser where

import "cryptonite" Crypto.Hash             ( Digest, hash )
import "cryptonite" Crypto.Hash.Algorithms  ( SHA3_512 )
import qualified Data.ByteString.Char8 as B ( ByteString, pack, concat, unpack )
import Data.Char                            ( chr )
import System.Directory                     ( doesFileExist )
import System.Random                        ( newStdGen, randomRs )

import Constants                            ( userFile, saltSize )
import Utils                                ( readOrCreate )

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
    gen <- newStdGen
    let randomChars = map chr (randomRs (33, 126) gen)
        salt = B.pack (take saltSize randomChars)
        hashValue = mkHash user pass salt
        savedUser = Saved user salt hashValue
    return savedUser

mkAndSaveUser :: UserName -> Password -> IO ()
mkAndSaveUser user pass = do
    text <- readOrCreate useFile
    let ls = lines text
        users = map (read :: String -> SavedUser) ls
        exists = any (\u -> userName u == user) users
    if exists 
        then putStrLn (unwords ["User", B.unpack user, "already exists.", "Nothing changed."])
        else do
            newUser <- mkUser user pass
            let newLs = show newUser : ls
                newText = unlines newLs
            writeFile userFile newText

mkHashed :: UserName -> Password -> Salt -> Hashed
mkHashed user pass salt = hash (B.concat [user, pass, salt])

mkHash :: UserName -> Password -> Salt -> HashValue
mkHash user pass salt = B.pack (show (mkHashed user pass salt))