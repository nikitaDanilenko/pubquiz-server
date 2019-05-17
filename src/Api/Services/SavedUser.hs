{-# Language OverloadedStrings, PackageImports #-}

module Api.Services.SavedUser where

import "cryptonite" Crypto.Hash             ( Digest, hash )
import "cryptonite" Crypto.Hash.Algorithms  ( SHA3_512 )
import qualified Data.ByteString.Char8 as B ( ByteString, pack, concat )
import System.Random                        ( newStdGen, randoms )

type Hashed = Digest SHA3_512

type UserName = B.ByteString
type Password = B.ByteString
type Salt = B.ByteString
type HashValue = B.ByteString

data SavedUser = Saved { 
    userName :: UserName, 
    salt :: Salt, 
    hashValue :: HashValue
} deriving (Show, Read)

instance Eq SavedUser where
    s1 == s2 = userName s1 == userName s2

mkUser :: UserName -> Password -> IO SavedUser
mkUser user pass = do
    gen <- newStdGen
    let randomChars = (randoms gen) :: String
        salt = B.pack randomChars
        hashValue = mkHash user pass salt
        savedUser = Saved user salt hashValue
    return savedUser

mkHashed :: UserName -> Password -> Salt -> Hashed
mkHashed user pass salt = hash (B.concat [user, pass, salt])

mkHash :: UserName -> Password -> Salt -> HashValue
mkHash user pass salt = B.pack (show (mkHashed user pass salt))