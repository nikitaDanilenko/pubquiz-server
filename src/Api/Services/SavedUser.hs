{-# Language PackageImports #-}

module Api.Services.SavedUser where

import "cryptonite" Crypto.Hash            ( Digest )
import "cryptonite" Crypto.Hash.Algorithms ( SHA3_512 )

type Hashed = Digest SHA3_512

data SavedUser = Saved { userName :: String, salt :: String, hashValue :: String }
    deriving (Show, Read)

instance Eq SavedUser where
    s1 == s2 = userName s1 == userName s2