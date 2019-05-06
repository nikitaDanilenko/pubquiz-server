{-# Language OverloadedStrings, PackageImports #-}

module Api.Services.SavedUser where

import "cryptonite" Crypto.Hash             ( Digest )
import "cryptonite" Crypto.Hash.Algorithms  ( SHA3_512 )
import qualified Data.ByteString.Char8 as B ( ByteString )

type Hashed = Digest SHA3_512

data SavedUser = Saved { 
    userName :: B.ByteString, 
    salt :: B.ByteString, 
    hashValue :: B.ByteString 
} deriving (Show, Read)

instance Eq SavedUser where
    s1 == s2 = userName s1 == userName s2