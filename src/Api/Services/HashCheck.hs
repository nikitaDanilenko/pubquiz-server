{-# Language OverloadedStrings #-}

module Api.Services.HashCheck where

import qualified Data.ByteString.Char8 as B 
import Data.Maybe                           ( maybe )

import Constants                            ( sessionKeysFile )
import Utils                                ( mkHashed, readOrCreateBS )

data Attempt = Attempt {
        user :: B.ByteString,
        arguments :: [B.ByteString],
        claim :: B.ByteString
    }

mkHash :: B.ByteString -> B.ByteString
mkHash = B.pack . show . mkHashed

verifyHash :: Attempt -> B.ByteString -> Bool
verifyHash attempt sessionKey = 
    mkHash (B.concat ([user attempt, sessionKey] ++ arguments attempt)) == claim attempt

toKV :: [B.ByteString] -> (B.ByteString, B.ByteString)
toKV (key : value : _) = (key, value)
toKV _                 = ("", "")

verifyHashFromFile :: Attempt -> IO Bool
verifyHashFromFile attempt = do
    ls <- fmap B.lines (readOrCreateBS sessionKeysFile)
    let kvs = map (toKV . B.words) ls
        mSessionKey = lookup (user attempt) kvs
        verified = maybe False (verifyHash attempt) mSessionKey
    return verified 