{-# Language OverloadedStrings #-}

module Api.Services.HashCheck where

import Control.Arrow                        ( second )
import Control.Monad.IO.Class               ( liftIO )  
import qualified Data.ByteString.Char8 as B 
import Data.Maybe                           ( maybe, fromMaybe )
import Snap.Core                            ( writeBS, modifyResponse, setResponseCode )
import Snap.Snaplet                         ( Handler )

import Constants                            ( sessionKeysFileIO, userParam )
import Utils                                ( mkHashed, readOrCreateEmptyBS )

data Attempt = Attempt {
        user :: B.ByteString,
        arguments :: [(B.ByteString, B.ByteString)],
        claim :: B.ByteString
    }

mkAttempt :: B.ByteString -> B.ByteString -> [(B.ByteString, B.ByteString)] -> Attempt
mkAttempt u cl kvs = Attempt u kvs cl

mkAttemptWithMaybe :: Maybe B.ByteString 
                   -> Maybe B.ByteString 
                   -> [(B.ByteString, Maybe B.ByteString)] 
                   -> Attempt
mkAttemptWithMaybe mUser mSig kMvs = mkAttempt (orEmpty mUser) 
                                               (orEmpty mSig) 
                                               (map (second orEmpty) kMvs) where
    orEmpty = fromMaybe ""

mkHash :: B.ByteString -> B.ByteString
mkHash = B.pack . show . mkHashed

verifyHash :: Attempt -> B.ByteString -> Bool
verifyHash attempt sessionKey = mkHash (B.concat [sessionKey, bsArgs]) == claim attempt
    where allArgs = (userParam, user attempt) : arguments attempt
          bsArgs = B.intercalate "&" (map (\(k, v) -> B.concat [k, "=", v]) allArgs)

toKV :: [B.ByteString] -> (B.ByteString, B.ByteString)
toKV (key : value : _) = (key, value)
toKV _                 = ("", "")

verifyHashFromFile :: Attempt -> IO Bool
verifyHashFromFile attempt = do
    sessionKeysFile <- sessionKeysFileIO
    ls <- fmap B.lines (readOrCreateEmptyBS sessionKeysFile)
    let kvs = map (toKV . B.words) ls
        mSessionKey = lookup (user attempt) kvs
        verified = maybe False (verifyHash attempt) mSessionKey
    return verified

mkVerifiedRequest :: Maybe B.ByteString 
                   -> Maybe B.ByteString 
                   -> [(B.ByteString, Maybe B.ByteString)] 
                   -> IO Bool
mkVerifiedRequest mUser mSig kMvs = verifyHashFromFile (mkAttemptWithMaybe mUser mSig kMvs)

authenticate :: Maybe B.ByteString
             -> Maybe B.ByteString
             -> [(B.ByteString, Maybe B.ByteString)]
             -> Handler b service Bool
authenticate mUser mSig params = liftIO (mkVerifiedRequest mUser mSig params)

failIfUnverified :: Bool -> Handler b service () -> Handler b service ()
failIfUnverified verified handle = 
  if verified then handle 
              else writeBS "Authentication failed" >> modifyResponse (setResponseCode 406)