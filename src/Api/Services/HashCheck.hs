{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Services.HashCheck
  ( authenticate
  , failIfUnverified
  ) where

import           Control.Arrow          (second)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as B
import qualified Data.ByteString.Lazy   as L
import           Data.Maybe             (fromMaybe, maybe)
import           Data.Text.Encoding     (decodeUtf8)
import           Snap.Core              (modifyResponse, setResponseCode,
                                         writeBS)
import           Snap.Snaplet           (Handler)

import           Data.Aeson             (encode, object, (.=))
import qualified Data.Text.Encoding     as E
import           Db.DbConversion        (Credentials, signature, userHash)
import qualified Db.DbConversion        as D
import           Db.Storage             (findUser)
import           General.Types          (Unwrappable, UserHash,
                                         UserName (UserName), unwrap, wrap)
import           Utils                  (mkHashed)

data Attempt =
  Attempt
    { userName  :: UserName
    , arguments :: [(B.ByteString, B.ByteString)]
    , claim     :: UserHash
    }

mkHash :: B.ByteString -> B.ByteString
mkHash = B.pack . show . mkHashed

verifyHash :: Attempt -> UserHash -> Bool
verifyHash attempt sessionKey = mkHash (B.concat [unwrap sessionKey, L.toStrict args]) == unwrap (claim attempt)
  where
    args = encode (object (map (\(k, v) -> decodeUtf8 k .= decodeUtf8 v) (arguments attempt)))

mkAttemptWithMaybe :: Maybe UserName -> Maybe UserHash -> [(B.ByteString, Maybe B.ByteString)] -> Attempt
mkAttemptWithMaybe mUser mSig kMvs = Attempt (orEmpty mUser) (map (second (fromMaybe "")) kMvs) (orEmpty mSig)

orEmpty :: Unwrappable t String => Maybe t -> t
orEmpty = fromMaybe (wrap "")

mkVerifiedRequest2 :: Maybe UserName -> Maybe UserHash -> [(B.ByteString, Maybe B.ByteString)] -> IO Bool
mkVerifiedRequest2 mUserName mSig kMvs = do
  mUser <- findUser (userName attempt)
  return (maybe False (verifyHash attempt . userHash) mUser)
  where
    attempt = mkAttemptWithMaybe mUserName mSig kMvs

-- todo: simplify to avoid conversions and qualified imports
authenticate :: Maybe Credentials -> [(B.ByteString, Maybe B.ByteString)] -> Handler b service Bool
authenticate credentials params =
  liftIO (mkVerifiedRequest2 (fmap D.user credentials) (fmap signature credentials) params)

failIfUnverified :: Bool -> Handler b service () -> Handler b service ()
failIfUnverified verified handle =
  if verified
    then handle
    else writeBS "Authentication failed" >> modifyResponse (setResponseCode 406)
