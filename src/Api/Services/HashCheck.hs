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
import           Data.Maybe             (fromJust, fromMaybe, maybe)
import           Data.Text.Encoding     (decodeUtf8)
import           Snap.Core              (modifyResponse, setResponseCode,
                                         writeBS)
import           Snap.Snaplet           (Handler)

import           Data.Aeson             (encode, object, (.=))
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as E
import           Db.DbConversion        (Credentials, signature, userHash)
import qualified Db.DbConversion        as D
import           Db.Storage             (findSessionKey, findUser)
import           General.Types          (Wrapped, UserHash,
                                         UserName (UserName), unwrap, wrap)
import           Network.HTTP.Types.URI (Query, QueryItem, renderQuery)
import           Utils                  (mkHashed)

data Attempt =
  Attempt
    { userName :: UserName
    , query    :: Query
    , claim    :: UserHash
    }

mkHash :: B.ByteString -> B.ByteString
mkHash = B.pack . show . mkHashed

verifyHash :: Attempt -> UserHash -> Bool
verifyHash attempt sessionKey =
  mkHash (B.concat [unwrap sessionKey, renderQuery False (query attempt)]) == unwrap (claim attempt)

mkAttemptWithMaybe :: Maybe UserName -> Maybe UserHash -> [QueryItem] -> Attempt
mkAttemptWithMaybe mUser mSig kMvs = Attempt (orEmpty mUser) kMvs (orEmpty mSig)

orEmpty :: Wrapped t String => Maybe t -> t
orEmpty = fromMaybe (wrap "")

mkVerifiedRequest :: Attempt -> IO Bool
mkVerifiedRequest attempt = fmap (maybe False (verifyHash attempt)) (findSessionKey (userName attempt))

authenticate :: Maybe Credentials -> Query -> Handler b service Bool
authenticate credentials params =
  liftIO (mkVerifiedRequest (mkAttemptWithMaybe (fmap D.user credentials) (fmap signature credentials) params))

failIfUnverified :: Bool -> Handler b service () -> Handler b service ()
failIfUnverified verified handle =
  if verified
    then handle
    else writeBS "Authentication failed" >> modifyResponse (setResponseCode 406)
