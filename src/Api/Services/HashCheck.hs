{-# LANGUAGE OverloadedStrings #-}

module Api.Services.HashCheck
  ( authenticate
  ) where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (ExceptT (ExceptT))
import qualified Data.ByteString.Base64     as B64 (encode)
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy       as L
import           Data.Maybe                 (maybe)
import           Db.DbConversion            (Credentials, signature)
import qualified Db.DbConversion            as D
import           Db.Storage                 (findSessionKey)
import           General.Types              (UserHash, UserName (UserName),
                                             unwrap)
import           Snap.Snaplet               (Handler)
import           Utils                      (mkHashed)

data Attempt =
  Attempt
    { userName :: UserName
    , body     :: B.ByteString
    , claim    :: UserHash
    }

mkHash :: B.ByteString -> B.ByteString
mkHash = B.pack . show . mkHashed

verifyHash :: Attempt -> UserHash -> Either L.ByteString ()
verifyHash attempt sessionKey =
  if condition
    then Right ()
    else Left "Authentication failed"
  where
    condition = mkHash (B.concat [unwrap sessionKey, B64.encode (body attempt)]) == unwrap (claim attempt)

mkVerifiedRequest :: Attempt -> IO (Either L.ByteString ())
mkVerifiedRequest attempt =
  fmap (maybe (Left "User not found") (verifyHash attempt)) (findSessionKey (userName attempt))

authenticate :: Credentials -> B.ByteString -> ExceptT L.ByteString (Handler b service) ()
authenticate credentials bd =
  ExceptT (liftIO (mkVerifiedRequest (Attempt (D.user credentials) bd (signature credentials))))
