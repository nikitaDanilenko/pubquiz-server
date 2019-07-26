{-# Language OverloadedStrings, FlexibleInstances, PackageImports #-}

module Api.Services.EstablishSecret ( SecretService, secretServiceInit ) where

import Control.Monad.IO.Class
import Crypto.PubKey.RSA                     ( generate, PublicKey )
import qualified Data.ByteString.Char8 as B 
import Data.Maybe                            ( fromMaybe )
import Snap.Core hiding                      ( pass )
import Snap.Snaplet

import Api.Services.SavedUser                ( SavedUser (..), UserName, Password, mkHash )
import Constants                             ( sessionKeysFileIO, userFileIO, publicExponent, 
                                               keySize, oneWayHashSize, userParam, passwordParam )
import Utils                                 ( readOrCreateEmptyBS, (+>), randomStringIO )

data SecretService = SecretService

secretRoutes :: [(B.ByteString, Handler b SecretService ())]
secretRoutes = ["/" +> method POST createSecret]

createSecret :: Handler b SecretService ()
createSecret = do
    mUser <- getPostParam userParam
    mPass <- getPostParam passwordParam
    valid <- liftIO $ case (mUser, mPass) of
        (Just user, Just pass) -> verifyUser user pass
        _                      -> pure False
    if valid then do
        oneWayHash <- liftIO (createHash (fromMaybe "error" mUser))
        writeBS (B.pack oneWayHash)
        modifyResponse (setResponseCode 201)
    else do
        writeBS authentificationError
        modifyResponse (setResponseCode 401)

authentificationError :: B.ByteString
authentificationError = B.pack "Failed to authenticate: user name or password is wrong."

-- | Verifies that the user is known and that the supplied password is correct.
--   Both values are fetched from a local data storage.
verifyUser :: UserName -> Password -> IO Bool
verifyUser username password = do
    userFile <- userFileIO
    fmap (verifyUserWithUsers username password . map read . lines) (readFile userFile)

verifyUserWithUsers :: UserName -> Password -> [SavedUser] -> Bool
verifyUserWithUsers username password users = fromMaybe False maybeVerified where
    candidate = lookup username (map (\u -> (userName u, u)) users)
    maybeVerified = fmap (verifyPassword username password) candidate

verifyPassword :: UserName -> Password -> SavedUser -> Bool
verifyPassword username password savedUser = hash == userHash savedUser where
    hash = mkHash username password (userSalt savedUser)

createHash :: UserName -> IO String
createHash user = do
    sessionKeysFile <- sessionKeysFileIO
    ls <- fmap B.lines (readOrCreateEmptyBS sessionKeysFile)
    oneWayHash <- fmap (take oneWayHashSize) randomStringIO
    let clearedLines = removeInit user ls
        newLines = B.unwords [user, B.pack oneWayHash] : clearedLines
    B.writeFile sessionKeysFile (B.unlines newLines)
    return oneWayHash

createKeyPair :: UserName -> IO PublicKey
createKeyPair user = do
    sessionKeysFile <- sessionKeysFileIO
    ls <- fmap B.lines (readOrCreateEmptyBS sessionKeysFile)
    let clearedLines = removeInit user ls
    (public, private) <- generate keySize publicExponent
    let newLines = B.unwords [user, B.pack (show private)] : clearedLines
    B.writeFile sessionKeysFile (B.unlines newLines)
    return public

removeInit :: B.ByteString -> [B.ByteString] -> [B.ByteString]
removeInit prefix = filter (not . B.isPrefixOf prefix) 

secretServiceInit :: SnapletInit b SecretService
secretServiceInit = makeSnaplet "secret" "Secret Service" Nothing $ do
    addRoutes secretRoutes
    return SecretService