{-# Language OverloadedStrings, FlexibleInstances, PackageImports #-}

module Api.Services.EstablishSecret where

import Control.Monad.IO.Class
import Crypto.PubKey.RSA                     ( generate, PublicKey )
import "cryptonite" Crypto.Hash              ( hash )
import Data.Maybe                            ( fromMaybe )
import Snap.Core hiding                      ( pass )
import Snap.Snaplet
import qualified Data.ByteString.Char8 as B 

import Api.Services.SavedUser                ( SavedUser (..), UserName, Password, mkHash )
import Constants                             ( sessionKeysFile, userFile, secretFile, 
                                               publicExponent, keySize )

data SecretService = SecretService

secretRoutes :: [(B.ByteString, Handler b SecretService ())]
secretRoutes = [("/", method POST createSecret)]

createSecret :: Handler b SecretService ()
createSecret = do
    mUser <- getPostParam "user"
    mPass <- getPostParam "pass"
    valid <- liftIO $ case (mUser, mPass) of
        (Just user, Just pass) -> verifyUser user pass
        _                      -> pure False
    if valid then do
        public <- liftIO (createKeyPair (fromMaybe "error" mUser))
        writeBS (B.concat [sessionKeyLabel, "=", B.pack (show public)])
        modifyResponse (setResponseCode 201)
    else do
        writeBS authentificationError
        modifyResponse (setResponseCode 401)

{- For future reference. 
createSecret :: Handler b SecretService ()
createSecret = do
    req <- getRequest
    mUser <- getPostParam "user"
    mPass <- getPostParam "pass"
    let mAuth = getHeader "Authorization" req
    liftIO (writeFile "./processingSecret.txt" (show [mUser, mPass, mAuth]))
    modifyResponse $ setResponseCode 201
-}

authentificationError :: B.ByteString
authentificationError = B.pack "Failed to authenticate: user name or password is wrong."

sessionKeyLabel :: B.ByteString
sessionKeyLabel = B.pack "sessionKey"

-- | Verifies that the user is known and that the supplied password is correct.
--   Both values are fetched from a local data storage.
verifyUser :: UserName -> Password -> IO Bool
verifyUser username password = 
    fmap (verifyUserWithUsers username password . map (read :: String -> SavedUser) . lines) 
         (readFile userFile)

verifyUserWithUsers :: UserName -> Password -> [SavedUser] -> Bool
verifyUserWithUsers username password users = fromMaybe False maybeVerified where
    candidate = lookup username (map (\u -> (userName u, u)) users)
    maybeVerified = fmap (verifyPassword username password) candidate

verifyPassword :: UserName -> Password -> SavedUser -> Bool
verifyPassword username password savedUser = hash == hashValue savedUser where
    hash = mkHash username password (salt savedUser)

createKeyPair :: UserName -> IO PublicKey
createKeyPair user = do
    lines <- fmap B.lines (B.readFile sessionKeysFile)
    let clearedLines = removeInit user lines
    (public, private) <- generate keySize publicExponent
    let newLines = B.unwords [user, B.pack (show private)] : clearedLines
    B.writeFile sessionKeysFile (B.unlines newLines)
    return public

removeInit :: B.ByteString -> [B.ByteString] -> [B.ByteString]
removeInit init = filter (not . B.isPrefixOf init) 



secretServiceInit :: SnapletInit b SecretService
secretServiceInit = makeSnaplet "secret" "Secret Service" Nothing $ do
    addRoutes secretRoutes
    return SecretService