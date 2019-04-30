{-# Language OverloadedStrings, TemplateHaskell, FlexibleInstances, PackageImports #-}

module Api.Services.EstablishSecret where

import Control.Lens
import Control.Monad.IO.Class
import Crypto.PubKey.RSA                     ( generate, PublicKey )
import "cryptonite" Crypto.Hash              ( hash )
import Data.List                             ( isPrefixOf )
import Data.Maybe                            ( fromMaybe )
import Snap.Core
import Snap.Snaplet
import qualified Data.ByteString.Char8 as B

import Api.Services.SavedUser                ( SavedUser (..), Hashed )

data SecretService = SecretService

-- makeLenses ''SecretService

secretRoutes :: [(B.ByteString, Handler b SecretService ())]
secretRoutes = [("/", method POST createSecret)]

createSecret :: Handler b SecretService ()
createSecret = do
    req <- getRequest
    mUser <- getPostParam "user"
    mPass <- getPostParam "pass"
    let mAuth = getHeader "Authorization" req
    liftIO (writeFile "./processingSecret.txt" (show [mUser, mPass, mAuth]))
    modifyResponse $ setResponseCode 201

type UserName = String
type Password = String

verifyUser :: UserName -> Password -> IO Bool
verifyUser username password = 
    fmap (verifyUserWithUsers username password . map (read :: String -> SavedUser) . lines) (readFile userFile)

verifyUserWithUsers :: UserName -> Password -> [SavedUser] -> Bool
verifyUserWithUsers username password users = fromMaybe False maybeVerified where
    candidate = lookup username (map (\u -> (userName u, u)) users)
    maybeVerified = fmap (verifyPassword username password) candidate

verifyPassword :: UserName -> Password -> SavedUser -> Bool
verifyPassword username password savedUser = show hashed == hashValue savedUser where
    hashed :: Hashed
    hashed = hash (B.pack (unwords [username, password, salt savedUser]))

createKeyPair :: UserName -> IO PublicKey
createKeyPair user = do
    lines <- fmap lines (readFile sessionKeysFile)
    let clearedLines = removeInit user lines
    (public, private) <- generate keySize publicExponent
    let newLines = unwords [user, show private] : clearedLines
    writeFile sessionKeysFile (unlines newLines)
    return public

removeInit :: Eq a => [a] -> [[a]] -> [[a]]
removeInit init = filter (not . isPrefixOf init) 

sessionKeysFile :: String
sessionKeysFile = "./db/sessionKeys.txt"

userFile :: String
userFile = "./db/users.txt"

publicExponent :: Integer
publicExponent = 103787

keySize :: Int
keySize = 2048

secretServiceInit :: SnapletInit b SecretService
secretServiceInit = makeSnaplet "secret" "Secret Service" Nothing $ do
    addRoutes secretRoutes
    return SecretService