module Constants where

sessionKeysFile :: String
sessionKeysFile = "./db/sessionKeys.txt"

userFile :: String
userFile = "./db/users.txt"

secretFile :: String
secretFile = "./db/secrets.txt"

publicExponent :: Integer
publicExponent = 103787

keySize :: Int
keySize = 2048