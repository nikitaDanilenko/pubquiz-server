module Constants where

import Data.List       ( intercalate )
import System.FilePath ( pathSeparator )

sessionKeysFile :: String
sessionKeysFile = addSeparator [".", "db", "sessionKeys.txt"]

userFile :: String
userFile = addSeparator [".", "db", "users.txt"]

secretFile :: String
secretFile = addSeparator [".", "db", "secrets.txt"]

quizzesFolder :: String
quizzesFolder = addSeparator [".", "quizzes"]

addSeparator :: [String] -> String
addSeparator = intercalate [pathSeparator]

locked :: String
locked = ".locked"

publicExponent :: Integer
publicExponent = 103787

keySize :: Int
keySize = 2048