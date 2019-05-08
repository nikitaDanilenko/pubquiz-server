{-# Language OverloadedStrings #-}

module Constants where

import Data.List                            ( intercalate )
import qualified Data.ByteString.Char8 as B
import System.FilePath                      ( pathSeparator )

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

lock :: B.ByteString
lock = "lock"

prefix :: B.ByteString
prefix = "prefix"

rounds :: B.ByteString
rounds = "rounds"

pageGenerator :: String
pageGenerator = addSeparator [".", "runGenerator"]

quiz :: B.ByteString
quiz = "quiz"

roundsFile :: String
roundsFile = "rounds.txt"

publicExponent :: Integer
publicExponent = 103787

keySize :: Int
keySize = 2048