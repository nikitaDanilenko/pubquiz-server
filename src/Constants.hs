{-# Language OverloadedStrings #-}

module Constants where

import Data.List                            ( intercalate )
import Data.Maybe                           ( fromMaybe )
import qualified Data.ByteString.Char8 as B
import System.FilePath                      ( pathSeparator )

sessionKeysFile :: String
sessionKeysFile = addSeparator [".", "db", "sessionKeys.txt"]

userFile :: String
userFile = addSeparator [".", "db", "users.txt"]

secretFile :: String
secretFile = addSeparator [".", "db", "secrets.txt"]

quizzesFolderIO :: IO String
quizzesFolderIO = do
        text <- readFile pageGenerator
        let settings = map splitOnSetter (lines text)
            folder = fromMaybe defaultFolder (lookup "quizzesFolder" settings)
        return folder



    where defaultFolder = addSeparator [".", "quizzes"]

splitOnSetter :: String -> (String, String)
splitOnSetter str = (key, drop 1 preValue) where
  (key, preValue) = span ((/=) '=') str

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
pageGenerator = addSeparator [".", "runGenerator.sh"]

quiz :: B.ByteString
quiz = "quiz"

roundsFile :: String
roundsFile = "rounds.txt"

publicExponent :: Integer
publicExponent = 103787

keySize :: Int
keySize = 2048