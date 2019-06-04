{-# Language OverloadedStrings #-}

module Constants where

import Control.Exception                    ( catch )
import Control.Exception.Base               ( IOException )
import Data.List                            ( intercalate )
import Data.Maybe                           ( fromMaybe )
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T             ( Text )
import System.FilePath                      ( pathSeparator )

sessionKeysFile :: String
sessionKeysFile = addSeparator [".", "db", "sessionKeys.txt"]

userFile :: String
userFile = addSeparator [".", "db", "users.txt"]

secretFile :: String
secretFile = addSeparator [".", "db", "secrets.txt"]

configFile :: String
configFile = "./config.txt"

quizzesFolderIO :: IO String
quizzesFolderIO = do
        text <- readFile configFile `catch` noConfigFile
        let settings = map splitOnSetter (lines text)
            folder = fromMaybe defaultFolder (lookup "quizzesFolder" settings)
        return folder

    where defaultFolder = addSeparator [".", "quizzes"]
          
          noConfigFile :: IOException -> IO String
          noConfigFile _ = do 
            putStrLn "No config file found. This should not happen. Created an empty one."
            writeFile configFile ""
            return ""


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

labels :: B.ByteString
labels = "labels"

colors :: B.ByteString
colors = "colors"

actionParam :: B.ByteString
actionParam = "action"

createQuiz :: B.ByteString
createQuiz = "createQuiz"

roundsNumberParam :: B.ByteString
roundsNumberParam = "roundsNumber"

quizParam :: B.ByteString
quizParam = "quiz"

roundsFile :: String
roundsFile = "rounds.txt"

labelsFile :: String
labelsFile = "labels.txt"

colorsFile :: String
colorsFile = "colors.txt"

roundParam :: B.ByteString
roundParam = "roundLabel"

groupParam :: B.ByteString
groupParam = "groupLabel"

ownPointsParam :: B.ByteString
ownPointsParam = "ownPointsLabel"

maxReachedParam :: B.ByteString
maxReachedParam = "maxReachedLabel"

maxReachableParam :: B.ByteString
maxReachableParam = "maxReachableLabel"

backToChartViewParam :: B.ByteString
backToChartViewParam = "backToChartViewLabel"

mainParam :: B.ByteString
mainParam = "mainLabel"

ownPageParam :: B.ByteString
ownPageParam = "ownPageLabel"

userParam :: B.ByteString
userParam = "user"

newUserParam :: B.ByteString
newUserParam = "newUser"

passwordParam :: B.ByteString
passwordParam = "pass"

signatureParam :: B.ByteString
signatureParam = "signature"

server :: String
server = "https://www.danilenko.io"

apiPath :: T.Text
apiPath = "api"

secretPath :: T.Text
secretPath = "secrets"

quizPath :: T.Text
quizPath = "quiz"

userPath :: T.Text
userPath = "users"

publicExponent :: Integer
publicExponent = 103787

keySize :: Int
keySize = 256

oneWayHashSize :: Int
oneWayHashSize = 2048

saltSize :: Int
saltSize = 24