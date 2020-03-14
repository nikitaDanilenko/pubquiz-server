{-# LANGUAGE OverloadedStrings #-}

module Constants where

import           Control.Exception      (catch)
import           Control.Exception.Base (IOException)
import qualified Data.ByteString.Char8  as B
import           Data.List              (intercalate)
import           Data.Map               (Map)
import qualified Data.Map               as M (fromList)
import           Data.Map.Lazy          ((!?))
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T (Text, pack)
import           System.Directory       (createDirectoryIfMissing,
                                         doesDirectoryExist)
import           System.FilePath        (pathSeparator)

configFile :: String
configFile = "./config.txt"

configMap :: IO (Map String String)
configMap = do
  text <- readFile configFile `catch` noConfigFile
  let settings = map splitOnSetter (lines text)
      kvsMap = M.fromList settings
  return kvsMap

readFromConfigFile :: String -> String -> IO String
readFromConfigFile param dft = do
  settings <- configMap
  let path = fromMaybe dft (settings !? param)
  return path

sheetsFolderIO :: IO T.Text
sheetsFolderIO = fmap T.pack (readFromConfigFile "sheetsFolder" (addSeparator [".", "sheets"]))

quizzesFolderIO :: IO String
quizzesFolderIO = readFromConfigFile "quizzesFolder" (addSeparator [".", "quizzes"])

serverPathIO :: IO String
serverPathIO = readFromConfigFile "serverPath" "localhost:9000"

noConfigFile :: IOException -> IO String
noConfigFile _ = do
  putStrLn "No config file found. This should not happen. Created an empty one."
  writeFile configFile ""
  return ""

splitOnSetter :: String -> (String, String)
splitOnSetter str = (key, drop 1 preValue)
  where
    (key, preValue) = span ('=' /=) str

addSeparator :: [String] -> String
addSeparator = intercalate [pathSeparator]

quizRatingsParam :: B.ByteString
quizRatingsParam = "quizRatings"

actionParam :: B.ByteString
actionParam = "action"

quizIdParam :: B.ByteString
quizIdParam = "quizId"

quizIdentifierParam :: B.ByteString
quizIdentifierParam = "quizIdentifier"

quizSettingsParam :: B.ByteString
quizSettingsParam = "quizSettings"

credentialsParam :: B.ByteString
credentialsParam = "credentials"

userParam :: B.ByteString
userParam = "user"

passwordParam :: B.ByteString
passwordParam = "pass"

teamQueryParam :: B.ByteString
teamQueryParam = "teamQuery"

teamNumberParam :: B.ByteString
teamNumberParam = "teamNumber"

teamCodeParam :: B.ByteString
teamCodeParam = "teamCode"

userCreationParam :: B.ByteString
userCreationParam = "userCreation"

serverQuizzesFolderIO :: IO String
serverQuizzesFolderIO = readFromConfigFile "serverQuizzesFolder" "quizzes"

serverSheetsFolderIO :: IO T.Text
serverSheetsFolderIO = fmap T.pack (readFromConfigFile "serverSheetsFolder" "sheets")

apiPath :: T.Text
apiPath = "api"

secretPath :: T.Text
secretPath = "secrets"

quizPath :: T.Text
quizPath = "quiz"

userPath :: T.Text
userPath = "users"

databaseHost :: String
databaseHost = "databaseHost"

databaseName :: String
databaseName = "databaseName"

databaseUser :: String
databaseUser = "databaseUser"

databasePassword :: String
databasePassword = "databasePassword"

databasePort :: String
databasePort = "databasePort"

allApi :: B.ByteString
allApi = "all"

getQuizRatingsApi :: B.ByteString
getQuizRatingsApi = "getQuizRatings"

getLabelsApi :: B.ByteString
getLabelsApi = "getLabels"

updateQuizApi :: B.ByteString
updateQuizApi = "updateQuiz"

updateQuizRatingsApi :: B.ByteString
updateQuizRatingsApi = "updateQuizRatings"

lockApi :: B.ByteString
lockApi = "lock"

newApi :: B.ByteString
newApi = "new"

teamTableApi :: B.ByteString
teamTableApi = "teamTable"

getQuizInfoApi :: B.ByteString
getQuizInfoApi = "getQuizInfo"

createUserApi :: B.ByteString
createUserApi = "createUser"

secretApi :: B.ByteString
secretApi = "/"

sheetFileName :: T.Text
sheetFileName = "QuizSheet"

qrOnlyFileName :: T.Text
qrOnlyFileName = "QuizQROnly"

publicExponent :: Integer
publicExponent = 103787

keySize :: Int
keySize = 256

oneWayHashSize :: Int
oneWayHashSize = 2048

saltSize :: Int
saltSize = 24
