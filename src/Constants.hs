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
import           System.Directory       (createDirectoryIfMissing)
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

quizIdParam :: B.ByteString
quizIdParam = "quizId"

teamQueryParam :: B.ByteString
teamQueryParam = "teamQuery"

teamNumberParam :: B.ByteString
teamNumberParam = "teamNumber"

teamCodeParam :: B.ByteString
teamCodeParam = "teamCode"

serverQuizzesFolderIO :: IO String
serverQuizzesFolderIO = readFromConfigFile "serverQuizzesFolder" "quizzes"

serverSheetsFolderIO :: IO T.Text
serverSheetsFolderIO = do
  folder <- readFromConfigFile "serverSheetsFolder" "sheets"
  createDirectoryIfMissing True folder
  pure (T.pack folder)

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

allActiveApi :: B.ByteString
allActiveApi = "allActive"

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

getQuizSettingsApi :: B.ByteString
getQuizSettingsApi = "getQuizSettings"

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
