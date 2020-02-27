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

dbFolderIO :: IO String
dbFolderIO = readFromConfigFile "database" (addSeparator [".", "db"])

sessionKeysFileIO :: IO String
sessionKeysFileIO = fmap (\dbFolder -> addSeparator [dbFolder, "sessionKeys.txt"]) dbFolderIO

userFileIO :: IO String
userFileIO = fmap (\dbFolder -> addSeparator [dbFolder, "users.txt"]) dbFolderIO

doesDBExist :: IO Bool
doesDBExist = dbFolderIO >>= doesDirectoryExist

createDBFolder :: IO ()
createDBFolder = dbFolderIO >>= createDirectoryIfMissing True

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

addSeparatorBS :: [B.ByteString] -> B.ByteString
addSeparatorBS = B.intercalate (B.pack [pathSeparator])

locked :: String
locked = ".locked"

prefix :: B.ByteString
prefix = "prefix"

quizRatingsParam :: B.ByteString
quizRatingsParam = "quizRatings"

labelsParam :: B.ByteString
labelsParam = "labels"

actionParam :: B.ByteString
actionParam = "action"

roundsNumberParam :: B.ByteString
roundsNumberParam = "roundsNumber"

quizIdParam :: B.ByteString
quizIdParam = "quizId"

quizIdentifierParam :: B.ByteString
quizIdentifierParam = "quizIdentifier"

quizSettingsParam :: B.ByteString
quizSettingsParam = "quizSettings"

credentialsParam :: B.ByteString
credentialsParam = "credentials"

roundsFile :: String
roundsFile = "rounds.txt"

labelsFile :: String
labelsFile = "labels.txt"

roundParam :: B.ByteString
roundParam = "roundLabel"

teamParam :: B.ByteString
teamParam = "teamLabel"

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

viewQuizzesParam :: B.ByteString
viewQuizzesParam = "viewQuizzesLabel"

cumulativeParam :: B.ByteString
cumulativeParam = "cumulativeLabel"

individualParam :: B.ByteString
individualParam = "individualLabel"

progressionParam :: B.ByteString
progressionParam = "progressionLabel"

placementParam :: B.ByteString
placementParam = "placementLabel"

placeParam :: B.ByteString
placeParam = "placeLabel"

pointsParam :: B.ByteString
pointsParam = "pointsLabel"

roundWinnerParam :: B.ByteString
roundWinnerParam = "roundWinnerLabel"

userParam :: B.ByteString
userParam = "user"

newUserParam :: B.ByteString
newUserParam = "newUser"

passwordParam :: B.ByteString
passwordParam = "pass"

signatureParam :: B.ByteString
signatureParam = "signature"

numberOfTeamsParam :: B.ByteString
numberOfTeamsParam = "numberOfTeams"

server :: String
server = "https://www.danilenko.io"

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

updateQuizSettingsApi :: B.ByteString
updateQuizSettingsApi = "updateQuizSettings"

updateApi :: B.ByteString
updateApi = "update"

lockApi :: B.ByteString
lockApi = "lock"

newApi :: B.ByteString
newApi = "new"

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
