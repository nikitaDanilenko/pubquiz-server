module Sheet.SheetMaker
  ( createSheetWith
  , safeRemoveFile
  , Ending
  ) where

import Control.Exception (catch)
import Control.Exception.Base (IOException)
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T (Text, intercalate, concat, pack, unpack)
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.IO as I (writeFile)
import System.Directory (getCurrentDirectory, removeFile, setCurrentDirectory)
import System.Process (callProcess)

import Constants (addSeparator, quizzesFolderIO, sheetsFolderIO, sheetFileName, qrOnlyFileName, teamQueryParam)
import Data.List (sortOn)
import GHC.Natural (Natural)
import General.Types (Code, TeamNumber, unwrap)
import Sheet.Tex (mkQROnly, mkSheetWithArbitraryQuestions)
import Db.DbConversion (mkPathForQuizSheetWith, TeamQuery (TeamQuery))
import Data.Time.Calendar (Day)
import Db.Connection (DbQuizId)
import Data.Aeson (encode)

type Prefix = String

type Server = String

type Ending = String

--todo use proper types?
-- todo: reduce number of conversions?
-- todo: setting the folder vs. setting the file names could be improved?
createSheetWith :: String -> [Int] -> Server -> [(TeamNumber, Code)] -> Day -> DbQuizId -> IO ()
createSheetWith teamLabel rounds server numberedCodes day qid = do
  sheetsFolder <- sheetsFolderIO
  currentDir <- getCurrentDirectory
  let tl = T.pack teamLabel
      sht = mkSheetWithArbitraryQuestions tl rounds paths
      endings = sortOn fst numberedCodes
      paths = map (uncurry (mkPath server qid)) endings
      sheetFile = mkPathForQuizSheetWith (T.pack "") (T.pack ".") sheetFileName day qid
      qrs = mkQROnly tl paths
      codesFile = mkPathForQuizSheetWith (T.pack "") (T.pack ".") qrOnlyFileName day qid
  setCurrentDirectory (T.unpack sheetsFolder)
  writeAndCleanPDF (T.unpack sheetFile) sht
  writeAndCleanPDF (T.unpack codesFile) qrs
  setCurrentDirectory currentDir

mkPath :: Server -> DbQuizId -> TeamNumber -> Code -> T.Text
mkPath server qid tn code =
  T.intercalate (T.pack "/") [T.pack server, T.intercalate (T.pack "=") [E.decodeUtf8 teamQueryParam, E.decodeUtf8 (L.toStrict (encode teamQuery))]]
  where teamQuery = TeamQuery qid tn code

writeAndCleanPDF :: FilePath -> Text -> IO ()
writeAndCleanPDF mainPath content = do
  I.writeFile texFile content
  createPDF texFile `catch` noPDFLatex
  cleanLatex mainPath
  where
    texFile = mainPath ++ ".tex"
    noPDFLatex :: IOException -> IO ()
    noPDFLatex _ = putStrLn "pdflatex not found or it failed during document creation."

createPDF :: String -> IO ()
createPDF texFile = callProcess "pdflatex" ["-interaction=nonstopmode", texFile]

cleanLatex :: String -> IO ()
cleanLatex sheetFile = mapM_ (safeRemoveFile . (sheetFile ++)) [".log", ".aux", ".tex"]

safeRemoveFile :: String -> IO ()
safeRemoveFile path = removeFile path `catch` noFile
  where
    noFile :: IOException -> IO ()
    noFile _ = void (putStrLn "No file to remove")