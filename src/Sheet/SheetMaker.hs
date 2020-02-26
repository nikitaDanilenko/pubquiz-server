module Sheet.SheetMaker ( createSheetWith, Ending ) where

import Control.Exception           ( catch )
import Control.Exception.Base      ( IOException )
import Control.Monad               ( void )
import Data.Text                   ( Text )
import qualified Data.Text as T    ( pack, concat )
import qualified Data.Text.IO as I ( writeFile )
import System.Directory            ( setCurrentDirectory, getCurrentDirectory, removeFile )
import System.Process              ( callProcess )

import Constants                   ( quizzesFolderIO, addSeparator, sheetsFolderIO )
import Sheet.Tex                   ( mkSheetWithArbitraryQuestions, mkQROnly )
import General.Types (TeamNumber, Code, unwrap)
import Data.List (sortOn)
import GHC.Natural (Natural)

type Prefix = String
type Server = String
type Ending = String

createQRPath :: Prefix -> Ending -> Text
createQRPath prefix ending = T.concat (map T.pack [prefix, ending, ".html"])

createSheetWith :: String -> [Int] -> Prefix -> Server -> [(TeamNumber, Code)] -> IO ()
createSheetWith teamLabel rounds prefix server numberedCodes = do
  sheetsFolder <- sheetsFolderIO
  currentDir <- getCurrentDirectory
  let tl = T.pack teamLabel
      sht = mkSheetWithArbitraryQuestions tl rounds paths
      fullServerPath = addSeparator [server, prefix, ""]
      endings = map (unwrap . snd) (sortOn fst numberedCodes)
      paths = map (createQRPath fullServerPath) endings
      sheetFile = mkSheetFile prefix
      qrs = mkQROnly tl paths
      codesFile = mkCodesFile prefix
  setCurrentDirectory sheetsFolder
  writeAndCleanPDF sheetFile sht
  writeAndCleanPDF codesFile qrs
  setCurrentDirectory currentDir

writeAndCleanPDF :: FilePath -> Text -> IO ()
writeAndCleanPDF mainPath content = do
    I.writeFile texFile content
    createPDF texFile `catch` noPDFLatex
    cleanLatex mainPath
  where texFile = mainPath ++ ".tex"
 
        noPDFLatex :: IOException -> IO ()
        noPDFLatex _ = putStrLn "pdflatex not found or it failed during document creation."

sheet :: String
sheet = "Sheet"

codes :: String
codes = "QR"

mkSheetFile :: Prefix -> String
mkSheetFile prefix = mkFile prefix sheet

mkCodesFile :: Prefix -> String
mkCodesFile prefix = mkFile prefix codes

mkFile :: Prefix -> String -> String
mkFile prefix suffix = concat [prefix, "-", suffix]

createPDF :: String -> IO ()
createPDF texFile = callProcess "pdflatex" ["-interaction=nonstopmode", texFile]

cleanLatex :: String -> IO ()
cleanLatex sheetFile = mapM_ (safeRemoveFile . (sheetFile ++)) [".log", ".aux", ".tex"]

safeRemoveFile :: String -> IO ()
safeRemoveFile path = removeFile path `catch` noFile where
    noFile :: IOException -> IO ()
    noFile _ = void (putStrLn "No file to remove")