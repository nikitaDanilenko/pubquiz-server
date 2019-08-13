module Sheet.SheetMaker ( createSheetWith, defaultEndings, Ending ) where

import Control.Exception           ( catch )
import Control.Exception.Base      ( IOException )
import Control.Monad               ( when, void )
import Data.Text                   ( Text )
import qualified Data.Text as T    ( pack, concat )
import qualified Data.Text.IO as I ( writeFile )
import System.Directory            ( setCurrentDirectory, getCurrentDirectory, removeFile )
import System.Process              ( callProcess )

import Constants                   ( quizzesFolderIO, addSeparator )
import Sheet.Tex                   ( mkSheetWithConstantQuestions )

type Prefix = String
type Server = String
type Ending = String

defaultEndings :: [Ending]
defaultEndings = [
        "sdig1o", "aikp25", "vzt35d", "fs7g5r", "9hf347",
        "f853q7", "pwi5q3", "weu429", "8fwr7h", "hu5p73",
        "yle8rf", "mdl20a", "84hrui", "c8vb3w", "la9inh",
        "k6bghz", "jwb54g", "4geu7y", "nbc6t4", "amv6zh"
    ]

createQRPath :: Prefix -> Ending -> Text
createQRPath prefix ending = T.concat (map T.pack [prefix, ending, ".html"])

createSheetWith :: String -> Int -> Prefix -> Server -> [Ending] -> IO ()
createSheetWith teamLabel rounds prefix server endings = do
    quizzesFolder <- quizzesFolderIO
    currentDir <- getCurrentDirectory
    
    let sheet = mkSheetWithConstantQuestions (T.pack teamLabel) rounds paths
        fullServerPath = addSeparator [server, prefix, ""]
        paths = map (createQRPath fullServerPath) endings
        buildPath = addSeparator [quizzesFolder, prefix]
        sheetFile = mkSheetFile prefix
        texFile = sheetFile ++ ".tex"
    
    setCurrentDirectory buildPath
    I.writeFile texFile sheet

    createPDF texFile `catch` noPDFLatex
    cleanLatex sheetFile
    setCurrentDirectory currentDir
  where noPDFLatex :: IOException -> IO ()
        noPDFLatex _ = putStrLn "pdflatex not found or it failed during document creation."

sheet :: String
sheet = "Sheet"

mkSheetFile :: Prefix -> String
mkSheetFile prefix = concat [prefix, "-", sheet]

createPDF :: String -> IO ()
createPDF texFile = callProcess "pdflatex" ["-interaction=nonstopmode", texFile]

cleanLatex :: String -> IO ()
cleanLatex sheetFile = mapM_  safeRemoveFile (map (sheetFile ++) [".log", ".aux", ".tex"])

safeRemoveFile :: String -> IO ()
safeRemoveFile path = removeFile path `catch` noFile where
    noFile :: IOException -> IO ()
    noFile _ = void (putStrLn "No file to remove")