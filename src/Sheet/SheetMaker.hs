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
import Pages.HtmlUtil              ( unEscape )
import Sheet.Tex                   ( mkSheet, mkSheetWithConstantQuestions )

type Prefix = String
type Server = String
type Ending = String

createSheetWith :: String -> Int -> Prefix -> Server -> [Ending] -> IO ()
createSheetWith teamLabel rounds prefix server endings = do
    quizzesFolder <- quizzesFolderIO
    currentDir <- getCurrentDirectory
    let fullPath = addSeparator [quizzesFolder, prefix]
        fullServerPath = addSeparator [server, prefix, ""]
        sht = mkSheet (safeTeX teamLabel) rounds
        sheetFile = mkSheetFile prefix
        texFile = sheetFile ++ ".tex"

    setCurrentDirectory fullPath

    writeFile texFile sht 
    
    b <- (mapM_ (createQR fullServerPath) endings >> return True) `catch` noQREncode
    when b (createPDF texFile endings `catch` noPDFLatex)

    cleanImages endings
    cleanLatex sheetFile
    setCurrentDirectory currentDir
  where noQREncode :: IOException -> IO Bool
        noQREncode _ = putStrLn "qrencode not found. No sheet created." >> return False

        noPDFLatex :: IOException -> IO ()
        noPDFLatex _ = putStrLn "pdflatex not found or it failed during document creation."

defaultEndings :: [Ending]
defaultEndings = [
        "sdig1o", "aikp25", "vzt35d", "fs7g5r", "9hf347",
        "f853q7", "pwi5q3", "weu429", "8fwr7h", "hu5p73",
        "yle8rf", "mdl20a", "84hrui", "c8vb3w", "la9inh",
        "k6bghz", "jwb54g", "4geu7y", "nbc6t4", "amv6zh"
    ]

createQRPath :: Prefix -> Ending -> Text
createQRPath prefix ending = T.concat (map T.pack [prefix, ending, ".html"])

createSheetWith2 :: String -> Int -> Prefix -> Server -> [Ending] -> IO ()
createSheetWith2 teamLabel rounds prefix server endings = do
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

    createPDF texFile endings `catch` noPDFLatex
    cleanLatex sheetFile
    setCurrentDirectory currentDir
  where noPDFLatex :: IOException -> IO ()
        noPDFLatex _ = putStrLn "pdflatex not found or it failed during document creation."

createQR :: Prefix -> Ending -> IO ()
createQR prefix ending = 
    callProcess "qrencode" [concat [prefix, ending, ".html"], concat ["-o", ending, ".png"]]

variableFile :: String
variableFile = "listVariable.tex"

createLatexVariable :: [Ending] -> IO ()
createLatexVariable es = writeFile variableFile variable where
    indexed = zip [1 :: Int ..] es

    variable :: String
    variable = unlines (map (\(i, str) -> concat ["\\mkRounds{", show i, "}{", str, "}" ]) indexed)

sheet :: String
sheet = "Sheet"

mkSheetFile :: Prefix -> String
mkSheetFile prefix = concat [prefix, "-", sheet]

createPDF :: String -> [Ending] -> IO ()
createPDF texFile es = do
    createLatexVariable es
    callProcess "pdflatex" ["-interaction=nonstopmode", texFile]

cleanLatex :: String -> IO ()
cleanLatex sheetFile = 
    mapM_  safeRemoveFile (variableFile : map (sheetFile ++) [".log", ".aux", ".tex"])

safeRemoveFile :: String -> IO ()
safeRemoveFile path = removeFile path `catch` noFile where
    noFile :: IOException -> IO ()
    noFile _ = void (putStrLn "No file to remove")

cleanImages :: [Ending] -> IO ()
cleanImages = mapM_ (safeRemoveFile . (++ ".png"))

safeTeX :: String -> String
safeTeX = concatMap safeTeXChar . unEscape

safeTeXChar :: Char -> String
safeTeXChar c = case c of
    'ä'      -> "\\\"a"
    'ö'      -> "\\\"o"
    'ü'      -> "\\\"u"
    'Ä'      -> "\\\"A"
    'Ö'      -> "\\\"O"
    'Ü'      -> "\\\"U"
    'ß'      -> "\\ss{}"
    '\\'      -> "\\textbackslash"
    '~'      -> "\\textasciitilde"
    '&'      -> "\\&"
    '%'      -> "\\%"
    '$'      -> "\\$"
    '#'      -> "\\#"
    '_'      -> "\\_"
    '{'      -> "\\{"
    '}'      -> "\\}"
    '^'      -> "\\textasciicircum"
    anyOther -> [anyOther]