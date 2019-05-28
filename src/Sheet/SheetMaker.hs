module Sheet.SheetMaker ( createSheetWith, defaultEndings ) where

import Control.Exception       ( catch )
import Control.Exception.Base  ( IOException )
import System.Directory        ( setCurrentDirectory, getCurrentDirectory )
import System.Process          ( callProcess )
     
import Constants               ( quizzesFolderIO, addSeparator )
import Sheet.Tex               ( mkSheet )

type Prefix = String
type Server = String
type Ending = String

createSheetWith :: String -> Int -> Prefix -> Server -> [Ending] -> IO ()
createSheetWith groupLabel rounds prefix server endings = do
    quizzesFolder <- quizzesFolderIO
    currentDir <- getCurrentDirectory
    let fullPath = addSeparator [quizzesFolder, prefix]
        fullServerPath = addSeparator [server, simplify quizzesFolder, prefix, ""]
        sht = mkSheet groupLabel rounds
        sheetFile = mkSheetFile prefix
        texFile = concat [sheetFile, ".tex"]

    setCurrentDirectory fullPath

    writeFile texFile sht 
    let trySheet = do 
            b <- (mapM_ (createQR fullServerPath) endings >> return True) `catch` noQREncode
            if b then createPDF texFile endings `catch` noPDFLatex else return ()

    trySheet
    cleanImages endings
    cleanLatex sheetFile
    setCurrentDirectory currentDir
  where noQREncode :: IOException -> IO Bool
        noQREncode _ = putStrLn "qrencode not found. No sheet created." >> return False

        noPDFLatex :: IOException -> IO ()
        noPDFLatex _ = putStrLn "pdflatex not found or it failed during document creation."

simplify :: FilePath -> FilePath
simplify ('.' : '/' : actualPath) = actualPath
simplify other                    = other

defaultEndings :: [Ending]
defaultEndings = [
        "sdig1o", "aikp25", "vzt35d", "fs7g5r", "9hf347",
        "f853q7", "pwi5q3", "weu429", "8fwr7h", "hu5p73",
        "yle8rf", "mdl20a", "84hrui", "c8vb3w", "la9inh",
        "k6bghz", "jwb54g", "4geu7y", "nbc6t4", "amv6zh"
    ]

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
cleanLatex sheetFile = callProcess "rm" [concat [sheetFile, ".log"], 
                                         concat [sheetFile, ".aux"],
                                         concat [sheetFile, ".tex"],
                                         variableFile
                                        ]

cleanImages :: [Ending] -> IO ()
cleanImages es = callProcess "rm" (map (++ ".png") es)