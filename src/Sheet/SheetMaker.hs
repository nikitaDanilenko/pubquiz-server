module Sheet.SheetMaker ( createSheetWith ) where

import System.Directory   ( setCurrentDirectory, getCurrentDirectory )
import System.Process     ( callProcess )

import Constants          ( quizzesFolderIO, addSeparator )
import Sheet.Tex          ( mkSheet )

type Prefix = String
type Server = String
type Ending = String

createSheetWith :: String -> Int -> Prefix -> Server -> [Ending] -> IO ()
createSheetWith groupLabel rounds prefix server endings = do
    quizzesFolder <- quizzesFolderIO
    currentDir <- getCurrentDirectory
    let fullPath = addSeparator [quizzesFolder, prefix]
        fullServerPath = addSeparator [server, quizzesFolder, prefix, ""]
        sht = mkSheet groupLabel rounds
        sheetFile = mkSheetFile prefix
        texFile = concat [sheetFile, ".tex"]
    setCurrentDirectory fullPath
    writeFile texFile sht 
    mapM_ (createQR fullServerPath) endings

    createPDF texFile endings
    cleanImages endings
    cleanLatex sheetFile
    setCurrentDirectory currentDir

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
    callProcess "pdflatex" [texFile]

cleanLatex :: String -> IO ()
cleanLatex sheetFile = callProcess "rm" [concat [sheetFile, ".log"], 
                                         concat [sheetFile, ".aux"],
                                         concat [sheetFile, ".tex"],
                                         variableFile
                                        ]

cleanImages :: [Ending] -> IO ()
cleanImages es = callProcess "rm" (map (++ ".png") es)