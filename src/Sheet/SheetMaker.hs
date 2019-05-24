module SheetMaker where

import System.Directory   ( setCurrentDirectory, copyFile, getCurrentDirectory )
import System.Environment ( getArgs )
import System.Process     ( callProcess )

import Constants          ( quizzesFolderIO, addSeparator )
import Sheet.Tex          ( mkSheet )

type Prefix = String
type Ending = String

createQR :: Prefix -> Ending -> IO ()
createQR prefix ending = 
    callProcess "qrencode" [concat [prefix, ending, ".html"], concat ["-o", ending, ".png"]]

variableFile :: String
variableFile = "listVariable.tex"

createLatexVariable :: [Ending] -> IO ()
createLatexVariable es = writeFile variableFile variable where
    indexed = zip [1 ..] es

    variable :: String
    variable = unlines (map (\(i, str) -> concat ["\\mkRounds{", show i, "}{", str, "}" ]) indexed)

sheet :: String
sheet = "Sheet"

sheetTex :: String
sheetTex = concat [sheet, ".tex"]

createPDF :: [Ending] -> IO ()
createPDF es = do
    createLatexVariable es
    callProcess "pdflatex" [sheetTex]

cleanLatex :: IO ()
cleanLatex = callProcess "rm" [concat [sheet, ".log"], 
                               concat [sheet, ".aux"],
                               concat [sheet, ".tex"],
                               variableFile
                               ]

cleanImages :: [Ending] -> IO ()
cleanImages es = callProcess "rm" (map (++ ".png") es)

createSheetWith :: String -> Int -> Prefix -> [Ending] -> IO ()
createSheetWith groupLabel rounds prefix endings = do
    quizzesFolder <- quizzesFolderIO
    currentDir <- getCurrentDirectory
    let fullPath = addSeparator [quizzesFolder, prefix]
        sheet = mkSheet groupLabel rounds
    setCurrentDirectory fullPath
    writeFile sheetTex sheet 
    mapM_ (createQR prefix) endings

    createPDF endings
    cleanImages endings
    cleanLatex
    setCurrentDirectory currentDir


main :: IO ()
main = do
    prefix : endings <- getArgs
    mapM_ (createQR prefix) endings
    createPDF endings
    cleanImages endings
    cleanLatex