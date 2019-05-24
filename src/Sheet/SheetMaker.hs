module Main where

import System.Environment ( getArgs )
import System.Process     ( callProcess )

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

createPDF :: [Ending] -> IO ()
createPDF es = do
    createLatexVariable es
    callProcess "pdflatex" [concat [sheet, ".tex"]]

cleanLatex :: IO ()
cleanLatex = callProcess "rm" [concat [sheet, ".log"], concat [sheet, ".aux"]]

cleanImages :: [Ending] -> IO ()
cleanImages es = callProcess "rm" (map (++ ".png") es)

main :: IO ()
main = do
    prefix : endings <- getArgs
    mapM_ (createQR prefix) endings
    createPDF endings
    cleanImages endings
    cleanLatex