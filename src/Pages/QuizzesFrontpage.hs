module Pages.QuizzesFrontpage where

import Control.Monad    ( filterM )
import Data.List        ( sortBy )
import Data.Ord         ( comparing )
import System.Directory ( getDirectoryContents, doesFileExist )

import Constants        ( quizzesFolderIO, addSeparator, labelsFile )
import Labels           ( Labels, mainLabel, defaultLabels )
import Pages.HtmlUtil   ( taggedV, div, encoding )

import Prelude hiding   ( div )

getAllCandidates :: IO [String]
getAllCandidates = do
    quizzesFolder <- quizzesFolderIO
    contents <- getDirectoryContents quizzesFolder
    filterM (maybeQuiz quizzesFolder) contents

createFrontPage :: IO ()
createFrontPage = do
    candidates <- getAllCandidates
    quizzesFolder <- quizzesFolderIO
    preLabels <- mapM (\c -> readFile (addSeparator [quizzesFolder, c, labelsFile])) candidates
    let lbls = map saferRead preLabels
        zipped = zip candidates lbls
        content = mkHtml (sortBy (comparing fst) zipped)
    writeFile (addSeparator [quizzesFolder, indexFile]) content

saferRead :: String -> Labels
saferRead [] = defaultLabels
saferRead text = read (head (lines text))

maybeQuiz :: FilePath -> FilePath -> IO Bool
maybeQuiz parent relative = 
    if relative `notElem` [".", ".."]
        then doesFileExist (addSeparator [parent, relative, labelsFile])
        else pure False

cssPath :: String
cssPath = "<link rel='stylesheet' type='text/css' href='./style.css'/>"

mkHtml :: [(String, Labels)] -> String
mkHtml cls = 
    taggedV "html" (
           encoding ++
           taggedV "head" (taggedV "title" "Quizzes" ++ cssPath) ++
           concatMap (\(c, l) -> div (mkButton (mainLabel l) c)) cls
    )

mkButton :: String -> String -> String
mkButton text path = unlines ["<a href=\"", path ++ "/index.html", "\" class=\"quizLinkButton\">", text, "</a>"]

indexFile :: String
indexFile = "index.html"