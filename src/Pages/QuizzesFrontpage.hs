module Pages.QuizzesFrontpage where

import Control.Monad    ( filterM )
import Data.List        ( sortBy, intercalate )
import Data.Ord         ( comparing )
import System.Directory ( getDirectoryContents, doesFileExist )

import Constants        ( quizzesFolderIO, addSeparator, labelsFile )
import General.Labels   ( Labels, mainLabel, defaultLabels )
import Pages.HtmlUtil   ( tagged, taggedV, encoding, pageHeader )
import General.Types    ( Unwrappable (unwrap) )

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
    pageHeader ++
    taggedV "html" (
           taggedV "head" (taggedV "title" "Quizzes" ++ intercalate "\n" [encoding, cssPath]) ++
           intercalate "\n" (map (\(c, l) -> tagged "div" (mkButton (unwrap $ mainLabel l) c)) cls)
    )

mkButton :: String -> String -> String
mkButton text path = concat ["<a href=\"", path, "/index.html", "\" class=\"quizLinkButton\">", text, "</a>"]

indexFile :: String
indexFile = "index.html"