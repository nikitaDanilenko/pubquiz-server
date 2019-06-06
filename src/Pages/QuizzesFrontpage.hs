module Pages.QuizzesFrontpage where

import Control.Monad    ( filterM )
import Data.List        ( sortBy )
import Data.Ord         ( comparing )
import System.Directory ( getDirectoryContents, doesFileExist )

import Constants        ( quizzesFolderIO, addSeparator, labelsFile )
import Labels           ( Labels, mainLabel )
import Pages.HtmlUtil   ( tagged, div )

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
    let lbls = map (read :: String -> Labels) preLabels
        zipped = zip candidates lbls
        content = mkHtml (sortBy (comparing fst) zipped)
    writeFile (addSeparator [quizzesFolder, indexFile]) content

maybeQuiz :: FilePath -> FilePath -> IO Bool
maybeQuiz parent relative = 
    if relative `notElem` [".", ".."]
        then doesFileExist (addSeparator [parent, relative, labelsFile])
        else pure False

cssPath :: String
cssPath = "<link rel='stylesheet' type='text/css' href='./style.css'/>"

mkHtml :: [(String, Labels)] -> String
mkHtml cls = 
    tagged "html" (
           tagged "head" (tagged "title" "Quizzes" ++ cssPath) ++
           concatMap (\(c, l) -> div (mkButton (mainLabel l) c)) cls
    )

mkButton :: String -> String -> String
mkButton text path = concat ["<a href=\"", path, "\" class=\"quizLinkButton\">", text, "</a>"]

indexFile :: String
indexFile = "index.html"