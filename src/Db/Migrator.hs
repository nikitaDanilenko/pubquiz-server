{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Db.Migrator where

import           Control.Applicative           ((<|>))
import           Control.Arrow                 (first)
import           Data.Maybe                    (fromMaybe)
import qualified Data.Text                     as T
import           Data.Time.Calendar            (Day)
import           Db.DbConversion               (Header (..),
                                                QuizIdentifier (..),
                                                QuizRatings (..), Ratings (..),
                                                RoundRating (..), TeamInfo (..),
                                                TeamRating (..), quizId, quizId)
import           Db.Storage                    (createQuiz, setLabels,
                                                setQuizRatings)
import           General.Types                 (Activity (Active), QuizName,
                                                RoundNumber, fallback, unwrap,
                                                wrap)
import           GHC.Natural                   (Natural)
import           Text.Parsec.Prim              (parse)
import           Text.ParserCombinators.Parsec (Parser, char, many1, noneOf,
                                                oneOf, sepBy, spaces)

alphaNumeric :: String
alphaNumeric = ['0' .. '9'] ++ ['a' .. 'z']

codeParser :: Parser String
codeParser = many1 (oneOf alphaNumeric)

codeWithNameParser :: Parser (String, Maybe String)
codeWithNameParser = do
  char '('
  spaces
  code <- codeParser
  spaces
  char '|'
  spaces
  char '\\'
  name <- fmap Just (many1 (noneOf "\\")) <|> pure Nothing
  char '\\'
  spaces
  char ')'
  return (code, name)

codeWithMaybeNameParser :: Parser (String, Maybe String)
codeWithMaybeNameParser = fmap (, Nothing) codeParser <|> codeWithNameParser

codesWithMaybeNamesParser :: Parser [(String, Maybe String)]
codesWithMaybeNamesParser = sepBy codeWithMaybeNameParser spaces

parseCodesWithMaybeNames :: String -> [(String, Maybe String)]
parseCodesWithMaybeNames text =
  case parse codesWithMaybeNamesParser "" text of
    Left _       -> []
    Right result -> result

parseRound :: String -> RoundRating
parseRound text =
  RoundRating {reachableInRound = reachable, points = zipWith (TeamRating . wrap) [1 :: Natural ..] points}
  where
    reachable:_:points = map read (words text)

parseRounds :: [String] -> Ratings
parseRounds = Ratings . zipWith (\i ps -> (wrap i, ps)) [1 :: Natural ..] . map parseRound

parseQuiz :: String -> QuizRatings
parseQuiz text = QuizRatings (Header header) ratings
  where
    headLine:roundLines = lines text
    ratings = parseRounds roundLines
    numberOfTeams = length $ points $ snd $ head (unwrap ratings :: [(RoundNumber, RoundRating)])
    header =
      zipWith
        (\i (code, mName) -> TeamInfo (wrap code) (wrap (fromMaybe (unwords ["Gruppe", show i]) mName)) (wrap i) Active)
        [1 :: Natural ..]
        (take numberOfTeams (parseCodesWithMaybeNames headLine))

writeQuiz :: String -> String -> String -> IO ()
writeQuiz name date ratings = do
  quizInfo <- createQuiz (QuizIdentifier (wrap ("Café Godot" :: T.Text)) (wrap (read date :: Day)) (wrap name))
  let qid = quizId quizInfo
  setLabels qid fallback
  setQuizRatings qid (parseQuiz ratings)

writeQuizFromFile :: String -> IO ()
writeQuizFromFile filePath = do
  text <- readFile filePath
  let name : date : ratingsLines = lines text
  writeQuiz name date (unlines ratingsLines)

writeAll :: IO ()
writeAll =
  mapM_ (\folder -> writeQuizFromFile ("remote/" ++ folder ++ "/rounds.txt"))
    [
      "2019-02-16",
      "2019-05-15",
      "2019-05-22",
      "2019-05-29",
      "2019-06-03",
      "2019-06-12",
      "2019-06-19",
      "2019-07-03",
      "2019-07-17",
      "2019-07-26",
      "2019-07-31",
      "2019-08-14",
      "2019-09-04",
      "2019-09-18",
      "2019-10-09",
      "2019-10-16",
      "2019-10-23",
      "2019-10-30",
      "2019-11-06",
      "2019-11-13",
      "2019-11-20",
      "2019-11-27",
      "2019-12-04",
      "2019-12-11",
      "2019-12-18",
      "2020-01-08",
      "2020-01-29",
      "2020-02-19",
      "2020-02-26",
      "2020-03-18",
      "2020-03-25",
      "2020-04-01"
    ]