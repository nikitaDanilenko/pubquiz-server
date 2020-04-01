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
  quizInfo <- createQuiz (QuizIdentifier (wrap name) (wrap (read date :: Day)) (wrap ("Café Godot" :: T.Text)))
  let qid = quizId quizInfo
  setLabels qid fallback
  setQuizRatings qid (parseQuiz ratings)
