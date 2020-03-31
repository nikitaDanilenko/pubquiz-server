{-# LANGUAGE TupleSections #-}

module Db.Migrator where

import           Control.Applicative           ((<|>))
import           Control.Arrow                 (first)
import           Data.Maybe                    (fromMaybe)
import           Db.DbConversion               (QuizRatings (..), Ratings (..),
                                                RoundRating (..), TeamInfo (..),
                                                TeamRating (..))
import           General.Types                 (Active, wrap)
import           GHC.Natural                   (Natural)
import           Text.Parsec.Prim              (parse)
import           Text.ParserCombinators.Parsec (Parser, char, many1, noneOf,
                                                oneOf, sepBy, spaces)
import           Utils                         (hexadecimal)

codeParser :: Parser String
codeParser = many1 (oneOf hexadecimal)

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
parseQuiz text = QuizRatings header ratings
  where
    headLine:roundLines = lines text
    ratings = parseRounds roundLines
    header =
      zipWith
        (\i (code, mName) -> TeamInfo (wrap code) (wrap (fromMaybe (unwords ["Gruppe", show i]) mName)) (wrap i) Active)
        [1 :: Natural ..]
        (parseCodesWithMaybeNames headLine)
