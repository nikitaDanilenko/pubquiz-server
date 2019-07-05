module Pages.RoundsParser ( parseCodesWithMaybeNames ) where

import Control.Applicative           ( (<|>) )
import Text.Parsec.Prim              ( parse )
import Text.ParserCombinators.Parsec ( Parser, many1, oneOf, spaces, char, sepBy, noneOf )
import Utils                         ( alphaNumeric )

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
codeWithMaybeNameParser =
    fmap (\s -> (s, Nothing)) codeParser <|> codeWithNameParser

codesWithMaybeNamesParser :: Parser [(String, Maybe String)]
codesWithMaybeNamesParser = sepBy codeWithMaybeNameParser spaces

parseCodesWithMaybeNames :: String -> [(String, Maybe String)]
parseCodesWithMaybeNames text = 
    case parse codesWithMaybeNamesParser "" text of
        Left _       -> []
        Right result -> result