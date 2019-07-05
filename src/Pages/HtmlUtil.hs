module Pages.HtmlUtil where

import Prelude hiding ( div )

centerDiv :: String -> String
centerDiv = div . tagged "center"

div :: String -> String
div = tagged "div"

h1 :: String -> String
h1 = tagged "h1"

h1With :: String -> String -> String
h1With attrs text = concat [openWith, text, close] where
  (_, close) = tag "h1"
  openWith = concat ["<h1 ", attrs, ">"]

tableCell :: String -> String
tableCell = tagged "td"

tableRow :: String -> String
tableRow  = (++ "\n") . tagged "tr"

headerCell :: String -> String
headerCell = tagged "th"

tag :: String -> (String, String)
tag t = (concat ["<", t, ">"], concat ["</", t, ">"])

tagged :: String -> String -> String
tagged t text = concat (tagGroup t text)

taggedWith :: String -> String -> String -> String
taggedWith attrs t text = concat (tagGroupWith attrs t text)

taggedV :: String -> String -> String
taggedV t text = unlines (tagGroup t text)

taggedVWith :: String -> String -> String -> String
taggedVWith attrs t text = unlines (tagGroupWith attrs t text)

tagGroup :: String -> String -> [String]
tagGroup t text = [open, text, close] where
  (open, close) = tag t

tagGroupWith :: String -> String -> String -> [String]
tagGroupWith attrs t text = [open, text, close] where
    open = concat ["<", t, " ", attrs, ">"]
    close = concat ["</", t, ">"]

mkButton :: String -> String
mkButton = mkButtonTo "./index.html"

mkButtonTo :: String -> String -> String
mkButtonTo path = taggedWith (concat ["href=\"", path, "\" class=\"button\""]) "a"

pageHeader :: String
pageHeader = "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">\n"

htmlSafeChar :: Char -> String
htmlSafeChar 'ö' = "&ouml;"
htmlSafeChar 'ä' = "&auml;"
htmlSafeChar 'ü' = "&uuml;"
htmlSafeChar 'ß' = "&szlig;"
htmlSafeChar '<' = "&lt;"
htmlSafeChar '>' = "&gt;"
htmlSafeChar '\n' = "&nbsp;"
htmlSafeChar c = [c]

doubleEscape :: String -> String
doubleEscape [] = []
doubleEscape str = prefix ++ doubleEscape rest where
  (prefix, rest) = case str of
    '\195' : '\164' : cs -> ("&auml;", cs)
    '\195' : '\182' : cs -> ("&ouml;", cs)
    '\195' : '\188' : cs -> ("&uuml;", cs)
    '\195' : '\132' : cs -> ("&Auml;", cs)
    '\195' : '\150' : cs -> ("&Ouml;", cs)
    '\195' : '\156' : cs -> ("&Uuml;", cs)
    '\195' : '\159' : cs -> ("&szlig;", cs)
    c : cs               -> ([c], cs)
    []                   -> ([], [])

unEscape :: String -> String
unEscape [] = []
unEscape str = prefix ++ unEscape rest where
  (prefix, rest) = case str of
    '&' : 'a' : 'u' : 'm' : 'l' : ';' : cs       -> ("ä", cs)
    '&' : 'o' : 'u' : 'm' : 'l' : ';' : cs       -> ("ö", cs)
    '&' : 'u' : 'u' : 'm' : 'l' : ';' : cs       -> ("ü", cs)
    '&' : 'A' : 'u' : 'm' : 'l' : ';' : cs       -> ("Ä", cs)
    '&' : 'O' : 'u' : 'm' : 'l' : ';' : cs       -> ("Ö", cs)
    '&' : 'U' : 'u' : 'm' : 'l' : ';' : cs       -> ("Ü", cs)
    '&' : 's' : 'z' : 'l' : 'i' : 'g' : ';' : cs -> ("ß", cs)
    '&' : 'l' : 't' : ';' : cs                   -> ("<", cs)
    '&' : 'g' : 't' : ';' : cs                   -> ("g", cs)
    '&' : 'n' : 'b' : 's' : 'p' : ';' : cs       -> ("\n", cs)
    c : cs                                       -> ([c], cs)
    []                                           -> ([], [])

htmlSafeString :: String -> String
htmlSafeString = doubleEscape . concatMap htmlSafeChar