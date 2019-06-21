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
tagged t text = concat [open, text, close] where
  (open, close) = tag t

mkButton :: String -> String
mkButton = mkButtonTo "./index.html"

mkButtonTo :: String -> String -> String
mkButtonTo path text = concat ["<a href=\"", path, "\" class=\"button\">", text, "</a>"]

pageHeader :: String
pageHeader = "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">\n"