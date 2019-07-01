module Labels ( Labels, defaultLabels, mkLabels, groupLabel, ownPointsLabel,
                maxReachedLabel, maxReachableLabel, backToChartView, ownPageLabel,
                htmlSafeString, mainLabel, roundLabel, unEscape, viewPrevious,
                cumulativeLabel, individualRoundsLabel, progressionLabel ) where

data Labels = Labels { 
  roundLabel :: String,
  groupLabel :: String,
  ownPointsLabel :: String, 
  maxReachedLabel :: String,
  maxReachableLabel :: String,
  backToChartView :: String,
  mainLabel :: String,
  ownPageLabel :: String,
  viewPrevious :: String,
  cumulativeLabel :: String,
  individualRoundsLabel :: String,
  progressionLabel :: String
} deriving (Show, Read)

mkLabels :: String
         -> String
         -> String
         -> String
         -> String
         -> String
         -> String
         -> String
         -> String
         -> String
         -> String
         -> String
         -> Labels
mkLabels roundLbl groupLbl ownPointsLbl maxReachedLbl maxReachableLbl backLbl mainLbl ownPageLbl 
         viewPreviousLbl cumulativeLbl individualRoundsLbl progressionLbl =
    Labels {
        roundLabel = htmlSafeString roundLbl,
        groupLabel = htmlSafeString groupLbl,
        ownPointsLabel = htmlSafeString ownPointsLbl,
        maxReachedLabel = htmlSafeString maxReachedLbl,
        maxReachableLabel = htmlSafeString maxReachableLbl,
        backToChartView = htmlSafeString backLbl,
        mainLabel = htmlSafeString mainLbl,
        ownPageLabel = htmlSafeString ownPageLbl,
        viewPrevious = htmlSafeString viewPreviousLbl,
        cumulativeLabel = htmlSafeString cumulativeLbl,
        individualRoundsLabel = htmlSafeString individualRoundsLbl,
        progressionLabel = htmlSafeString progressionLbl
    } 

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

defaultLabels :: Labels
defaultLabels = mkLabels
   "Runde"
   "Gruppe"
   "Erreichte Punkte"
   "Erreichte Höchstpunktzahl"
   "Erreichbare Punkte"
   "Gesamtansicht"
   "Pubquiz"
   "Eigene Punkte"
   "Alle Quizzes"
   "Gesamtpunkte"
   "Punkte pro Runde"
   "Verlauf"