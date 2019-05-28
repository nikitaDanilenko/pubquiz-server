module Labels ( Labels, defaultLabels, mkLabels, groupLabel, ownPointsLabel,
                maxReachedLabel, maxReachableLabel, backToChartView, ownPageLabel,
                htmlSafeString, mainLabel, roundLabel ) where

data Labels = Labels { 
  roundLabel :: String,
  groupLabel :: String,
  ownPointsLabel :: String, 
  maxReachedLabel :: String,
  maxReachableLabel :: String,
  backToChartView :: String,
  mainLabel :: String,
  ownPageLabel :: String
} deriving (Show, Read)

mkLabels :: String -> String -> String -> String -> String -> String -> String -> String -> Labels
mkLabels roundLbl groupLbl ownPointsLbl maxReachedLbl maxReachableLbl backLbl mainLbl ownPageLbl =
    Labels {
        roundLabel = htmlSafeString roundLbl,
        groupLabel = htmlSafeString groupLbl,
        ownPointsLabel = htmlSafeString ownPointsLbl,
        maxReachedLabel = htmlSafeString maxReachedLbl,
        maxReachableLabel = htmlSafeString maxReachableLbl,
        backToChartView = backLbl,
        mainLabel = mainLbl,
        ownPageLabel = ownPageLbl
    } 

htmlSafeChar :: Char -> String
htmlSafeChar 'ö' = "&ouml;"
htmlSafeChar 'ä' = "&auml;"
htmlSafeChar 'ü' = "&uuml;"
htmlSafeChar 'ß' = "&szlig;"
htmlSafeChar c = [c]

htmlSafeString :: String -> String
htmlSafeString = concatMap htmlSafeChar

defaultLabels :: Labels
defaultLabels = Labels { 
  roundLabel = htmlSafeString "Runde",
  groupLabel = htmlSafeString "Gruppe",
  ownPointsLabel = htmlSafeString "Erreichte Punkte", 
  maxReachedLabel = htmlSafeString "Erreichte Höchstpunktzahl",
  maxReachableLabel = htmlSafeString "Erreichbare Punkte",
  backToChartView = htmlSafeString "Gesamtansicht",
  mainLabel = htmlSafeString "Pubquiz",
  ownPageLabel = htmlSafeString "Eigene Punkte"
}