module Labels ( Labels, defaultLabels, mkLabels, groupLabel, ownPointsLabel,
                maxReachedLabel, maxReachableLabel, backToChartView, ownPageLabel,
                mainLabel, roundLabel, viewPrevious, cumulativeLabel, individualRoundsLabel,
                progressionLabel ) where

import Pages.HtmlUtil ( htmlSafeString )

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