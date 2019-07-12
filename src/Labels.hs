module Labels ( Labels, defaultLabels, mkLabels, labelsFromParameterList,
                teamLabel, ownPointsLabel,
                maxReachedLabel, maxReachableLabel, backToChartView, ownPageLabel,
                mainLabel, roundLabel, viewPrevious, cumulativeLabel, individualRoundsLabel,
                progressionLabel, placementLabel, placeLabel, pointsLabel, roundWinnerLabel ) where

import Control.Applicative           ( (*>) )
import Text.Parsec.Language          ( haskellDef )
import Text.Parsec.Prim              ( parse )
import Text.Parsec.Token             ( makeTokenParser, stringLiteral )
import Text.ParserCombinators.Parsec ( Parser, spaces, char, choice, string, try )

import Pages.HtmlUtil                ( htmlSafeString )

data Labels = Labels { 
  roundLabel :: String,
  teamLabel :: String,
  ownPointsLabel :: String, 
  maxReachedLabel :: String,
  maxReachableLabel :: String,
  backToChartView :: String,
  mainLabel :: String,
  ownPageLabel :: String,
  viewPrevious :: String,

  cumulativeLabel :: String,
  individualRoundsLabel :: String,
  progressionLabel :: String,
  
  placementLabel :: String,
  placeLabel :: String,
  pointsLabel :: String,
  roundWinnerLabel :: String

} deriving Show

instance Read Labels where
  readsPrec _ text = case parse labelsParser "" text of
    Right labels -> [(labels, "")]
    Left  _ -> []

cumulativeFallback :: String
cumulativeFallback = "Gesamtwertung"

individualRoundsFallback :: String
individualRoundsFallback = "Punkte pro Runde"

progressionFallback :: String
progressionFallback = "Verlauf"

placementFallback :: String
placementFallback = "Platzierung"

placeFallback :: String
placeFallback = "Platz"

pointsFallback :: String
pointsFallback = "Punkte"

roundWinnerFallback :: String
roundWinnerFallback = "Rundensieger"

roundKey :: String
roundKey = "roundLabel"

teamKey :: String
teamKey = "teamLabel"

ownPointsKey :: String
ownPointsKey = "ownPointsLabel"

maxReachedKey :: String
maxReachedKey = "maxReachedLabel"

maxReachableKey :: String
maxReachableKey = "maxReachableLabel"

backToChartKey :: String
backToChartKey = "backToChartView"

mainKey :: String
mainKey = "mainLabel"

ownPageKey :: String
ownPageKey = "ownPageLabel"

viewPreviousKey :: String
viewPreviousKey = "viewPrevious"

cumulativeKey :: String
cumulativeKey = "cumulativeLabel"

individualRoundsKey :: String
individualRoundsKey = "individualRoundsLabel"

progressionKey :: String
progressionKey = "progressionLabel"

placementKey :: String
placementKey = "placementLabel"

placeKey :: String
placeKey = "placeLabel"

pointsKey :: String
pointsKey = "pointsLabel"

roundWinnerKey :: String
roundWinnerKey = "roundWinnerLabel"

originalKeys :: [String]
originalKeys = [ 
    roundKey, teamKey, ownPointsKey, 
    maxReachedKey, maxReachableKey, backToChartKey, 
    mainKey, ownPageKey, viewPreviousKey
  ]

additionalChartsKeys :: [String]
additionalChartsKeys = originalKeys ++ [cumulativeKey, individualRoundsKey, progressionKey]

placementsKeys :: [String]
placementsKeys = additionalChartsKeys ++ [placementKey, placeKey, pointsKey, roundWinnerKey]

labelsFromParameterList :: [String] -> Labels
labelsFromParameterList ws = case ws of
  r : t : op : mred : mr : btc : m : o : vp : [] -> 
      mkLabels r t op mred mr btc m o vp
             cumulativeFallback individualRoundsFallback progressionFallback
             placementFallback placeFallback pointsFallback roundWinnerFallback
  r : t : op : mred : mr : btc : m : o : vp : c : i : p : [] ->
      mkLabels r t op mred mr btc m o vp c i p 
             placementFallback placeFallback pointsFallback roundWinnerFallback
  r : t : op : mred : mr : btc : m : o : vp : c : i : p : plcm : plc : ps : rw : _ ->
      mkLabels r t op mred mr btc m o vp c i p plcm plc ps rw
  _ -> defaultLabels

labelsParser :: Parser Labels
labelsParser = 
  fmap labelsFromParameterList 
       (choice (map try [withOriginal, withAdditionalCharts, withPlacements]))

constructor :: String
constructor = "Labels"

argumentSeparator :: Char
argumentSeparator = ','

intersperse :: String -> [Parser a] -> Parser [a]
intersperse _         []       = pure []
intersperse _         [p]      = fmap pure (spaces *> p)
intersperse separator (p : ps) = do
  spaces
  r <- p
  spaces
  string separator
  rs <- intersperse separator ps
  return (r : rs)

stringParser :: Parser String
stringParser = stringLiteral (makeTokenParser haskellDef)

keyValueParser :: String -> Parser String
keyValueParser key = 
  string key *>
  spaces *>
  char '=' *>
  spaces *>
  stringParser

withKeyList :: [String] -> Parser [String]
withKeyList keys = do
  string constructor
  spaces
  char '{'
  spaces
  args <- intersperse [argumentSeparator] (map keyValueParser keys)
  spaces
  char '}'
  return args

withOriginal :: Parser [String]
withOriginal = withKeyList originalKeys

withAdditionalCharts :: Parser [String]
withAdditionalCharts = withKeyList additionalChartsKeys

withPlacements :: Parser [String]
withPlacements = withKeyList placementsKeys

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
         -> String
         -> String
         -> String
         -> String
         -> Labels
mkLabels roundLbl teamLbl ownPointsLbl maxReachedLbl maxReachableLbl backLbl mainLbl ownPageLbl 
         viewPreviousLbl cumulativeLbl individualRoundsLbl progressionLbl
         placementLbl placeLbl pointsLbl roundWinnerLbl =
    Labels {
        roundLabel = htmlSafeString roundLbl,
        teamLabel = htmlSafeString teamLbl,
        ownPointsLabel = htmlSafeString ownPointsLbl,
        maxReachedLabel = htmlSafeString maxReachedLbl,
        maxReachableLabel = htmlSafeString maxReachableLbl,
        backToChartView = htmlSafeString backLbl,
        mainLabel = htmlSafeString mainLbl,
        ownPageLabel = htmlSafeString ownPageLbl,
        viewPrevious = htmlSafeString viewPreviousLbl,
        cumulativeLabel = htmlSafeString cumulativeLbl,
        individualRoundsLabel = htmlSafeString individualRoundsLbl,
        progressionLabel = htmlSafeString progressionLbl,
        placementLabel = htmlSafeString placementLbl,
        placeLabel = htmlSafeString placeLbl,
        pointsLabel = pointsLbl,
        roundWinnerLabel = roundWinnerLbl
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
   "Platzierungen"
   "Platz"
   "Punkte"
   "Rundensiegen"