module Labels ( Labels, SafeLabels, unwrapped, defaultLabels, mkLabels, labelsFromParameterList,
                teamLabel, ownPointsLabel,
                maxReachedLabel, maxReachableLabel, backToChartView, ownPageLabel,
                mainLabel, roundLabel, viewPrevious, cumulativeLabel, individualRoundsLabel,
                progressionLabel, placementLabel, placeLabel, pointsLabel, roundWinnerLabel,
                mkHTMLSafe, showAsBS ) where

import Control.Applicative                  ( (*>) )
import qualified Data.ByteString.Char8 as B 
import Text.Parsec.Language                 ( haskellDef )
import Text.Parsec.Prim                     ( parse )
import Text.Parsec.Token                    ( makeTokenParser, stringLiteral )
import Text.ParserCombinators.Parsec        ( Parser, spaces, char, choice, string, try )

import Pages.HtmlUtil                       ( htmlSafeString )

newtype SafeLabels = SafeLabels { unwrapped :: Labels }

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
    Right lbls -> [(lbls, "")]
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
  spaces
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
        roundLabel = roundLbl,
        teamLabel = teamLbl,
        ownPointsLabel = ownPointsLbl,
        maxReachedLabel = maxReachedLbl,
        maxReachableLabel = maxReachableLbl,
        backToChartView = backLbl,
        mainLabel = mainLbl,
        ownPageLabel = ownPageLbl,
        viewPrevious = viewPreviousLbl,
        cumulativeLabel = cumulativeLbl,
        individualRoundsLabel = individualRoundsLbl,
        progressionLabel = progressionLbl,
        placementLabel = placementLbl,
        placeLabel = placeLbl,
        pointsLabel = pointsLbl,
        roundWinnerLabel = roundWinnerLbl
    }

mkHTMLSafe :: Labels -> SafeLabels
mkHTMLSafe lbls = SafeLabels sfLbls where
  sfLbls = Labels {
        roundLabel = htmlSafeString (roundLabel lbls),
        teamLabel = htmlSafeString (teamLabel lbls),
        ownPointsLabel = htmlSafeString (ownPointsLabel lbls),
        maxReachedLabel = htmlSafeString (maxReachedLabel lbls),
        maxReachableLabel = htmlSafeString (maxReachableLabel lbls),
        backToChartView = htmlSafeString (backToChartView lbls),
        mainLabel = htmlSafeString (mainLabel lbls),
        ownPageLabel = htmlSafeString (ownPageLabel lbls),
        viewPrevious = htmlSafeString (viewPrevious lbls),
        cumulativeLabel = htmlSafeString (cumulativeLabel lbls),
        individualRoundsLabel = htmlSafeString (individualRoundsLabel lbls),
        progressionLabel = htmlSafeString (progressionLabel lbls),
        placementLabel = htmlSafeString (placementLabel lbls),
        placeLabel = htmlSafeString (placeLabel lbls),
        pointsLabel = htmlSafeString (pointsLabel lbls),
        roundWinnerLabel = htmlSafeString (roundWinnerLabel lbls)
    }

{- Returns the same result as show, but uses ByteStrings for the actual values. -}
showAsBS :: [B.ByteString] -> B.ByteString
showAsBS bss = B.concat [
  B.pack constructor,
  B.pack " {",
  B.intercalate (B.pack ", ") 
                (zipWith (\k v -> B.unwords [B.pack k, 
                                             B.pack "=",
                                             B.concat [B.pack "\"", secure v, B.pack "\""]]) 
                         placementsKeys 
                         bss),
  B.pack "}"
  ] where
    secure = B.concatMap (\c -> if c == '\"' then B.pack "\\\"" else B.singleton c)

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