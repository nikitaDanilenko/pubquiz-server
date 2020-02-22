{-# LANGUAGE TemplateHaskell #-}

module General.Labels
  ( Labels
  , SafeLabels
  , unwrapped
  , defaultLabels
  , mkLabels
  , labelsFromParameterList
  , teamLabel
  , ownPointsLabel
  , maxReachedLabel
  , maxReachableLabel
  , backToChartView
  , ownPageLabel
  , mainLabel
  , roundLabel
  , viewPrevious
  , cumulativeLabel
  , individualRoundsLabel
  , progressionLabel
  , placementLabel
  , placeLabel
  , pointsLabel
  , roundWinnerLabel
  , mkHTMLSafe
  , showAsBS
  , parameters
  , fallbackLabels
  ) where

import           Control.Applicative           ((*>))
import qualified Data.ByteString.Char8         as B
import           Text.Parsec.Language          (haskellDef)
import           Text.Parsec.Prim              (parse)
import           Text.Parsec.Token             (makeTokenParser, stringLiteral)
import           Text.ParserCombinators.Parsec (Parser, char, choice, spaces,
                                                string, try)

import           Data.Aeson.TH                 (defaultOptions, deriveJSON)
import           General.Types                 (BackToChartViewLabel,
                                                CumulativeLabel,
                                                Fallback (fallback),
                                                IndividualRoundsLabel,
                                                MainLabel, MaxReachableLabel,
                                                MaxReachedLabel, OwnPageLabel,
                                                OwnPointsLabel, PlaceLabel,
                                                PlacementLabel, PointsLabel,
                                                ProgressionLabel, RoundLabel,
                                                RoundWinnerLabel, TeamLabel,
                                                Unwrappable (unwrap, wrap),
                                                ViewPreviousLabel)
import           Pages.HtmlUtil                (htmlSafeString)

newtype SafeLabels =
  SafeLabels
    { unwrapped :: Labels
    }

-- todo: remove fallbacks, parsers, otherwise unused functions
-- todo: remove show instance
data Labels =
  Labels
    { roundLabel            :: RoundLabel
    , teamLabel             :: TeamLabel
    , ownPointsLabel        :: OwnPointsLabel
    , maxReachedLabel       :: MaxReachedLabel
    , maxReachableLabel     :: MaxReachableLabel
    , backToChartView       :: BackToChartViewLabel
    , mainLabel             :: MainLabel
    , ownPageLabel          :: OwnPageLabel
    , viewPrevious          :: ViewPreviousLabel
    , cumulativeLabel       :: CumulativeLabel
    , individualRoundsLabel :: IndividualRoundsLabel
    , progressionLabel      :: ProgressionLabel
    , placementLabel        :: PlacementLabel
    , placeLabel            :: PlaceLabel
    , pointsLabel           :: PointsLabel
    , roundWinnerLabel      :: RoundWinnerLabel
    }

deriveJSON defaultOptions ''Labels

instance Read Labels where
  readsPrec _ text =
    case parse labelsParser "" text of
      Right lbls -> [(lbls, "")]
      Left _     -> []

parameters :: Labels -> [String]
parameters lbls =
  map
    ($ lbls)
    [ unwrap . roundLabel
    , unwrap . teamLabel
    , unwrap . ownPointsLabel
    , unwrap . maxReachedLabel
    , unwrap . maxReachableLabel
    , unwrap . backToChartView
    , unwrap . mainLabel
    , unwrap . ownPageLabel
    , unwrap . viewPrevious
    , unwrap . cumulativeLabel
    , unwrap . individualRoundsLabel
    , unwrap . progressionLabel
    , unwrap . placementLabel
    , unwrap . placeLabel
    , unwrap . pointsLabel
    , unwrap . roundWinnerLabel
    ]

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
originalKeys =
  [ roundKey
  , teamKey
  , ownPointsKey
  , maxReachedKey
  , maxReachableKey
  , backToChartKey
  , mainKey
  , ownPageKey
  , viewPreviousKey
  ]

additionalChartsKeys :: [String]
additionalChartsKeys = originalKeys ++ [cumulativeKey, individualRoundsKey, progressionKey]

placementsKeys :: [String]
placementsKeys = additionalChartsKeys ++ [placementKey, placeKey, pointsKey, roundWinnerKey]

labelsFromParameterList :: [String] -> Labels
labelsFromParameterList ws =
  case ws of
    [r, t, op, mred, mr, btc, m, o, vp] ->
      mkLabels
        r
        t
        op
        mred
        mr
        btc
        m
        o
        vp
        cumulativeFallback
        individualRoundsFallback
        progressionFallback
        placementFallback
        placeFallback
        pointsFallback
        roundWinnerFallback
    [r, t, op, mred, mr, btc, m, o, vp, c, i, p] ->
      mkLabels r t op mred mr btc m o vp c i p placementFallback placeFallback pointsFallback roundWinnerFallback
    r:t:op:mred:mr:btc:m:o:vp:c:i:p:plcm:plc:ps:rw:_ -> mkLabels r t op mred mr btc m o vp c i p plcm plc ps rw
    _ -> defaultLabels

labelsParser :: Parser Labels
labelsParser = fmap labelsFromParameterList (choice (map try [withOriginal, withAdditionalCharts, withPlacements]))

constructor :: String
constructor = "Labels"

argumentSeparator :: Char
argumentSeparator = ','

intersperse :: String -> [Parser a] -> Parser [a]
intersperse _ [] = pure []
intersperse _ [p] = fmap pure (spaces *> p)
intersperse separator (p:ps) = do
  spaces
  r <- p
  spaces
  string separator
  rs <- intersperse separator ps
  return (r : rs)

stringParser :: Parser String
stringParser = stringLiteral (makeTokenParser haskellDef)

keyValueParser :: String -> Parser String
keyValueParser key = string key *> spaces *> char '=' *> spaces *> stringParser

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

-- todo: use text directly
mkLabels ::
     String
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
mkLabels roundLbl teamLbl ownPointsLbl maxReachedLbl maxReachableLbl backLbl mainLbl ownPageLbl viewPreviousLbl cumulativeLbl individualRoundsLbl progressionLbl placementLbl placeLbl pointsLbl roundWinnerLbl =
  Labels
    { roundLabel = wrap roundLbl
    , teamLabel = wrap teamLbl
    , ownPointsLabel = wrap ownPointsLbl
    , maxReachedLabel = wrap maxReachedLbl
    , maxReachableLabel = wrap maxReachableLbl
    , backToChartView = wrap backLbl
    , mainLabel = wrap mainLbl
    , ownPageLabel = wrap ownPageLbl
    , viewPrevious = wrap viewPreviousLbl
    , cumulativeLabel = wrap cumulativeLbl
    , individualRoundsLabel = wrap individualRoundsLbl
    , progressionLabel = wrap progressionLbl
    , placementLabel = wrap placementLbl
    , placeLabel = wrap placeLbl
    , pointsLabel = wrap pointsLbl
    , roundWinnerLabel = wrap roundWinnerLbl
    }

mkHTMLSafe :: Labels -> SafeLabels
mkHTMLSafe lbls = SafeLabels sfLbls
  where
    sfLbls =
      Labels
        { roundLabel = wrap (htmlSafeString (unwrap (roundLabel lbls)))
        , teamLabel = wrap (htmlSafeString (unwrap (teamLabel lbls)))
        , ownPointsLabel = wrap (htmlSafeString (unwrap (ownPointsLabel lbls)))
        , maxReachedLabel = wrap (htmlSafeString (unwrap (maxReachedLabel lbls)))
        , maxReachableLabel = wrap (htmlSafeString (unwrap (maxReachableLabel lbls)))
        , backToChartView = wrap (htmlSafeString (unwrap (backToChartView lbls)))
        , mainLabel = wrap (htmlSafeString (unwrap (mainLabel lbls)))
        , ownPageLabel = wrap (htmlSafeString (unwrap (ownPageLabel lbls)))
        , viewPrevious = wrap (htmlSafeString (unwrap (viewPrevious lbls)))
        , cumulativeLabel = wrap (htmlSafeString (unwrap (cumulativeLabel lbls)))
        , individualRoundsLabel = wrap (htmlSafeString (unwrap (individualRoundsLabel lbls)))
        , progressionLabel = wrap (htmlSafeString (unwrap (progressionLabel lbls)))
        , placementLabel = wrap (htmlSafeString (unwrap (placementLabel lbls)))
        , placeLabel = wrap (htmlSafeString (unwrap (placeLabel lbls)))
        , pointsLabel = wrap (htmlSafeString (unwrap (pointsLabel lbls)))
        , roundWinnerLabel = wrap (htmlSafeString (unwrap (roundWinnerLabel lbls)))
        }

{- Returns the same result as show, but uses ByteStrings for the actual values. -}
showAsBS :: [B.ByteString] -> B.ByteString
showAsBS bss =
  B.concat
    [ B.pack constructor
    , B.pack " {"
    , B.intercalate
        (B.pack ", ")
        (zipWith
           (\k v -> B.unwords [B.pack k, B.pack "=", B.concat [B.pack "\"", secure v, B.pack "\""]])
           placementsKeys
           bss)
    , B.pack "}"
    ]
  where
    secure =
      B.concatMap
        (\c ->
           case c of
             '\"' -> B.pack "\\\""
             '\\' -> B.pack "\\\\"
             _    -> B.singleton c)

-- todo: remove
defaultLabels :: Labels
defaultLabels = fallbackLabels

fallbackLabels :: Labels
fallbackLabels =
  Labels
    (fallback :: RoundLabel)
    (fallback :: TeamLabel)
    (fallback :: OwnPointsLabel)
    (fallback :: MaxReachedLabel)
    (fallback :: MaxReachableLabel)
    (fallback :: BackToChartViewLabel)
    (fallback :: MainLabel)
    (fallback :: OwnPageLabel)
    (fallback :: ViewPreviousLabel)
    (fallback :: CumulativeLabel)
    (fallback :: IndividualRoundsLabel)
    (fallback :: ProgressionLabel)
    (fallback :: PlacementLabel)
    (fallback :: PlaceLabel)
    (fallback :: PointsLabel)
    (fallback :: RoundWinnerLabel)
