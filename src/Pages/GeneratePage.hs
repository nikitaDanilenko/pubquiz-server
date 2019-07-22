module Pages.GeneratePage ( createWith, main ) where

import Control.Arrow          ( second, (&&&), (***), (>>>) )
import Control.Exception      ( catch )
import Control.Exception.Base ( IOException )
import Control.Monad          ( mfilter )
import Data.Function          ( on )
import Data.List              ( intercalate, maximumBy, sortBy, groupBy )
import Data.Map               ( Map, fromList, unionsWith, toList, lookup )
import Data.Maybe             ( fromMaybe )
import Data.Ord               ( comparing )
import System.Environment     ( getArgs )

import Prelude hiding         ( lookup, div )

import Labels                 ( Labels, mainLabel, ownPageLabel, backToChartView, roundLabel,
                                ownPageLabel, ownPointsLabel, maxReachedLabel, maxReachableLabel,
                                teamLabel, defaultLabels, viewPrevious, placeLabel, pointsLabel,
                                cumulativeLabel, progressionLabel, individualRoundsLabel,
                                placementLabel, roundWinnerLabel, mkHTMLSafe, SafeLabels, labels )
import Pages.HtmlUtil         ( centerDiv, h1With, tableCell, tableRow, headerCell, tag, tagged,
                                mkButton, mkButtonTo, pageHeader, div, taggedV, taggedWith,
                                htmlSafeString, encoding )
import Pages.RoundsParser     ( parseCodesWithMaybeNames )

data RoundRating = RoundRating { 
  roundNumber :: Int, 
  maxReached :: Double, 
  reachablePoints :: Double, 
  ownPoints :: Double
} deriving Show

type Points = [RoundRating]

score :: Points -> (Double, Double)
score ps = 
  foldr (\rating (o, r) -> ((ownPoints &&& reachablePoints) >>> ((+ o) *** (+ r))) rating) (0, 0) ps

mkSum :: Points -> String
mkSum = score >>> uncurry (\own reachable -> concat [prettyDouble own, "/", prettyDouble reachable])

prettyDouble :: Double -> String
prettyDouble d = short where
  int = round d :: Int
  short | (fromIntegral int :: Double) == d = show int
        | otherwise = show d

type SimplePoints = [Double]

type TeamRating = (TeamKey, Double)

data Team = Team { teamKey :: TeamKey, points :: Points }
  deriving Show

mkTeamName :: String -> TeamKey -> String
mkTeamName teamLbl key = name where
  fallback = (unwords [teamLbl, show (teamNumber key)])
  name = fromMaybe fallback (mfilter (not . null) (teamName key))

simplePoints :: Team -> SimplePoints
simplePoints = map ownPoints . points

data Round = Round { number :: Int, 
                     possible :: Double, 
                     teamRatings :: [TeamRating] 
                   }
  deriving Show

fromIndex :: [(Code, Maybe String)] -> Int -> Double -> [Double] -> Round
fromIndex teamCodes n maxPossible ps = Round n maxPossible ratings where
  ratings = zipWith3 (\i (c, ms) p -> (mkTeamKey i c ms, p)) [1 .. ] teamCodes ps

type Code = String

data TeamKey = TeamKey { teamNumber :: Int, code :: Code, teamName :: Maybe String }
  deriving Show

mkTeamKey :: Int -> Code -> Maybe String -> TeamKey
mkTeamKey i c = TeamKey i c . fmap htmlSafeString

mkSimpleTeamKey :: Int -> Code -> TeamKey
mkSimpleTeamKey i c = mkTeamKey i c Nothing

instance Eq TeamKey where
  (==) = (==) `on` teamNumber

instance Ord TeamKey where
  compare = compare `on` teamNumber

-- Computes the maximum number reached in a given round.
maxInRound :: Round -> Double
maxInRound = snd . maximumBy (comparing snd) . teamRatings

roundRating :: Round -> (Map TeamKey Points, [TeamKey])
roundRating rd = (fromList ratings, winners) where
  n = number rd
  reachable = possible rd
  maxAny = maxInRound rd
  gs = teamRatings rd
  winners = map fst (filter ((maxAny ==) . snd) gs)
  ratings = map (second (pure . RoundRating n maxAny reachable)) gs
 
mkTeams :: [Round] -> ([Team], [[TeamKey]])
mkTeams rs = (ts, tks) where 
  ratings = map roundRating rs
  ts = map (uncurry Team) . toList . unionsWith (++) . map fst $ ratings
  tks = map snd ratings

writePointPages :: String -> Labels -> [Team] -> [Color] -> IO ()
writePointPages prefix labels teams colors =
  mapM_ (\(team, color) -> writeFile (prefix ++ code (teamKey team) ++ ".html")
        (pointPage labels color team)) (zip teams colors)

writeGraphPage :: String -> Labels -> Int -> [Team] -> [[TeamKey]] -> [String] -> IO ()
writeGraphPage prefix labels rounds teams winners colors =
  writeFile (prefix ++ "index.html")
            (graphPage labels rounds teams winners colors)

cssPath :: String
cssPath = "<link rel='stylesheet' type='text/css' href='../style.css'/>"

pointPage :: Labels -> Color -> Team -> String
pointPage labels color team =
  pageHeader ++
    tagged "html" (
      encoding ++
      tagged "head" 
             (tagged "title" (concat [mainLabel labels, ": ", ownPageLabel labels]) ++ cssPath) ++
      tagged "body" (
        centerDiv (h1With coloured (concat [mkTeamName (teamLabel labels) (teamKey team), ": ", mkSum ps])) ++
        centerDiv (mkTable labels ps) ++
        centerDiv (mkButton (backToChartView labels))
      )
    )
  where coloured = "style=\"color:" ++ color ++ "\""
        ps = points team

mkTableLine :: RoundRating -> String
mkTableLine rating =   
  tableRow (concatMap tableCell [
    show (roundNumber rating), 
    prettyDouble (ownPoints rating), 
    prettyDouble (maxReached rating), 
    prettyDouble (reachablePoints rating)
    ])

tableHeader :: Labels -> String
tableHeader labels = 
  tableRow (concatMap headerCell [
    roundLabel labels, 
    ownPointsLabel labels, 
    maxReachedLabel labels, 
    maxReachableLabel labels
    ]
  )

mkTable :: Labels -> Points -> String
mkTable labels ps = 
  concat [
  openTable, 
  tableHeader labels, 
  concatMap mkTableLine ps,
  closeTable] where
    (openTable, closeTable) = tag "table"

type Color = String

defaultColors :: [Color]
defaultColors = cycle [ 
  "rgb(255, 99, 132)"
  , "rgb(255, 159, 64)"
  , "rgb(255, 205, 86)"
  , "rgb(75, 192, 192)"
  , "rgb(54, 162, 235)"
  , "rgb(153, 102, 255)"
  , "rgb(201, 203, 207)"
  , "rgb(88, 68, 146)"
  , "rgb(142, 64, 255)"
  , "rgb(128, 0, 128)"
  , "rgb(0, 255, 255)"
  , "rgb(120, 0, 200)"
  , "rgb(255, 20, 147)"
  , "rgb(119, 136, 153)"
  , "rgb(128, 0, 0)"
  , "rgb(135, 206, 250)"
  , "rgb(0, 255, 127)"
  , "rgb(189, 183, 107)"
  , "rgb(220, 20, 60)"
  , "rgb(216, 191, 216)"
  ]

toDatasetWith :: (SimplePoints -> SimplePoints) -> String -> String -> Team -> Color -> String
toDatasetWith pointMaker rd team g c = unlines [
    "{",
    "  label: '" ++ mkTeamName team (teamKey g) ++ "',",
    "  borderColor: " ++ show c ++ ",",
    "  backgroundColor: " ++ show c ++ ",",
    "  fill: false,",
    "  lineTension: 0,",
    "  data: [" ++ intercalate ","
                               (zipWith (\x y -> "{ x: '" ++ x ++ "' , y: '" ++ show y ++ "'}")
                                        (roundListInf rd)
                                        (pointMaker (simplePoints g))) ++ "]",
    "}"
  ]

toCumulativeDataset :: String -> String -> Team -> Color -> String
toCumulativeDataset = toDatasetWith (tail . scanl (+) 0)

toIndividualDataset :: String -> String -> Team -> Color -> String
toIndividualDataset = toDatasetWith id

roundList :: String -> Int -> String
roundList rn n = intercalate "," (map enclose (take n (roundListInf rn))) where
  enclose :: String -> String
  enclose t = concat ["'", t, "'"]

roundListInf :: String -> [String]
roundListInf rn = 
  zipWith (\r i -> concat [r, " ", show i]) (repeat rn) [(1 :: Int) ..]

addCanvas :: String -> String
addCanvas canvasLabel = div (taggedWith (concat ["id='", canvasLabel, "'"]) "canvas" "")

barChartLabel :: String
barChartLabel = "barChart"

lineChartLabel :: String
lineChartLabel = "lineChart"

perRoundChartLabel :: String
perRoundChartLabel = "perRoundChart"

data ChartType = Bar | Line

chartTypeToConstructor :: ChartType -> String
chartTypeToConstructor ct = case ct of
  Bar -> "Chart"
  Line -> "Chart.Line"

mkChartEntry :: ChartType -> String -> String -> String -> String
mkChartEntry ct canvasLabel chartTitle chartData = unlines [ 
    "  var " ++ context ++ " = document.getElementById('" ++ canvasLabel ++ "').getContext('2d');",
    "  window.myLine = new " ++ chartTypeToConstructor ct ++ "(" ++ context ++ ", {",
    "      type: 'bar', ",
    "      data: " ++ chartData ++ ",",
    "      options: {",
    "        responsive: true,",
    "        lineTension: 0,",
    "        hoverMode: 'index',",
    "        stacked: false,", 
    "        title: {",
    "          display: true,",
    "          text: '" ++ chartTitle ++ "'",
    "        },",
    "        scales: {",
    "          yAxes: [",
    "            {",
    "              type: 'linear',",
    "              display: true,",
    "              position: 'left',",
    "              id: 'y-axis-1',",
    "              ticks: {",
    "                beginAtZero: true",
    "              }",
    "            }",
    "          ]",
    "        }",
    "      }",
    "    }",
    "  );"
  ]
  where context = canvasLabel ++ "Context"

mkChartsWith :: Labels -> Int -> [Team] -> [Color] -> String
mkChartsWith labels rounds teams colors = 
  taggedV "script"
          (unlines [
            "var " ++ cumulativeData ++ " = {",
            "   labels: [" ++ lbls ++ "],",
            "   datasets: [" ++ mkDataSet toCumulativeDataset ++ "]",
            "};",
            "var " ++ perRoundData ++ " = {",
            "    labels: [" ++ lbls ++ "],",
            "    datasets: [" ++ mkDataSet toIndividualDataset ++ "]",
            "};",
            "",
            "window.onload = function() {",
            mkChartEntry Bar barChartLabel (cumulativeLabel labels) cumulativeData,
            mkChartEntry Line lineChartLabel (progressionLabel labels) cumulativeData,
            mkChartEntry Bar perRoundChartLabel (individualRoundsLabel labels) perRoundData,
            "};"
            ]
          )
  where lbls = roundList (roundLabel labels) rounds
        mkDataSet f = intercalate "," (zipWith (f (roundLabel labels) (teamLabel labels))
                                               teams 
                                               colors)
        cumulativeData = "cumulativeData"
        perRoundData = "perRoundData"

graphPage :: Labels -> Int -> [Team] -> [[TeamKey]] -> [Color] -> String
graphPage labels rounds teams winners colors = unlines [
  taggedV "html"
          (unlines [
             encoding,
             taggedV "head"
                     (unlines [
                        tagged "title" (mainLabel labels),
                        taggedWith "src='https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.7.3/Chart.bundle.min.js'"
                                   "script"
                                   "",
                        cssPath
                      ]
                     ),
             taggedV "body"
                     (unlines [
                        taggedWith "id = 'mainTitle'"
                                   "div"
                                   (mainLabel labels),
                        taggedWith "id = 'top3'"
                                   "div"
                                   (unlines[
                                             tagged "label" (placementLabel labels),
                                             mkTopDownList (teamLabel labels)
                                                           (placeLabel labels)
                                                           (pointsLabel labels)
                                                           teams
                                            ]
                                   ),
                        taggedWith "id = 'winners'"
                                   "div"
                                   (unlines [
                                      tagged "label" (roundWinnerLabel labels),
                                      mkWinnerList (roundLabel labels) (teamLabel labels) winners
                                      ]
                                    ),
                        addCanvas barChartLabel,
                        addCanvas perRoundChartLabel,
                        addCanvas lineChartLabel,
                        mkChartsWith labels rounds teams colors,
                        taggedWith "id = 'copyright'"
                                   "div"
                                   (unwords [
                                      "Powered by",
                                      taggedWith "href='https://www.chartjs.org'" "a" "Chart.js" 
                                      ]
                                    ),
                        taggedWith "id = 'allQuizzes'"
                                   "div"
                                   (mkButtonTo "../index.html" (viewPrevious labels))
                      ]
                     ) 
            ]
          )
  ]

findTopDownOrder :: [Team] -> [(Double, [Team])]
findTopDownOrder = map (\gds -> (snd (head gds), reverse (map fst gds)))
             . groupBy ((==) `on` snd)
             . reverse
             . sortBy (comparing snd) 
             . map (\g -> (g, sum (simplePoints g)))

mkTopDownList :: String -> String -> String -> [Team] -> String
mkTopDownList teamLbl placeLbl pointsLbl gs = unlines (map (tagged "div") rated) where
  rated = zipWith (\i (ps, grs) -> unwords [unwords [placeLbl, show i], 
                                                     "(" ++ unwords [prettyDouble ps, pointsLbl] ++ ")", 
                                                     ":", 
                                                     teams grs])
                  [(1 :: Int) ..] 
                  tops
  teams =  intercalate ", " . map (\g -> mkTeamName teamLbl (teamKey g))
  tops = findTopDownOrder gs

mkWinnerList :: String -> String -> [[TeamKey]] -> String
mkWinnerList roundLbl teamLbl =
  unlines . map (\(i, ws) -> tagged "div" 
                     (concat [unwords [roundLbl, show i], 
                              ": ", 
                              intercalate ", " (map (mkTeamName teamLbl) ws)]))
          . zip [(1 :: Int) ..]

readLabels :: String -> IO Labels
readLabels labelsPath = fmap read (readFile labelsPath) `catch` handle where
  handle :: IOException -> IO Labels
  handle _ = putStrLn (labelsPath ++ " not found - using default labels.") >> return defaultLabels

readPoints :: String -> (Double, [Double])
readPoints [] = (0, [])
readPoints text = (total, ps) where
  (total : _ : ps) = map read (words text)

readColors :: String -> IO [Color]
readColors colorsPath = fmap lines (readFile colorsPath) `catch` handle where
  handle :: IOException -> IO [Color]
  handle _ = putStrLn (colorsPath ++ " not found - using default colors.") >> return defaultColors

parseCodesWithNamesAndRounds :: String -> ([(Code, Maybe String)], [Round])
parseCodesWithNamesAndRounds [] = ([], [])
parseCodesWithNamesAndRounds text = (codesAndNames, rounds) where
  (l : ls) = lines text
  codesAndNames = parseCodesWithMaybeNames l
  pts = map readPoints ls
  indexedPoints = zip [1 ..] pts
  rounds = map (\(i, (total, ps)) -> fromIndex codesAndNames i total ps) indexedPoints

readCodesAndRounds :: String -> IO ([(Code, Maybe String)], [Round])
readCodesAndRounds roundsPath =
  fmap parseCodesWithNamesAndRounds (readFile roundsPath) `catch` handle where
    handle :: IOException -> IO ([(Code, Maybe String)], [Round]) 
    handle e = putStrLn (show e) >> 
               putStrLn "Unexpected format or missing file. No output generated." >> 
               return ([], [])

splitOnSetter :: String -> (String, String)
splitOnSetter str = (key, drop 1 preValue) where
  (key, preValue) = span ((/=) '=') str

-- Creates teams with proper keys, but all points set to empty.
mkEmptyTeams :: [Code] -> [Team]
mkEmptyTeams = map (\(i, c) -> Team (mkSimpleTeamKey i c) []) . zip [1 ..]

createWith :: [(String, String)] -> IO ()
createWith associations = do
    unsafeLabels <- readLabels labelsPath
    (codesAndNames, rounds) <- readCodesAndRounds roundsPath
    colors <- readColors colorsPath
    let (teamsCandidates, winners) = mkTeams rounds
        safeLabels = mkHTMLSafe unsafeLabels
        -- If there are no rounds, we create teams that have not played any rounds yet.
        -- This facilitates the initial creation of the point pages.
        teams = if null teamsCandidates then mkEmptyTeams (map fst codesAndNames) 
                                        else teamsCandidates
        n = length rounds
    writePointPages prefix (labels safeLabels) teams colors
    writeGraphPage prefix unsafeLabels n teams winners colors
  where kvs = fromList associations
        labelsPath = fromMaybe "labels.txt" (lookup "labels" kvs)
        colorsPath = fromMaybe "colors.txt" (lookup "colors" kvs)
        roundsPath = fromMaybe "rounds.txt" (lookup "rounds" kvs)
        prefix     = fromMaybe "./"         (lookup "prefix" kvs)

main :: IO ()
main = getArgs >>= createWith . map splitOnSetter