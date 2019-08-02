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
                                cumulativeLabel, progressionLabel, individualRoundsLabel, unwrapped,
                                placementLabel, roundWinnerLabel, mkHTMLSafe, SafeLabels )
import Pages.Colours          ( mkHTMLColours )
import Pages.HtmlUtil         ( centerDivV, h1With, tableCell, tableRow, headerCell, tag, tagged,
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
score = 
  foldr (\rating (o, r) -> ((ownPoints &&& reachablePoints) >>> ((+ o) *** (+ r))) rating) (0, 0)

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

data HTMLSafety = Safe | Unsafe

isHTMLSafe :: HTMLSafety -> Bool
isHTMLSafe Safe = True
isHTMLSafe _    = False

mkTeamName :: HTMLSafety -> String -> TeamKey -> String
mkTeamName sf teamLbl key = name where
  fallback = unwords [teamLbl, show (teamNumber key)]
  name = fromMaybe fallback 
                  (mfilter (not . null) 
                           ((if isHTMLSafe sf then htmlSafeTeamName else teamName) key))

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

htmlSafeTeamName :: TeamKey -> Maybe String
htmlSafeTeamName = fmap htmlSafeString . teamName

mkTeamKey :: Int -> Code -> Maybe String -> TeamKey
mkTeamKey = TeamKey

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

writePointPages :: String -> SafeLabels -> [Team] -> [Color] -> IO ()
writePointPages prefix labels teams colors =
  mapM_ (\(team, color) -> writeFile (prefix ++ code (teamKey team) ++ ".html")
        (pointPage labels color team)) (zip teams colors)

writeGraphPage :: String 
               -> SafeLabels 
               -> Labels 
               -> Int 
               -> [Team] 
               -> [[TeamKey]] 
               -> [String] 
               -> IO ()
writeGraphPage prefix safeLabels labels rounds teams winners colors =
  writeFile (prefix ++ "index.html")
            (graphPage safeLabels labels rounds teams winners colors)

cssPath :: String
cssPath = "<link rel='stylesheet' type='text/css' href='../style.css'/>"

pointPage :: SafeLabels -> Color -> Team -> String
pointPage safeLabels color team =
  pageHeader ++
    taggedV "html" (
      taggedV "head" 
              (intercalate "\n" [
                encoding, 
                taggedV "title" (concat [mainLabel labels, ": ", ownPageLabel labels]) ++ cssPath]) ++
      taggedV "body" (
        intercalate "\n" [
          centerDivV (h1With coloured 
                           (concat [mkTeamName Safe (teamLabel labels) (teamKey team), ": ", mkSum ps])),
          centerDivV (mkTable labels ps),
          centerDivV (mkButton (backToChartView labels))
        ]
      )
    )
  where coloured = "style=\"color:" ++ color ++ "\""
        ps = points team
        labels = unwrapped safeLabels

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
  intercalate "\n" [
  openTable, 
  tableHeader labels, 
  intercalate "\n" (map mkTableLine ps),
  closeTable] where
    (openTable, closeTable) = tag "table"

type Color = String

toDatasetWith :: (SimplePoints -> SimplePoints) -> String -> String -> Team -> Color -> String
toDatasetWith pointMaker rd team g c = unlines [
    "{",
    "  label: '" ++ mkTeamName Unsafe team (teamKey g) ++ "',",
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
roundListInf rn = map (\i -> concat [rn, " ", show i]) [(1 :: Int) ..]

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

chartJSFontSize :: Int
chartJSFontSize = 30

mkChartEntry :: ChartType -> String -> String -> String -> String
mkChartEntry ct canvasLabel chartTitle chartData = unlines [ 
    "  var " ++ context ++ " = document.getElementById('" ++ canvasLabel ++ "').getContext('2d');",
    "  Chart.defaults.global.defaultFontSize = " ++ show chartJSFontSize ++ ";",
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

graphPage :: SafeLabels -> Labels -> Int -> [Team] -> [[TeamKey]] -> [Color] -> String
graphPage safeLbls labels rounds teams winners colors = unlines [
  pageHeader,
  taggedV "html"
          (unlines [
             taggedV "head"
                     (unlines [
                        encoding,
                        tagged "title" (mainLabel safeLabels),
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
                                   (mainLabel safeLabels),
                        taggedWith "id = 'top3'"
                                   "div"
                                   (unlines[
                                             tagged "label" (placementLabel safeLabels),
                                             mkTopDownList (teamLabel safeLabels)
                                                           (placeLabel safeLabels)
                                                           (pointsLabel safeLabels)
                                                           teams
                                            ]
                                   ),
                        taggedWith "id = 'winners'"
                                   "div"
                                   (unlines [
                                      tagged "label" (roundWinnerLabel safeLabels),
                                      mkWinnerList (roundLabel safeLabels) 
                                                   (teamLabel safeLabels) 
                                                   winners
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
                                   (mkButtonTo "../index.html" (viewPrevious safeLabels))
                      ]
                     ) 
            ]
          )
  ] where safeLabels = unwrapped safeLbls

findTopDownOrder :: [Team] -> [(Double, [Team])]
findTopDownOrder = map (\gds -> (snd (head gds), reverse (map fst gds)))
             . groupBy ((==) `on` snd)
             . sortBy (flip (comparing snd))
             . map (\g -> (g, sum (simplePoints g)))

mkTopDownList :: String -> String -> String -> [Team] -> String
mkTopDownList teamLbl placeLbl pointsLbl gs = unlines (map (tagged "div") rated) where
  rated = zipWith (\i (ps, grs) -> unwords [unwords [placeLbl, show i], 
                                                     "(" ++ unwords [prettyDouble ps, pointsLbl] ++ ")", 
                                                     ":", 
                                                     teams grs])
                  [(1 :: Int) ..] 
                  tops
  teams =  intercalate ", " . map (mkTeamName Safe teamLbl . teamKey)
  tops = findTopDownOrder gs

mkWinnerList :: String -> String -> [[TeamKey]] -> String
mkWinnerList roundLbl teamLbl =
  unlines . map (\(i, ws) -> tagged "div" 
                     (concat [unwords [roundLbl, show i], 
                              ": ", 
                              intercalate ", " (map (mkTeamName Safe teamLbl) ws)]))
          . zip [(1 :: Int) ..]

readLabels :: String -> IO Labels
readLabels labelsPath = fmap read (readFile labelsPath) `catch` handle where
  handle :: IOException -> IO Labels
  handle _ = putStrLn (labelsPath ++ " not found - using default labels.") >> return defaultLabels

readPoints :: String -> (Double, [Double])
readPoints [] = (0, [])
readPoints text = (total, ps) where
  (total : _ : ps) = map read (words text)

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
    handle e = print e >> 
               putStrLn "Unexpected format or missing file. No output generated." >> 
               return ([], [])

splitOnSetter :: String -> (String, String)
splitOnSetter str = (key, drop 1 preValue) where
  (key, preValue) = span ('=' /=) str

-- Creates teams with proper keys, but all points set to empty.
mkEmptyTeams :: [Code] -> [Team]
mkEmptyTeams = map (\(i, c) -> Team (mkSimpleTeamKey i c) []) . zip [1 ..]

createWith :: [(String, String)] -> IO ()
createWith associations = do
    unsafeLabels <- readLabels labelsPath
    (codesAndNames, rounds) <- readCodesAndRounds roundsPath
    let (teamsCandidates, winners) = mkTeams rounds
        safeLabels = mkHTMLSafe unsafeLabels
        -- If there are no rounds, we create teams that have not played any rounds yet.
        -- This facilitates the initial creation of the point pages.
        teams = if null teamsCandidates then mkEmptyTeams (map fst codesAndNames) 
                                        else teamsCandidates
        colours = mkHTMLColours (length teams)
        n = length rounds
    writePointPages prefix safeLabels teams colours
    writeGraphPage prefix safeLabels unsafeLabels n teams winners colours
  where kvs = fromList associations
        labelsPath = fromMaybe "labels.txt" (lookup "labels" kvs)
        roundsPath = fromMaybe "rounds.txt" (lookup "rounds" kvs)
        prefix     = fromMaybe "./"         (lookup "prefix" kvs)

main :: IO ()
main = getArgs >>= createWith . map splitOnSetter