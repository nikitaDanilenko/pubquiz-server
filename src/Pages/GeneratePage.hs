module Pages.GeneratePage ( createWith ) where

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
                                groupLabel, defaultLabels, unEscape, viewPrevious,
                                cumulativeLabel, progressionLabel, individualRoundsLabel,
                                htmlSafeString )
import Pages.HtmlUtil         ( centerDiv, h1With, tableCell, tableRow, headerCell, tag, tagged,
                                mkButton, mkButtonTo, pageHeader, div, taggedV, taggedWith )
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

type GroupRating = (GroupKey, Double)

data Group = Group { groupKey :: GroupKey, points :: Points }
  deriving Show

mkGroupName :: String -> Group -> String
mkGroupName groupLbl group = name where
  fallback = (unwords [htmlSafeString groupLbl, show (groupNumber (groupKey group))]) 
  name = fromMaybe fallback (mfilter (not . null) (teamName (groupKey group)))

simplePoints :: Group -> SimplePoints
simplePoints = map ownPoints . points

data Round = Round { name :: String, number :: Int, possible :: Double, groupRatings :: [GroupRating] }
  deriving Show

fromIndex :: [(Code, Maybe String)] -> String -> Int -> Double -> [Double] -> Round
fromIndex groupCodes nm n maxPossible ps = Round nm n maxPossible ratings where
  ratings = zipWith3 (\i (c, ms) p -> (mkGroupKey i c ms, p)) [1 .. ] groupCodes ps

type Code = String

data GroupKey = GroupKey { groupNumber :: Int, code :: Code, teamName :: Maybe String }
  deriving Show

mkGroupKey :: Int -> Code -> Maybe String -> GroupKey
mkGroupKey i c = GroupKey i c . fmap htmlSafeString

mkSimpleGroupKey :: Int -> Code -> GroupKey
mkSimpleGroupKey i c = mkGroupKey i c Nothing

instance Eq GroupKey where
  (==) = (==) `on` groupNumber

instance Ord GroupKey where
  compare = compare `on` groupNumber

-- Computes the maximum number reached in a given round.
maxInRound :: Round -> Double
maxInRound = snd . maximumBy (comparing snd) . groupRatings

roundRating :: Round -> Map GroupKey Points
roundRating rd = fromList ratings where
  n = number rd
  reachable = possible rd
  maxAny = maxInRound rd
  gs = groupRatings rd
  ratings = map (second (pure . RoundRating n maxAny reachable)) gs
 
mkGroups :: [Round] -> [Group]
mkGroups = map (uncurry Group) . toList . unionsWith (++) . map roundRating

writePointPages :: String -> Labels -> [Group] -> [Color] -> IO ()
writePointPages prefix labels groups colors =
  mapM_ (\(group, color) -> writeFile (prefix ++ code (groupKey group) ++ ".html")
        (pointPage labels color (points group))) (zip groups colors)

writeGraphPage :: String -> Labels -> Int -> [Group] -> [String] -> IO ()
writeGraphPage prefix labels rounds groups colors =
  writeFile (prefix ++ "index.html")
            (graphPage labels rounds groups colors)

cssPath :: String
cssPath = "<link rel='stylesheet' type='text/css' href='../style.css'/>"

pointPage :: Labels -> Color -> Points -> String
pointPage labels color ps =
  pageHeader ++
    tagged "html" ( 
      tagged "head" 
             (tagged "title" (concat [mainLabel labels, ": ", ownPageLabel labels]) ++ cssPath) ++
      tagged "body" (
        centerDiv (h1With coloured (mkSum ps)) ++
        centerDiv (mkTable labels ps) ++
        centerDiv (mkButton (backToChartView labels))
      )
    )
  where coloured = "style=\"color:" ++ color ++ "\""

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

toDatasetWith :: (SimplePoints -> SimplePoints) -> String -> String -> Group -> Color -> String
toDatasetWith pointMaker rd group g c = unlines [
    "{",
    "  label: '" ++ mkGroupName group g ++ "',",
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

toCumulativeDataset :: String -> String -> Group -> Color -> String
toCumulativeDataset = toDatasetWith (tail . scanl (+) 0)

toIndividualDataset :: String -> String -> Group -> Color -> String
toIndividualDataset = toDatasetWith id

roundList :: String -> Int -> String
roundList roundName n = intercalate "," (map enclose (take n (roundListInf roundName))) where
  enclose :: String -> String
  enclose t = concat ["'", t, "'"]

roundListInf :: String -> [String]
roundListInf roundName = 
  zipWith (\r i -> concat [r, " ", show i]) (repeat roundName) [(1 :: Int) ..]

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
    "          text: '" ++ unEscape chartTitle ++ "'",
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

mkChartsWith :: Labels -> Int -> [Group] -> [Color] -> String
mkChartsWith labels rounds groups colors = 
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
            mkChartEntry Bar barChartLabel (unEscape (cumulativeLabel labels)) cumulativeData,
            mkChartEntry Line lineChartLabel (unEscape (progressionLabel labels)) cumulativeData,
            mkChartEntry Bar perRoundChartLabel (unEscape (individualRoundsLabel labels)) perRoundData,
            "};"
            ]
          )
  where lbls = roundList (roundLabel labels) rounds
        mkDataSet f = intercalate "," (zipWith (f (roundLabel labels) (groupLabel labels))
                                               groups 
                                               colors)
        cumulativeData = "cumulativeData"
        perRoundData = "perRoundData"

graphPage :: Labels -> Int -> [Group] -> [Color] -> String
graphPage labels rounds groups colors = unlines [
  taggedV "html"
          (unlines [
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
                                   (mkTopThree (groupLabel labels) groups),
                        addCanvas barChartLabel,
                        addCanvas perRoundChartLabel,
                        addCanvas lineChartLabel,
                        mkChartsWith labels rounds groups colors,
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

findTopThree :: [Group] -> [(Double, [Group])]
findTopThree = take 3
             . map (\gds -> (snd (head gds), reverse (map fst gds)))
             . groupBy ((==) `on` snd)
             . reverse
             . sortBy (comparing snd) 
             . map (\g -> (g, sum (simplePoints g)))

mkTopThree :: String -> [Group] -> String
mkTopThree groupLbl gs = unlines (map (tagged "div") rated) where
  rated = zipWith (\i (ps, grs) -> unwords [show i, "(" ++ prettyDouble ps ++ ")", ":", groups grs])
                  [1 ..] 
                  tops
  groups =  intercalate ", " . map (\g -> mkGroupName groupLbl g)
  tops = findTopThree gs

readLabels :: String -> IO Labels
readLabels labelsPath = fmap (read :: String -> Labels) (readFile labelsPath) `catch` handle where
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

parseCodesWithNamesAndRounds :: String -> String -> ([(Code, Maybe String)], [Round])
parseCodesWithNamesAndRounds _ [] = ([], [])
parseCodesWithNamesAndRounds roundName text = (codesAndNames, rounds) where
  (l : ls) = lines text
  codesAndNames = parseCodesWithMaybeNames l
  pts = map readPoints ls
  indexedPoints = zip [1 ..] pts
  rounds = map (\(i, (total, ps)) -> fromIndex codesAndNames roundName i total ps) indexedPoints

readCodesAndRounds :: String -> String -> IO ([(Code, Maybe String)], [Round])
readCodesAndRounds roundsPath rdLabel =
  fmap (parseCodesWithNamesAndRounds rdLabel) (readFile roundsPath) `catch` handle where
    handle :: IOException -> IO ([(Code, Maybe String)], [Round]) 
    handle e = putStrLn (show e) >> 
               putStrLn "Unexpected format or missing file. No output generated." >> 
               return ([], [])

splitOnSetter :: String -> (String, String)
splitOnSetter str = (key, drop 1 preValue) where
  (key, preValue) = span ((/=) '=') str

-- Creates groups with proper keys, but all points set to empty.
mkEmptyGroups :: [Code] -> [Group]
mkEmptyGroups = map (\(i, c) -> Group (mkSimpleGroupKey i c) []) . zip [1 ..]

createWith :: [(String, String)] -> IO ()
createWith associations = do
    labels <- readLabels labelsPath
    (codesAndNames, rounds) <- readCodesAndRounds roundsPath (roundLabel labels)
    writeFile "debug.txt" (show codesAndNames)
    colors <- readColors colorsPath
    let groupsCandidates = mkGroups rounds
        -- If there are no rounds, we create groups that have not played any rounds yet.
        -- This facilitates the initial creation of the point pages.
        groups = if null groupsCandidates then mkEmptyGroups (map fst codesAndNames) 
                                          else groupsCandidates
        n = length rounds
    writePointPages prefix labels groups colors
    writeGraphPage prefix labels n groups colors
  where kvs = fromList associations
        labelsPath = fromMaybe "labels.txt" (lookup "labels" kvs)
        colorsPath = fromMaybe "colors.txt" (lookup "colors" kvs)
        roundsPath = fromMaybe "rounds.txt" (lookup "rounds" kvs)
        prefix     = fromMaybe "./"         (lookup "prefix" kvs)

main :: IO ()
main = getArgs >>= createWith . map splitOnSetter