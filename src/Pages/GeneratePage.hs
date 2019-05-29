module Pages.GeneratePage where

import Control.Arrow          ( second, (&&&), (***), (>>>) )
import Control.Exception      ( catch )
import Control.Exception.Base ( IOException )
import Data.Function          ( on )
import Data.List              ( intercalate, maximumBy )
import Data.Map               ( Map, fromList, unionsWith, toList, lookup )
import Data.Maybe             ( fromMaybe )
import Data.Ord               ( comparing )
import System.Environment     ( getArgs )

import Prelude hiding         ( lookup )

import Labels                 ( Labels, mainLabel, ownPageLabel, backToChartView, roundLabel,
                                ownPageLabel, ownPointsLabel, maxReachedLabel, maxReachableLabel,
                                groupLabel, defaultLabels, unEscape )

data RoundRating = RoundRating { 
  roundNumber :: Int, 
  maxReached :: Double, 
  reachablePoints :: Double, 
  ownPoints :: Double
}

type Points = [RoundRating]

score :: Points -> (Double, Double)
score points = 
  foldr (\rating (o, r) -> ((ownPoints &&& reachablePoints) >>> ((+ o) *** (+ r))) rating) (0, 0) points

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

simplePoints :: Group -> SimplePoints
simplePoints = map ownPoints . points

data Round = Round { name :: String, number :: Int, possible :: Double, groupRatings :: [GroupRating] }

fromIndex :: [Code] -> String -> Int -> Double -> [Double] -> Round
fromIndex groupCodes name number possible points = Round name number possible ratings where
  ratings = zipWith3 (\i c p -> (GroupKey i c, p)) [1 .. ] groupCodes points

type Code = String

data GroupKey = GroupKey { groupNumber :: Int, code :: Code }

instance Eq GroupKey where
  (==) = (==) `on` groupNumber

instance Ord GroupKey where
  compare = compare `on` groupNumber

-- Computes the maximum number reached in a given round.
maxInRound :: Round -> Double
maxInRound = snd . maximumBy (comparing snd) . groupRatings

roundRating :: Round -> Map GroupKey Points
roundRating round = fromList ratings where
  n = number round
  reachable = possible round
  maxReached = maxInRound round
  gs = groupRatings round
  ratings = map (second (pure . RoundRating n maxReached reachable)) gs
 
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

centerDiv :: String -> String
centerDiv = tagged "div" . tagged "center"

cssPath :: String
cssPath = "<link rel='stylesheet' type='text/css' href='style.css'/>"

pointPage :: Labels -> Color -> Points -> String
pointPage labels color points =
  "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">\n" ++
  (tagged "html" . tagged "head") 
    (tagged "title" (concat [mainLabel labels, ": ", ownPageLabel labels]) ++ cssPath) ++
  centerDiv (h1With coloured (mkSum points)) ++
  centerDiv (mkTable labels points) ++
  centerDiv (mkButton (backToChartView labels)) ++
  "</body></html>" where
    coloured = "style=\"color:" ++ color ++ "\""

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
mkTable labels points = 
  concat [
  openTable, 
  tableHeader labels, 
  concatMap mkTableLine points,
  closeTable] where
    (openTable, closeTable) = tag "table"

tag :: String -> (String, String)
tag t = (concat ["<", t, ">"], concat ["</", t, ">"])

tagged :: String -> String -> String
tagged t text = concat [open, text, close] where
  (open, close) = tag t

mkButton :: String -> String
mkButton text = concat ["<a href=\"./index.html\" class=\"button\">", text, "</a>"]

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

toDataset :: String -> String -> Group -> Color -> String
toDataset round group g c =
  "{" ++ "label: '" ++ unwords [group, show (groupNumber (groupKey g))] ++ "'" ++
  "," ++ "borderColor: " ++ show c ++
  "," ++ "backgroundColor: " ++ show c ++
  "," ++ "fill: " ++ "false" ++
  "," ++ "data: [" ++ intercalate ","
                            (zipWith (\x y -> "{ x: '" ++ x ++ "' , y: '" ++ show y ++ "'}")
                                     (roundListInf round)
                                     (tail (scanl (+) 0 (simplePoints g)))) ++
  "]}"

roundList :: String -> Int -> String
roundList roundName n = intercalate "," (map enclose (take n (roundListInf roundName))) where
  enclose :: String -> String
  enclose t = concat ["'", t, "'"]

roundListInf :: String -> [String]
roundListInf roundName = 
  zipWith (\r i -> concat [r, " ", show i]) (repeat roundName) [1 ..]

graphPage :: Labels -> Int -> [Group] -> [Color] -> String
graphPage labels rounds groups colors =
  "<html>\
  \<head>"
  ++
  tagged "title" (mainLabel labels)
  ++
  "<script src='https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.7.3/Chart.bundle.min.js'></script>"
  ++
  cssPath
  ++
  "</head>\
  \<body>\
  \<div>\
  \<canvas id='canvas'></canvas>\
  \</div>\
  \<script>\
  \var lineChartData = {\
  \    labels: ["
  ++ 
  roundList (roundLabel labels) rounds
  ++ 
  "], \ 
  \    datasets:["
  ++
  intercalate "," (zipWith (toDataset (roundLabel labels) (groupLabel labels)) groups colors)
  ++
  "]};\
  \window.onload = function() {\
  \  var ctx = document.getElementById('canvas').getContext('2d');\
  \  window.myLine = new Chart(ctx, {\
  \   type: 'bar', \
  \   data: lineChartData,\
  \   options: {\
  \   responsive: true,\
  \   hoverMode: 'index',\
  \   stacked: false,\
  \   title: {\
  \     display: true,\
  \     text: '"
  ++ 
  unEscape (mainLabel labels)
  ++ 
  "\'"
  ++
  " },\
  \ scales: {\
  \ yAxes: [\
  \   { type: 'linear', display: true, position: 'left', id: 'y-axis-1',\
  \     ticks: { beginAtZero : true } } ]\
  \ }}});};</script>\
  \ <div id = 'copyright'>Powered by <a href='https://www.chartjs.org'>Chart.js</a></div>\
  \</body></html>"

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

parseCodesAndRounds :: String -> String -> ([String], [Round])
parseCodesAndRounds _ [] = ([], [])
parseCodesAndRounds roundName text = (codes, rounds) where
  (l : ls) = lines text
  codes = words l
  points = map readPoints ls
  indexedPoints = zip [1 ..] points
  rounds = map (\(i, (total, ps)) -> fromIndex codes roundName i total ps) indexedPoints

readCodesAndRounds :: String -> String -> IO ([String], [Round])
readCodesAndRounds roundsPath roundLabel =
  fmap (parseCodesAndRounds roundLabel) (readFile roundsPath) `catch` handle where
    handle :: IOException -> IO ([String], [Round]) 
    handle e = putStrLn (show e) >> 
               putStrLn "Unexpected format or missing file. No output generated." >> 
               return ([], [])

splitOnSetter :: String -> (String, String)
splitOnSetter str = (key, drop 1 preValue) where
  (key, preValue) = span ((/=) '=') str

createWith :: [(String, String)] -> IO ()
createWith associations = do
    labels <- readLabels labelsPath
    (codes, rounds) <- readCodesAndRounds roundsPath (roundLabel labels)
    colors <- readColors colorsPath
    let groups = mkGroups rounds
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