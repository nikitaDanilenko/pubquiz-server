module Pages.PointComputation where

import           Control.Arrow  (second, (&&&), (***), (>>>))
import           Control.Monad  (mfilter)
import           Data.Function  (on)
import           Data.List      (groupBy, intercalate, maximumBy, sortBy)
import           Data.Map       (Map, fromList, toList, unionsWith)
import           Data.Maybe     (fromMaybe)
import           Data.Ord       (comparing)
import           Pages.HtmlUtil (htmlSafeString)

data RoundRating =
  RoundRating
    { roundNumber     :: Int
    , maxReached      :: Double
    , reachablePoints :: Double
    , ownPoints       :: Double
    }
  deriving (Show)

type Points = [RoundRating]

score :: Points -> (Double, Double)
score = foldr (\rating (o, r) -> ((ownPoints &&& reachablePoints) >>> ((+ o) *** (+ r))) rating) (0, 0)

mkSum :: Points -> String
mkSum = score >>> uncurry (\own reachable -> concat [prettyDouble own, "/", prettyDouble reachable])

prettyDouble :: Double -> String
prettyDouble d = short
  where
    int = round d :: Int
    short
      | (fromIntegral int :: Double) == d = show int
      | otherwise = show d

type SimplePoints = [Double]

type TeamRating = (TeamKey, Double)

data Team =
  Team
    { teamKey :: TeamKey
    , points  :: Points
    }
  deriving (Show)

data HTMLSafety
  = Safe
  | Unsafe

isHTMLSafe :: HTMLSafety -> Bool
isHTMLSafe Safe = True
isHTMLSafe _    = False

mkTeamName :: HTMLSafety -> String -> TeamKey -> String
mkTeamName sf teamLbl key = name
  where
    fallback = unwords [teamLbl, show (teamNumber key)]
    name =
      fromMaybe
        fallback
        (mfilter
           (not . null)
           ((if isHTMLSafe sf
               then htmlSafeTeamName
               else teamName)
              key))

simplePoints :: Team -> SimplePoints
simplePoints = map ownPoints . points

data Round =
  Round
    { number      :: Int
    , possible    :: Double
    , teamRatings :: [TeamRating]
    }
  deriving (Show)

fromIndex :: [(Code, Maybe String)] -> Int -> Double -> [Double] -> Round
fromIndex teamCodes n maxPossible ps = Round n maxPossible ratings
  where
    ratings = zipWith3 (\i (c, ms) p -> (mkTeamKey i c ms, p)) [1 ..] teamCodes ps

type Code = String

data TeamKey =
  TeamKey
    { teamNumber :: Int
    , code       :: Code
    , teamName   :: Maybe String
    }
  deriving (Show)

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
roundRating rd = (fromList ratings, winners)
  where
    n = number rd
    reachable = possible rd
    maxAny = maxInRound rd
    gs = teamRatings rd
    winners = map fst (filter ((maxAny ==) . snd) gs)
    ratings = map (second (pure . RoundRating n maxAny reachable)) gs

mkTeams :: [Round] -> ([Team], [[TeamKey]])
mkTeams rs = (ts, tks)
  where
    ratings = map roundRating rs
    ts = map (uncurry Team) . toList . unionsWith (++) . map fst $ ratings
    tks = map snd ratings
