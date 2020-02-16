module Db.DbRatings where

import           Control.Arrow ((&&&))
import           Data.Function (on)
import           Data.List     (groupBy, sortOn)
import           Data.Map      (fromList, intersectionWith, toList)
import           Db.Connection (DbRoundReachable (dbRoundReachablePoints, dbRoundReachableRoundNumber),
                                DbRoundReached (dbRoundReachedPoints, dbRoundReachedRoundNumber, dbRoundReachedTeamNumber))
import           Db.DbTypes    (RoundNumber (RoundNumber),
                                TeamNumber (TeamNumber))

data RoundRating =
  RoundRating
    { reachableInRound :: Double
    , points           :: [(TeamNumber, Double)]
    }

newtype Ratings =
  Ratings
    { roundRatings :: [(RoundNumber, RoundRating)]
    }

-- | Merges a list of reachable points and a list of reached points in their respective database representations
--   into a unified rating element.
--   Rounds that are mentioned in only one of the two lists are ignored.
--   This function ignores the quizzes parameters of both parameters.
ratingsFromDb :: [DbRoundReachable] -> [DbRoundReached] -> Ratings
ratingsFromDb reachables reacheds = Ratings (toList (intersectionWith RoundRating reachableMap reachedMap))
  where
    reachableMap = fromList (map ((RoundNumber . dbRoundReachableRoundNumber) &&& dbRoundReachablePoints) reachables)
    reachedMap =
      fromList
        (map
           ((RoundNumber . dbRoundReachedRoundNumber . head) &&&
            map ((TeamNumber . dbRoundReachedTeamNumber) &&& dbRoundReachedPoints))
           (groupBy ((==) `on` dbRoundReachedRoundNumber) (sortOn dbRoundReachedRoundNumber reacheds)))
    mkRoundNumber = undefined
    mkRoundRating = undefined
