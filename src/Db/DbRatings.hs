{-# LANGUAGE DeriveGeneric     #-}

module Db.DbRatings where

import           Control.Arrow ((&&&))
import           Data.Aeson    (ToJSON (toJSON), object, (.=))
import           Data.Function (on)
import           Data.List     (groupBy, sortOn)
import           Data.Map      (fromList, intersectionWith, toList)
import           Data.Text     (pack)
import           Db.Connection (DbRoundReachable (dbRoundReachablePoints, dbRoundReachableRoundNumber),
                                DbRoundReached (dbRoundReachedPoints, dbRoundReachedRoundNumber, dbRoundReachedTeamNumber))
import           Db.DbTypes    (RoundNumber (RoundNumber),
                                TeamNumber (TeamNumber))
import           GHC.Generics

data TeamRating = TeamRating {
  teamNumber :: TeamNumber,
  rating :: Double
} deriving Generic

data RoundRating =
  RoundRating
    { reachableInRound :: Double
    , points           :: [TeamRating]
    } deriving Generic

instance ToJSON TeamRating
instance ToJSON RoundRating

newtype Ratings =
  Ratings
    { roundRatings :: [(RoundNumber, RoundRating)]
    } deriving Generic

instance ToJSON Ratings

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
            map (uncurry TeamRating . ((TeamNumber . dbRoundReachedTeamNumber) &&& dbRoundReachedPoints)))
           (groupBy ((==) `on` dbRoundReachedRoundNumber) (sortOn dbRoundReachedRoundNumber reacheds)))
    mkRoundNumber = undefined
    mkRoundRating = undefined
