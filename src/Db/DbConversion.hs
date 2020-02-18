{-# LANGUAGE DeriveGeneric #-}

module Db.DbConversion where

import           Control.Arrow    ((&&&))
import           Data.Aeson       (FromJSON, ToJSON (toJSON), object, (.=))
import           Data.Function    (on)
import           Data.List        (groupBy, sortOn)
import           Data.Map         (fromList, intersectionWith, toList)
import           Data.Text        (pack)
import           Database.Persist (Entity, entityKey, entityVal)
import           Db.Connection    (DbQuiz (dbQuizDate, dbQuizName, dbQuizPlace),
                                   DbQuizId,
                                   DbRoundReachable (dbRoundReachablePoints, dbRoundReachableRoundNumber),
                                   DbRoundReached (dbRoundReachedPoints, dbRoundReachedRoundNumber, dbRoundReachedTeamNumber))
import           General.Types    (Place, QuizDate, QuizName,
                                   RoundNumber (RoundNumber),
                                   TeamNumber (TeamNumber), wrap)
import           GHC.Generics     (Generic)

data TeamRating =
  TeamRating
    { teamNumber :: TeamNumber
    , rating     :: Double
    }
  deriving (Generic)

instance ToJSON TeamRating

instance FromJSON TeamRating

data RoundRating =
  RoundRating
    { reachableInRound :: Double
    , points           :: [TeamRating]
    }
  deriving (Generic)

instance ToJSON RoundRating

instance FromJSON RoundRating

newtype Ratings =
  Ratings
    { roundRatings :: [(RoundNumber, RoundRating)]
    }
  deriving (Generic)

instance ToJSON Ratings

instance FromJSON Ratings

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

data QuizInfo =
  QuizInfo
    { quizId :: DbQuizId
    , name   :: QuizName
    , date   :: QuizDate
    , place  :: Place
    }
  deriving (Generic)

instance ToJSON QuizInfo

instance FromJSON QuizInfo

mkQuizInfo :: Entity DbQuiz -> QuizInfo
mkQuizInfo eq =
  QuizInfo {quizId = entityKey eq, name = wrap (dbQuizName q), date = wrap (dbQuizDate q), place = wrap (dbQuizPlace q)}
  where
    q = entityVal eq
