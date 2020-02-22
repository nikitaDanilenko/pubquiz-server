{-# LANGUAGE TemplateHaskell #-}

module Db.DbConversion where

import           Control.Arrow    ((&&&))
import           Data.Aeson.TH    (deriveJSON, defaultOptions)
import           Data.Function    (on)
import           Data.List        (groupBy, sortOn)
import           Data.Map         (fromList, intersectionWith, toList)
import           Data.Text        (pack)
import qualified Data.Text        as T
import           Database.Persist (Entity, entityKey, entityVal)
import           Db.Connection    (DbQuiz (dbQuizDate, dbQuizName, dbQuizPlace),
                                   DbQuizId,
                                   DbRoundReachable (dbRoundReachablePoints, dbRoundReachableRoundNumber),
                                   DbRoundReached (dbRoundReachedPoints, dbRoundReachedRoundNumber, dbRoundReachedTeamNumber))
import           General.Labels   (Labels, fallbackLabels)
import           General.Types    (Place, QuizDate, QuizName,
                                   RoundNumber (RoundNumber),
                                   TeamNumber (TeamNumber), wrap, UserName, UserSalt, UserHash)
import           GHC.Natural      (Natural)

data TeamRating =
  TeamRating
    { teamNumber :: TeamNumber
    , rating     :: Double
    }

deriveJSON defaultOptions ''TeamRating

data RoundRating =
  RoundRating
    { reachableInRound :: Double
    , points           :: [TeamRating]
    }

deriveJSON defaultOptions ''RoundRating

newtype Ratings =
  Ratings
    { roundRatings :: [(RoundNumber, RoundRating)]
    }

deriveJSON defaultOptions ''Ratings

data Credentials =
  Credentials
    { user      :: T.Text
    , signature :: T.Text
    }

deriveJSON defaultOptions ''Credentials

data QuizSettings =
  QuizSettings
    { rounds        :: [Natural]
    , numberOfTeams :: Natural
    , labels        :: Labels
    }

deriveJSON defaultOptions ''QuizSettings

fallbackSettings :: QuizSettings
fallbackSettings = QuizSettings {rounds = replicate 4 8, numberOfTeams = 20, labels = fallbackLabels}

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

data QuizPDN =
  QuizPDN
    { place :: Place
    , date  :: QuizDate
    , name  :: QuizName
    }

deriveJSON defaultOptions ''QuizPDN

data QuizInfo =
  QuizInfo
    { quizId     :: DbQuizId
    , identifier :: QuizPDN
    }

deriveJSON defaultOptions ''QuizInfo

mkQuizInfo :: Entity DbQuiz -> QuizInfo
mkQuizInfo eq =
  QuizInfo
    { quizId = entityKey eq
    , identifier =
        QuizPDN {name = wrap (T.pack (dbQuizName q)), date = wrap (dbQuizDate q), place = wrap (T.pack (dbQuizPlace q))}
    }
  where
    q = entityVal eq

data SavedUser = SavedUser { 
    userName :: UserName, 
    userSalt :: UserSalt, 
    userHash :: UserHash
}