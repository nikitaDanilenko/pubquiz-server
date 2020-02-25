{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Db.DbConversion where

import           Control.Arrow      ((&&&))
import           Data.Aeson.TH      (defaultOptions, deriveJSON)
import           Data.Function      (on)
import           Data.List          (groupBy, sortOn)
import           Data.Map           (fromList, intersectionWith, toList)
import           Data.Text          (pack)
import qualified Data.Text          as T
import           Data.Time.Calendar (Day)
import           Database.Persist   (Entity, entityKey, entityVal)
import           Db.Connection      (DbQuiz (dbQuizDate, dbQuizName, dbQuizPlace),
                                     DbQuizId,
                                     DbRoundReachable (dbRoundReachablePoints, dbRoundReachableRoundNumber),
                                     DbRoundReached (dbRoundReachedPoints, dbRoundReachedRoundNumber, dbRoundReachedTeamNumber),
                                     DbTeamNameCode (DbTeamNameCode, dbTeamNameCodeActive, dbTeamNameCodeQuizId, dbTeamNameCodeTeamCode, dbTeamNameCodeTeamName, dbTeamNameCodeTeamNumber),
                                     DbUser (DbUser, dbUserUserHash, dbUserUserName, dbUserUserSalt),
                                     dbQuizActive)
import           General.Labels     (Labels, fallbackLabels)
import           General.Types      (Activity (Active), Code, Place, QuizDate,
                                     QuizName, RoundNumber (RoundNumber),
                                     TeamLabel, TeamName,
                                     TeamNumber (TeamNumber), Unwrappable,
                                     UserHash, UserName, UserSalt, unwrap, wrap)
import           GHC.Natural        (Natural, intToNatural, naturalToInt)
import           Utils              (randomDistinctHexadecimal)

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

data TeamInfo =
  TeamInfo
    { teamInfoCode     :: Code
    , teamInfoName     :: TeamName
    , teamInfoNumber   :: TeamNumber
    , teamInfoActivity :: Activity
    }

dbTeamNameCodeToTeamInfo :: DbTeamNameCode -> TeamInfo
dbTeamNameCodeToTeamInfo db =
  TeamInfo
    { teamInfoCode = wrap (dbTeamNameCodeTeamCode db)
    , teamInfoName = wrap (dbTeamNameCodeTeamName db)
    , teamInfoNumber = wrap (dbTeamNameCodeTeamNumber db)
    , teamInfoActivity = wrap (dbTeamNameCodeActive db)
    }

teamInfoToDbTeamNameCode :: DbQuizId -> TeamInfo -> DbTeamNameCode
teamInfoToDbTeamNameCode qid ti =
  DbTeamNameCode
    { dbTeamNameCodeQuizId = qid
    , dbTeamNameCodeTeamCode = unwrap (teamInfoCode ti)
    , dbTeamNameCodeTeamName = unwrap (teamInfoName ti)
    , dbTeamNameCodeTeamNumber = unwrap (teamInfoNumber ti)
    , dbTeamNameCodeActive = unwrap (teamInfoActivity ti)
    }

deriveJSON defaultOptions ''TeamInfo

newtype Header =
  Header [TeamInfo]

deriveJSON defaultOptions ''Header

defaultTeamName :: TeamNumber -> TeamLabel -> TeamName
defaultTeamName tn tl = wrap (T.unwords [unwrap tl, T.pack (show (unwrap tn :: Natural))])

mkDefaultTeamInfos :: Natural -> TeamLabel -> [String] -> [TeamInfo]
mkDefaultTeamInfos lowestTeamNumber teamLabel =
  zipWith
    (\n e ->
       TeamInfo
         { teamInfoCode = wrap e
         , teamInfoName = defaultTeamName (wrap n) teamLabel
         , teamInfoNumber = wrap n
         , teamInfoActivity = Active
         })
    [lowestTeamNumber ..]

adjustHeaderToSize :: Natural -> Int -> TeamLabel -> Header -> IO Header
adjustHeaderToSize n codeSize teamLabel h
  | n <= size = pure (wrap (take (naturalToInt n) ts))
  | otherwise =
    fmap
      (wrap . (unwrap h ++) . mkDefaultTeamInfos (1 + size) teamLabel)
      (randomDistinctHexadecimal (naturalToInt (n - size)) codeSize)
  where
    ts = unwrap h :: [TeamInfo]
    size = intToNatural (length ts)

instance Unwrappable Header [TeamInfo] where
  wrap = Header
  unwrap (Header tis) = tis

newtype Ratings =
  Ratings [(RoundNumber, RoundRating)]

deriveJSON defaultOptions ''Ratings

instance Unwrappable Ratings [(RoundNumber, RoundRating)] where
  wrap = Ratings
  unwrap (Ratings rs) = rs

data QuizRatings =
  QuizRatings
    { header  :: Header
    , ratings :: Ratings
    }

deriveJSON defaultOptions ''QuizRatings

data Credentials =
  Credentials
    { user      :: UserName
    , signature :: UserHash
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

data QuizIdentifier =
  QuizIdentifier
    { place :: Place
    , date  :: QuizDate
    , name  :: QuizName
    }

deriveJSON defaultOptions ''QuizIdentifier

data QuizInfo =
  QuizInfo
    { quizId         :: DbQuizId
    , quizIdentifier :: QuizIdentifier
    , active         :: Activity
    }

deriveJSON defaultOptions ''QuizInfo

mkQuizInfo :: Entity DbQuiz -> QuizInfo
mkQuizInfo eq =
  QuizInfo
    { quizId = entityKey eq
    , quizIdentifier =
        QuizIdentifier
          {name = wrap (T.pack (dbQuizName q)), date = wrap (dbQuizDate q), place = wrap (T.pack (dbQuizPlace q))}
    , active = wrap (dbQuizActive q)
    }
  where
    q = entityVal eq

fullQuizName :: QuizIdentifier -> T.Text
fullQuizName identifier =
  T.unwords
    [ T.concat [T.pack (show (unwrap (date identifier) :: Day)), T.pack ":"]
    , unwrap (name identifier)
    , T.concat [T.pack "(", unwrap (place identifier), T.pack ")"]
    ]

data SavedUser =
  SavedUser
    { userName :: UserName
    , userSalt :: UserSalt
    , userHash :: UserHash
    }

savedUserToDbUser :: SavedUser -> DbUser
savedUserToDbUser savedUser =
  DbUser
    { dbUserUserName = unwrap (userName savedUser)
    , dbUserUserSalt = unwrap (userSalt savedUser)
    , dbUserUserHash = unwrap (userHash savedUser)
    }

dbUserToSavedUser :: DbUser -> SavedUser
dbUserToSavedUser dbUser =
  SavedUser
    { userName = wrap (dbUserUserName dbUser)
    , userSalt = wrap (dbUserUserSalt dbUser)
    , userHash = wrap (dbUserUserHash dbUser)
    }
