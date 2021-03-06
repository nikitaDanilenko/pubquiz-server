{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Db.DbConversion where

import           Constants            (qrOnlyFileName, sheetFileName)
import           Control.Arrow        (first, (&&&))
import           Data.Aeson           (encode)
import           Data.Aeson.TH        (defaultOptions, deriveJSON)
import qualified Data.ByteString.Lazy as L
import           Data.Function        (on)
import           Data.List            (groupBy, maximumBy, sortOn)
import           Data.Map             (elems, fromList, intersectionWith,
                                       intersectionWithKey, toList)
import           Data.Ord             (comparing)
import           Data.Text            (pack)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as E
import           Data.Time.Calendar   (Day)
import           Database.Persist     (Entity, entityKey, entityVal)
import           Db.Connection        (DbQuiz (dbQuizDate, dbQuizName, dbQuizPlace),
                                       DbQuizId, DbRoundQuestions,
                                       DbRoundReachable (dbRoundReachablePoints, dbRoundReachableRoundNumber),
                                       DbRoundReached (dbRoundReachedPoints, dbRoundReachedRoundNumber, dbRoundReachedTeamNumber),
                                       DbTeamNameCode (DbTeamNameCode, dbTeamNameCodeActive, dbTeamNameCodeQuizId, dbTeamNameCodeTeamCode, dbTeamNameCodeTeamName, dbTeamNameCodeTeamNumber),
                                       DbUser (DbUser, dbUserUserHash, dbUserUserName, dbUserUserSalt),
                                       dbQuizActive, dbRoundQuestionsQuestions,
                                       dbRoundQuestionsRoundNumber)
import           General.Labels       (Labels)
import           General.Types        (Activity (Active), Code,
                                       NumberOfQuestions, Place, QuizDate,
                                       QuizName, RoundNumber (RoundNumber),
                                       TeamLabel, TeamName,
                                       TeamNumber (TeamNumber), Wrapped,
                                       UserHash, UserName, UserSalt, unwrap,
                                       wrap, fallback)
import           GHC.Natural          (Natural, intToNatural, naturalToInt)
import           Utils                (randomDistinctHexadecimal, elmOptions)

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
         { teamInfoCode = wrap (show n ++ e)
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

instance Wrapped Header [TeamInfo] where
  wrap = Header
  unwrap (Header tis) = tis

newtype Ratings =
  Ratings [(RoundNumber, RoundRating)]

deriveJSON defaultOptions ''Ratings

instance Wrapped Ratings [(RoundNumber, RoundRating)] where
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

deriveJSON elmOptions ''Credentials

data QuestionsInRound =
  QuestionsInRound
    { questionsInRoundRoundNumber       :: RoundNumber
    , questionsInRoundNumberOfQuestions :: NumberOfQuestions
    }

deriveJSON defaultOptions ''QuestionsInRound

dbRoundQuestionsToQuestionsInRound :: DbRoundQuestions -> QuestionsInRound
dbRoundQuestionsToQuestionsInRound rq =
  QuestionsInRound (wrap (dbRoundQuestionsRoundNumber rq)) (wrap (dbRoundQuestionsQuestions rq))

newtype QuestionsInQuiz =
  QuestionsInQuiz [QuestionsInRound]

deriveJSON defaultOptions ''QuestionsInQuiz

instance Wrapped QuestionsInQuiz [QuestionsInRound] where
  unwrap (QuestionsInQuiz rqs) = rqs
  wrap = QuestionsInQuiz

data QuizSettings =
  QuizSettings
    { questionsInQuiz :: QuestionsInQuiz
    , numberOfTeams   :: Natural
    , labels          :: Labels
    }

deriveJSON defaultOptions ''QuizSettings

fallbackSettings :: QuizSettings
fallbackSettings =
  QuizSettings
    { questionsInQuiz =
        wrap
          (zipWith (\rn q -> QuestionsInRound (wrap (intToNatural rn)) (wrap (intToNatural q))) [1 ..] (replicate 4 8))
    , numberOfTeams = 20
    , labels = fallback :: Labels
    }

mkQuizSettings :: QuestionsInQuiz -> Header -> Labels -> QuizSettings
mkQuizSettings qs h ls = 
  QuizSettings {
    questionsInQuiz = qs,
    numberOfTeams = intToNatural (length (unwrap h :: [TeamInfo])),
    labels = ls
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
    , fullSheetPath  :: T.Text
    , qrOnlyPath     :: T.Text
    }

deriveJSON defaultOptions ''QuizInfo

mkQuizInfo :: T.Text -> Entity DbQuiz -> QuizInfo
mkQuizInfo sheetsFolder eq =
  QuizInfo
    { quizId = qid
    , quizIdentifier =
        QuizIdentifier {name = wrap (T.pack (dbQuizName q)), date = wrap day, place = wrap (T.pack (dbQuizPlace q))}
    , active = wrap (dbQuizActive q)
    , fullSheetPath = mkPathForQuizSheetWith (T.pack ".pdf") sheetsFolder sheetFileName day qid
    , qrOnlyPath = mkPathForQuizSheetWith (T.pack ".pdf") sheetsFolder qrOnlyFileName day qid
    }
  where
    q = entityVal eq
    qid = entityKey eq
    day = dbQuizDate q

mkPathForQuizSheetWith :: T.Text -> T.Text -> T.Text -> Day -> DbQuizId -> T.Text
mkPathForQuizSheetWith fileEnding sheetsFolder fileName day qid =
  T.intercalate
    (T.pack "/")
    [ sheetsFolder
    , T.intercalate
        (T.pack "-")
        [T.pack (show day), E.decodeUtf8 (L.toStrict (encode qid)), T.concat [fileName, fileEnding]]
    ]

data TeamLine =
  TeamLine
    { roundNumber     :: RoundNumber
    , reachedPoints   :: Double
    , maximumPoints   :: Double
    , reachablePoints :: Double
    }

deriveJSON defaultOptions ''TeamLine

newtype TeamTable =
  TeamTable [TeamLine]

deriveJSON defaultOptions ''TeamTable

data TeamQuery =
  TeamQuery
    { teamQueryQuizId     :: DbQuizId
    , teamQueryTeamNumber :: TeamNumber
    , teamQueryTeamCode   :: Code
    }

deriveJSON defaultOptions ''TeamQuery

data TeamTableInfo =
  TeamTableInfo
    { teamTable                  :: TeamTable
    , teamTableInfoTeamName      :: TeamName
    , teamTableInfoNumberOfTeams :: Natural
    , teamTableInfoTeamNumber    :: TeamNumber
    }

deriveJSON defaultOptions ''TeamTableInfo

mkTeamTable :: [Entity DbRoundReached] -> [Entity DbRoundReachable] -> [Entity DbRoundReached] -> TeamTable
mkTeamTable reached reachable allReached =
  TeamTable (elems (intersectionWithKey (\k r (p, m) -> TeamLine k r p m) ratings (intersectionWith (,) possibles ms)))
  where
    ratings = fromList (mkReached wrap reached)
    possibles = fromList (map (((wrap . dbRoundReachableRoundNumber) &&& dbRoundReachablePoints) . entityVal) reachable)
    ms =
      fromList
        (map (first wrap . maximumBy (comparing snd)) $ groupBy ((==) `on` fst) $ sortOn fst (mkReached id allReached))
    mkReached w = map (((w . dbRoundReachedRoundNumber) &&& dbRoundReachedPoints) . entityVal)

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
