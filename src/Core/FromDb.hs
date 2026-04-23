{-# LANGUAGE DisambiguateRecordFields #-}

module Core.FromDb
  ( quizKeyToId
  , quizIdToKey
  , quizToIdentifier
  , dbRoundToRound
  , dbTeamToTeam
  , dbToScoreBoard
  , dbToQuizSummary
  )
where

import           Core.Domain          (Place (..), Points (..), QuizId (..),
                                       QuizIdentifier (..), QuizName (..),
                                       QuizSummary (..), Round (..),
                                       RoundNumber (..), ScoreBoard (..),
                                       Team (..), TeamName (..),
                                       TeamNumber (..))
import qualified Data.Map.Strict      as Map
import           Database.Persist     (Entity (..))
import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import qualified Db.Schema            as Db

quizKeyToId :: Db.Key Db.Quiz -> QuizId
quizKeyToId = QuizId . fromIntegral . fromSqlKey

quizIdToKey :: QuizId -> Db.Key Db.Quiz
quizIdToKey = toSqlKey . fromIntegral . unQuizId

quizToIdentifier :: Db.Quiz -> QuizIdentifier
quizToIdentifier quiz =
  QuizIdentifier
    { name = QuizName (Db.quizName quiz)
    , place = Place (Db.quizPlace quiz)
    , date = Db.quizDate quiz
    }

dbRoundToRound :: Db.Round -> Round
dbRoundToRound round =
  Round
    { roundNumber = RoundNumber (Db.roundRoundNumber round)
    , displayMaxPoints = Points (Db.roundReachablePoints round)
    , numberOfQuestions = Db.roundNumberOfQuestions round
    }

dbTeamToTeam :: Db.Team -> Team
dbTeamToTeam team =
  Team
    { number = TeamNumber (Db.teamNumber team)
    , teamName = TeamName (Db.teamName team)
    , active = Db.teamActive team
    }

dbToScoreBoard :: [Entity Db.Team] -> [Entity Db.TeamRoundScore] -> ScoreBoard
dbToScoreBoard teamEntities scoreEntities =
  ScoreBoard
    { teams = map (dbTeamToTeam . entityVal) teamEntities
    , scores =
        Map.fromList
          [ ((TeamNumber (Db.teamRoundScoreTeamNumber score), RoundNumber (Db.teamRoundScoreRoundNumber score)), Points (Db.teamRoundScorePoints score))
          | Entity _ score <- scoreEntities
          ]
    }

dbToQuizSummary :: Entity Db.Quiz -> QuizSummary
dbToQuizSummary (Entity quizKey quiz) =
  QuizSummary
    { quizId = quizKeyToId quizKey
    , identifier = quizToIdentifier quiz
    , active = Db.quizActive quiz
    }
