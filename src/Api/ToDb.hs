{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot      #-}

module Api.ToDb
  ( quizIdToKey
  , identifierToQuiz
  , teamToDb
  , teamRoundScoreToDb
  )
where

import           Api.Types            (Place (..), Points (..), QuizId (..),
                                       QuizIdentifier (..), QuizName (..),
                                       RoundNumber (..), Team (..),
                                       TeamName (..), TeamNumber (..))
import           Database.Persist.Sql (toSqlKey)
import qualified Db.Schema            as Db

quizIdToKey :: QuizId -> Db.Key Db.Quiz
quizIdToKey = toSqlKey . fromIntegral . unQuizId

identifierToQuiz :: QuizIdentifier -> Db.Quiz
identifierToQuiz ident =
  Db.Quiz
    { Db.quizPlace = unPlace ident.place
    , Db.quizDate = ident.date
    , Db.quizName = unQuizName ident.name
    , Db.quizActive = True
    }

teamRoundScoreToDb :: Db.Key Db.Quiz -> TeamNumber -> RoundNumber -> Points -> Db.TeamRoundScore
teamRoundScoreToDb quizKey teamNum roundNum pts =
  Db.TeamRoundScore
    { Db.teamRoundScoreQuizId = quizKey
    , Db.teamRoundScoreTeamNumber = unTeamNumber teamNum
    , Db.teamRoundScoreRoundNumber = unRoundNumber roundNum
    , Db.teamRoundScorePoints = unPoints pts
    }

teamToDb :: Db.Key Db.Quiz -> Team -> Db.Team
teamToDb quizKey team =
  Db.Team
    { Db.teamQuizId = quizKey
    , Db.teamNumber = unTeamNumber team.number
    , Db.teamName = unTeamName team.name
    , Db.teamActive = team.active
    }
