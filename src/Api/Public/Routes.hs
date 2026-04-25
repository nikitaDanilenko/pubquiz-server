{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE TypeOperators            #-}

module Api.Public.Routes where

import           Api.FromDb                  (dbRoundToRound, dbTeamToTeam,
                                              dbToQuizSummary, dbToScoreBoard,
                                              dbToScores, quizToIdentifier)
import           Api.Public.Types            (RoundScore (..),
                                              StandingEntry (..), TeamInfo (..),
                                              TeamView (..))
import           Api.ToDb                    (quizIdToKey)
import           Api.Types                   (Points (..), QuizId (..),
                                              QuizSummary, Round (..),
                                              RoundNumber (..), SomeQuiz,
                                              Team (..), TeamNumber (..),
                                              fromActivity)
import qualified Api.Types                   as Api
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Maybe   (MaybeT (..), hoistMaybe)
import           Data.List                   (find, nub, sortOn)
import qualified Data.Map.Strict             as Map
import           Data.Ord                    (Down (..))
import           Data.Pool                   (Pool)
import           Database.Persist            (Entity (..), (==.))
import           Database.Persist.Postgresql (get, selectList)
import           Database.Persist.Sql        (SqlBackend)
import qualified Db.Schema                   as Db
import           Db.Util                     (runDb)
import           Servant

-- Public API: no authentication required

-- GET /public                       → list all quizzes (for searching)
-- GET /public/:id                   → single quiz
-- GET /public/:id/teams/:teamNumber → team's perspective

type PublicApi =
  "public" :>
    ( Get '[JSON] [QuizSummary]
        :<|> Capture "quizId" QuizId :> Get '[JSON] SomeQuiz
        :<|> Capture "quizId" QuizId :> "teams" :> Capture "teamNumber" TeamNumber :> Get '[JSON] TeamView
    )

publicApi :: Proxy PublicApi
publicApi = Proxy

-- Handlers

publicServer :: Pool SqlBackend -> Server PublicApi
publicServer pool =
  listQuizzes pool
    :<|> getQuiz pool
    :<|> getTeamView pool

listQuizzes :: Pool SqlBackend -> Handler [QuizSummary]
listQuizzes pool = runDb pool $ do
  quizEntities <- selectList [] []
  pure $ map dbToQuizSummary quizEntities

getQuiz :: Pool SqlBackend -> QuizId -> Handler SomeQuiz
getQuiz pool quizId = runDb pool statement >>= maybe (throwError err404) pure
 where
  dbQuizId = quizIdToKey quizId
  statement = runMaybeT $ do
    quizRecord <- MaybeT $ get dbQuizId
    teamEntities <- lift $ selectList [Db.TeamQuizId ==. dbQuizId] []
    roundEntities <- lift $ selectList [Db.RoundQuizId ==. dbQuizId] []
    scoreEntities <- lift $ selectList [Db.TeamRoundScoreQuizId ==. dbQuizId] []

    let quiz = Api.Quiz
          { Api.quizId = quizId
          , Api.identifier = quizToIdentifier quizRecord
          , Api.rounds = map (dbRoundToRound . entityVal) roundEntities
          , Api.scoreBoard = dbToScoreBoard teamEntities scoreEntities
          }

    pure $ fromActivity (Db.quizActive quizRecord) quiz

getTeamView :: Pool SqlBackend -> QuizId -> TeamNumber -> Handler TeamView
getTeamView pool quizId teamNumber = runDb pool statement >>= maybe (throwError err404) pure
 where
  dbQuizId = quizIdToKey quizId
  statement = runMaybeT $ do
    quiz <- MaybeT $ get dbQuizId
    -- We really need all active teams, because they are used later for the rank.
    -- Inactive teams are not shown, because there is a use case difference:
    -- * never played -> inactive, don't show
    -- * played, but left -> active, but 0 points from the point where they left, still part of the ranking
    teamEntities <- lift $ selectList [Db.TeamQuizId ==. dbQuizId, Db.TeamActive ==. True] []
    roundEntities <- lift $ selectList [Db.RoundQuizId ==. dbQuizId] []
    scoreEntities <- lift $ selectList [Db.TeamRoundScoreQuizId ==. dbQuizId] []

    let teams = map (dbTeamToTeam . entityVal) teamEntities
        rounds = map (dbRoundToRound . entityVal) roundEntities
        scores = dbToScores scoreEntities
    team <- hoistMaybe $ find (\t -> t.number == teamNumber) teams

    pure TeamView
      { identifier = quizToIdentifier quiz
      , team = TeamInfo
          { number = team.number
          , name = team.name
          }
      , scores = computeTeamScores teamNumber scores
      , standings = computeStandings rounds teams scores
      }

-- Helper: compute standings from teams and scores
computeStandings :: [Round] -> [Team] -> Map.Map (TeamNumber, RoundNumber) Points -> [StandingEntry]
computeStandings rounds teams scores = assignRanks sorted
 where
  -- Sum all reachable points across rounds
  totalReachable = sum [ unPoints r.displayMaxPoints | r <- rounds ]

  -- Build map of team number -> total points
  teamTotals = Map.fromListWith (+)
    [ (tn, unPoints pts)
    | ((tn, _), pts) <- Map.toList scores
    ]

  -- Build team info with totals
  teamsWithTotals = map (\team -> (team.name, Map.findWithDefault 0 team.number teamTotals)) teams

  -- Sort by total points descending
  sorted = sortOn (\(_, pts) -> Down pts) teamsWithTotals

  -- Assign ranks with ties: teams with same score share the same rank (dense ranking)
  assignRanks ts = zipWith toEntry ranks ts
   where
    allScores = map snd ts
    distinctScores = nub allScores
    ranks = [ 1 + length (filter (> pts) distinctScores) | pts <- allScores ]

  toEntry rank (tName, pts) = StandingEntry
    { rank = rank
    , teamName = tName
    , totalPoints = Points pts
    , reachablePoints = Points totalReachable
    }

computeTeamScores :: TeamNumber -> Map.Map (TeamNumber, RoundNumber) Points -> [RoundScore]
computeTeamScores teamNum scores =
  sortOn roundNumber
    [ RoundScore { roundNumber = rn, points = pts }
    | ((tn, rn), pts) <- Map.toList scores
    , tn == teamNum
    ]
