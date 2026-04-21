{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api.Public.Routes where

import           Api.Public.Types (QuizSummary, QuizView, TeamView)
import           Core.Domain      (QuizId, TeamNumber)
import           Servant

-- Public API: no authentication required

-- GET /                  → list all quizzes (for searching)
-- GET /:id               → single quiz view with leaderboard
-- GET /:id/teams/:teamNumber → team's perspective

type PublicApi =
  Get '[JSON] [QuizSummary]
    :<|> Capture "quizId" QuizId :> Get '[JSON] QuizView
    :<|> Capture "quizId"  QuizId :>  "teams" :> Capture "teamNumber" TeamNumber :> Get '[JSON] TeamView

publicApi :: Proxy PublicApi
publicApi = Proxy

-- Handlers (placeholder implementations)

publicServer :: Server PublicApi
publicServer =
  listQuizzes
    :<|> getQuiz
    :<|> getTeamView

listQuizzes :: Handler [QuizSummary]
listQuizzes = undefined

getQuiz :: QuizId -> Handler QuizView
getQuiz = undefined

getTeamView :: QuizId -> TeamNumber -> Handler TeamView
getTeamView = undefined
