{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE TypeOperators            #-}

module Api.Public.Routes where

import           Api.FromDb                  (dbRoundToRound, dbToQuizSummary,
                                              dbToScoreBoard, quizToIdentifier)
import           Api.ToDb                    (quizIdToKey)
import           Api.Types                   (QuizId (..), QuizSummary,
                                              SomeQuiz, fromActivity)
import qualified Api.Types                   as Api
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Maybe   (MaybeT (..))
import           Data.Pool                   (Pool)
import           Database.Persist            (Entity (..), (==.))
import           Database.Persist.Postgresql (get, selectList)
import           Database.Persist.Sql        (SqlBackend)
import qualified Db.Schema                   as Db
import           Db.Util                     (runDb)
import           Servant

-- Public API: no authentication required

-- GET /public      → list all quizzes (for searching)
-- GET /public/:id  → single quiz

type PublicApi =
  "public" :>
    ( Get '[JSON] [QuizSummary]
        :<|> Capture "quizId" QuizId :> Get '[JSON] SomeQuiz
    )

publicApi :: Proxy PublicApi
publicApi = Proxy

-- Handlers

publicServer :: Pool SqlBackend -> Server PublicApi
publicServer pool =
  listQuizzes pool
    :<|> getQuiz pool

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
