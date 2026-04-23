{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Api.BackOffice.Routes where

import           Api.BackOffice.Types        (QuizSummary (..))
import           Api.Util                    (runDb)
import           Control.Monad               (forM, forM_, unless)
import           Core.Domain                 (NumberOfQuestions (..),
                                              Place (..), Points (..),
                                              Quiz (..), QuizId (..),
                                              QuizIdentifier (..),
                                              QuizName (..), QuizSettings (..),
                                              QuizState (..), Round (..),
                                              RoundNumber (..), ScoreBoard (..),
                                              SomeQuiz (..), Team (..),
                                              TeamName (..), TeamNumber (..),
                                              fromActivity)
import           Core.FromDb                 (dbRoundToRound,
                                              dbScoresToScoreBoard,
                                              dbTeamToTeam, quizIdToKey,
                                              quizKeyToId, quizToIdentifier)
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.List                   (nub)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (isJust)
import           Data.Pool                   (Pool)
import           Database.Persist            (Entity (..), (=.), (==.))
import           Database.Persist.Postgresql (get, insert, selectList, update,
                                              upsert)
import           Database.Persist.Sql        (SqlBackend)
import qualified Db.Schema                   as Db
import           GHC.Generics                (Generic)
import           Servant
import           Servant.Auth.Server

-- JWT payload: authenticated organizer
newtype AuthenticatedUser = AuthenticatedUser
  {    isAdmin       :: Bool
   }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToJWT, FromJWT)

data QuizMetaData = QuizMetaData
  {
    identifier :: QuizIdentifier,
    settings   :: QuizSettings
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data UpdateScoreRequest = UpdateScoreRequest
  { teamNumber  :: TeamNumber
  , roundNumber :: RoundNumber
  , points      :: Points
  }
  deriving (Show, Eq, Generic, FromJSON)

-- GET  /backoffice                → list quizzes
-- POST /backoffice                → create quiz
-- GET  /backoffice/:id            → view quiz
-- PUT  /backoffice/:id/settings   → update quiz setup
-- POST /backoffice/:id/scores     → enter scores
-- POST /backoffice/:id/lock       → lock quiz
-- POST /backoffice/:id/unlock     → unlock quiz

type BackOfficeRoutes =
  Get '[JSON] [QuizSummary]
    :<|> ReqBody '[JSON] QuizMetaData :> Post '[JSON] (Quiz 'Active)
    :<|> Capture "quizId" QuizId :> Get '[JSON] SomeQuiz
    :<|> Capture "quizId" QuizId :> "settings" :> ReqBody '[JSON] QuizMetaData :> Put '[JSON] QuizMetaData
    :<|> Capture "quizId" QuizId :> "scores" :> ReqBody '[JSON] UpdateScoreRequest :> Post '[JSON] ScoreBoard
    :<|> Capture "quizId" QuizId :> "lock" :> Post '[JSON] NoContent
    :<|> Capture "quizId" QuizId :> "unlock" :> Post '[JSON] NoContent

-- Wrap routes with JWT authentication
type BackOfficeApi = "backoffice" :> Auth '[JWT] AuthenticatedUser :> BackOfficeRoutes

backOfficeApi :: Proxy BackOfficeApi
backOfficeApi = Proxy

-- Handlers (placeholder implementations)

backOfficeServer :: Pool SqlBackend -> AuthResult AuthenticatedUser -> Server BackOfficeRoutes
backOfficeServer pool (Authenticated user) =
  listQuizzes pool
    :<|> createQuiz pool
    :<|> getQuiz pool
    :<|> updateSettings pool
    :<|> updateScore pool
    :<|> lockQuiz pool
    :<|> unlockQuiz pool user
backOfficeServer _ _ = throwAll err401

listQuizzes :: Pool SqlBackend -> Handler [QuizSummary]
listQuizzes pool = runDb pool statement
 where
  statement = do
    quizEntities <- selectList [] []
    pure $ map toQuizSummary quizEntities
   where
    toQuizSummary (Entity quizKey quiz) =
      QuizSummary
        { quizId = quizKeyToId quizKey
        , identifier = quizToIdentifier quiz
        , active = Db.quizActive quiz
        }

createQuiz :: Pool SqlBackend -> QuizMetaData -> Handler (Quiz 'Active)
createQuiz pool request = do
  quizKey <- runDb pool statement
  pure $
    Quiz
      { quizId = quizKeyToId quizKey
      , identifier = request.identifier
      , rounds = []
      -- todo: better - take the teams from the insert statement.
      , teams = [ Team (TeamNumber n) (TeamName "") True | n <- [1 .. numberOfTeams request.settings] ]
      , scoreBoard = ScoreBoard mempty
      }
 where
  statement = do
    -- Insert quiz record
    quizId <-
      insert $
        Db.Quiz
          { quizPlace = unPlace (place request.identifier)
          , quizDate = date request.identifier
          , quizName = unQuizName (name request.identifier)
          , quizActive = True
          }

    -- Insert default teams
    forM_ [1 .. numberOfTeams request.settings] $ \teamNumber -> do
      insert $
        Db.Team
          { teamQuizId = quizId
          , teamNumber = teamNumber
          , teamName = ""
          , teamActive = True
          }

    pure quizId

getQuiz :: Pool SqlBackend -> QuizId -> Handler SomeQuiz
getQuiz pool quizId = runDb pool statement >>= maybe (throwError err404) pure
 where
  dbQuizId = quizIdToKey quizId
  statement = do
    maybeQuiz <- get dbQuizId
    forM maybeQuiz $ \quizRecord -> do
      teamEntities <- selectList [Db.TeamQuizId ==. dbQuizId] []
      roundEntities <- selectList [Db.RoundQuizId ==. dbQuizId] []
      scoreEntities <- selectList [Db.TeamRoundScoreQuizId ==. dbQuizId] []

      let quiz :: Quiz state
          quiz =
            Quiz
              { quizId = quizId
              , identifier = quizToIdentifier quizRecord
              , rounds = map (dbRoundToRound . entityVal) roundEntities
              , teams = map (dbTeamToTeam . entityVal) teamEntities
              , scoreBoard = dbScoresToScoreBoard scoreEntities
              }

      pure $ fromActivity (Db.quizActive quizRecord) quiz


data UpdateResult a = NotFound | NotEditable | Success a

updateSettings :: Pool SqlBackend -> QuizId -> QuizMetaData -> Handler QuizMetaData
updateSettings pool quizId request = do
  result <- runDb pool statement
  case result of
    NotFound         -> throwError err404
    NotEditable      -> throwError err409
    Success metaData -> pure metaData
 where
  dbQuizId = quizIdToKey quizId
  statement = do
    maybeQuiz <- get dbQuizId
    case maybeQuiz of
      Nothing -> pure NotFound
      Just quiz
        | not (Db.quizActive quiz) -> pure NotEditable
        | otherwise -> do
            -- Update quiz identifier
            update dbQuizId
              [ Db.QuizName =. unQuizName (name request.identifier)
              , Db.QuizPlace =. unPlace (place request.identifier)
              , Db.QuizDate =. date request.identifier
              ]

            -- Get max team number from Team table
            -- Todo: This looks more complicated than it should be. Check again.
            teamEntities <- selectList [Db.TeamQuizId ==. dbQuizId] []
            let maxTeamInDb = if null teamEntities
                              then 0
                              else maximum $ map (Db.teamNumber . entityVal) teamEntities

            -- Get which rounds have score entries
            scoreEntities <- selectList [Db.TeamRoundScoreQuizId ==. dbQuizId] []
            let roundsWithScores = nub $ map (Db.teamRoundScoreRoundNumber . entityVal) scoreEntities

            let requestedTeams = numberOfTeams request.settings
            let finalTeamCount = max maxTeamInDb requestedTeams

            -- If we need more teams, add zero scores for rounds that have entries
            forM_ [maxTeamInDb + 1 .. finalTeamCount] $ \teamNum ->
              forM_ roundsWithScores $ \roundNum ->
                insert $
                  Db.TeamRoundScore
                    { teamRoundScoreQuizId = dbQuizId
                    , teamRoundScoreTeamNumber = teamNum
                    , teamRoundScoreRoundNumber = roundNum
                    , teamRoundScorePoints = 0
                    }

            let updatedSettings = request.settings { numberOfTeams = finalTeamCount }
            pure $ Success request { settings = updatedSettings }


updateScore :: Pool SqlBackend -> QuizId -> UpdateScoreRequest -> Handler ScoreBoard
updateScore pool quizId request = do
  result <- runDb pool statement
  case result of
    NotFound      -> throwError err404
    NotEditable   -> throwError err409
    Success board -> pure board
 where
  dbQuizId = quizIdToKey quizId
  statement = do
    maybeQuiz <- get dbQuizId
    case maybeQuiz of
      Nothing -> pure NotFound
      Just quiz
        | not (Db.quizActive quiz) -> pure NotEditable
        | otherwise -> do
            -- Todo: This is completely wrong - We want to update the score board, not a single score!
            _ <-
              upsert
                Db.TeamRoundScore
                  { teamRoundScoreQuizId = dbQuizId
                  , teamRoundScoreTeamNumber = unTeamNumber request.teamNumber
                  , teamRoundScoreRoundNumber = unRoundNumber request.roundNumber
                  , teamRoundScorePoints = unPoints request.points
                  }
                [Db.TeamRoundScorePoints =. unPoints request.points]

            -- Fetch updated scoreboard
            scoreEntities <- selectList [Db.TeamRoundScoreQuizId ==. dbQuizId] []
            pure $ Success (dbScoresToScoreBoard scoreEntities)

setQuizActiveHandler :: Pool SqlBackend -> QuizId -> Bool -> Handler NoContent
setQuizActiveHandler pool quizId active = do
  found <- runDb pool $ do
    let dbQuizId = quizIdToKey quizId
    maybeQuiz <- get dbQuizId
    forM_ maybeQuiz $ \_ -> update dbQuizId [Db.QuizActive =. active]
    pure (isJust maybeQuiz)
  unless found $ throwError err404
  pure NoContent

lockQuiz :: Pool SqlBackend -> QuizId -> Handler NoContent
lockQuiz pool quizId = setQuizActiveHandler pool quizId False

unlockQuiz :: Pool SqlBackend -> AuthenticatedUser -> QuizId -> Handler NoContent
unlockQuiz pool user quizId = do
  unless user.isAdmin $ throwError err403
  setQuizActiveHandler pool quizId True
