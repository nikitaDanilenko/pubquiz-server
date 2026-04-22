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
import           Control.Monad               (forM_, unless)
import           Core.Domain                 (NumberOfQuestions (..),
                                              Place (..), Points (..),
                                              Quiz (..), QuizId (..),
                                              QuizIdentifier (..),
                                              QuizName (..), QuizSettings (..),
                                              QuizState (..), Round (..),
                                              RoundNumber (..), ScoreBoard (..),
                                              SomeQuiz (..), Team (..),
                                              TeamName (..), TeamNumber (..))
import           Data.Aeson                  (FromJSON, ToJSON)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (isJust)
import           Data.Pool                   (Pool)
import           Data.Text                   (Text)
import           Database.Persist            (Entity (..), (=.), (==.))
import           Database.Persist.Postgresql (delete, get, insert, selectList,
                                              update, upsert)
import           Database.Persist.Sql        (SqlBackend, fromSqlKey, toSqlKey)
import qualified Db.Schema                   as Db
import           GHC.Generics                (Generic)
import           Servant
import           Servant.Auth.Server

-- JWT payload: authenticated organizer
newtype AuthenticatedUser = AuthenticatedUser
  {    isAdmin       :: Bool
   }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToJWT, FromJWT)

data CreateQuizRequest = CreateQuizRequest
  { identifier :: QuizIdentifier
  , settings   :: QuizSettings
  }
  deriving (Show, Eq, Generic, FromJSON)

newtype UpdateQuizSettingsRequest = UpdateQuizSettingsRequest
  { settings :: QuizSettings
  }
  deriving (Show, Eq, Generic, FromJSON)

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
    :<|> ReqBody '[JSON] CreateQuizRequest :> Post '[JSON] (Quiz 'Active)
    :<|> Capture "quizId" QuizId :> Get '[JSON] SomeQuiz
    :<|> Capture "quizId" QuizId :> "settings" :> ReqBody '[JSON] UpdateQuizSettingsRequest :> Put '[JSON] (Quiz 'Active)
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

dbScoresToScoreBoard :: [Entity Db.TeamRoundScore] -> ScoreBoard
dbScoresToScoreBoard scoreEntities =
  ScoreBoard $
    Map.fromList
      [ ((TeamNumber (Db.teamRoundScoreTeamNumber score), RoundNumber (Db.teamRoundScoreRoundNumber score)), Points (Db.teamRoundScorePoints score))
      | Entity _ score <- scoreEntities
      ]

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

createQuiz :: Pool SqlBackend -> CreateQuizRequest -> Handler (Quiz 'Active)
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
getQuiz pool quizId = do
  maybeResult <- runDb pool statement
  case maybeResult of
    Nothing -> throwError err404
    Just (quizIdentifier, domainRounds, domainTeams, scoreBoard, isActive) ->
      let mkQuiz :: Quiz state
          mkQuiz =
            Quiz
              { quizId = quizId
              , identifier = quizIdentifier
              , rounds = domainRounds
              , teams = domainTeams
              , scoreBoard = scoreBoard
              }
       in if isActive
            then pure $ SomeActive mkQuiz
            else pure $ SomeLocked mkQuiz
 where
  statement = do
    let dbQuizId = quizIdToKey quizId
    maybeQuiz <- get dbQuizId
    case maybeQuiz of
      Nothing -> pure Nothing
      Just quiz -> do
        teamEntities <- selectList [Db.TeamQuizId ==. dbQuizId] []
        roundEntities <- selectList [Db.RoundQuizId ==. dbQuizId] []
        scoreEntities <- selectList [Db.TeamRoundScoreQuizId ==. dbQuizId] []

        let domainTeams = map (dbTeamToTeam . entityVal) teamEntities

        let domainRounds = map (dbRoundToRound . entityVal) roundEntities

        let scoreBoard = dbScoresToScoreBoard scoreEntities

        let quizIdentifier = quizToIdentifier quiz

        pure $ Just (quizIdentifier, domainRounds, domainTeams, scoreBoard, Db.quizActive quiz)

updateSettings :: Pool SqlBackend -> QuizId -> UpdateQuizSettingsRequest -> Handler (Quiz 'Active)
updateSettings pool quizId request = do
  result <- runDb pool statement
  case result of
    Nothing           -> throwError err404
    Just (Left ())    -> throwError err409
    Just (Right quiz) -> pure quiz
 where
  statement = do
    let dbQuizId = quizIdToKey quizId
    maybeQuiz <- get dbQuizId
    case maybeQuiz of
      Nothing -> pure Nothing
      Just quiz
        | not (Db.quizActive quiz) -> pure $ Just (Left ())
        | otherwise -> do
            -- Delete existing rounds and recreate
            existingRounds <- selectList [Db.RoundQuizId ==. dbQuizId] []
            forM_ existingRounds $ \(Entity _ _) -> pure () -- Rounds have composite PK, handle separately

            -- Insert new rounds based on questionsPerRound
            forM_ (zip [1 ..] (questionsPerRound request.settings)) $ \(roundNum, numberOfQuestions) -> do
              upsert
                Db.Round
                  { roundQuizId = dbQuizId
                  , roundRoundNumber = roundNum
                  , roundReachablePoints = 0 -- Default, can be updated later
                  , roundNumberOfQuestions = fromIntegral $ unNumberOfQuestions numberOfQuestions
                  }
                [Db.RoundNumberOfQuestions =. fromIntegral (unNumberOfQuestions numberOfQuestions)]

            -- Adjust team count
            existingTeams <- selectList [Db.TeamQuizId ==. dbQuizId] []
            let currentTeamCount = length existingTeams
            let targetTeamCount = numberOfTeams request.settings

            -- Add new teams if needed
            forM_ [currentTeamCount + 1 .. targetTeamCount] $ \teamNumber -> do
              insert $
                Db.Team
                  { teamQuizId = dbQuizId
                  , teamNumber = teamNumber
                  , teamName = ""
                  , teamActive = True
                  }

            -- Fetch updated data
            teamEntities <- selectList [Db.TeamQuizId ==. dbQuizId] []
            roundEntities <- selectList [Db.RoundQuizId ==. dbQuizId] []
            scoreEntities <- selectList [Db.TeamRoundScoreQuizId ==. dbQuizId] []

            let domainTeams = map (dbTeamToTeam . entityVal) teamEntities

            let domainRounds = map (dbRoundToRound . entityVal) roundEntities

            let scoreBoard = dbScoresToScoreBoard scoreEntities

            pure $
              Just $
                Right
                  Quiz
                    { quizId = quizId
                    , identifier = quizToIdentifier quiz
                    , rounds = domainRounds
                    , teams = domainTeams
                    , scoreBoard = scoreBoard
                    }

updateScore :: Pool SqlBackend -> QuizId -> UpdateScoreRequest -> Handler ScoreBoard
updateScore pool quizId request = do
  result <- runDb pool statement
  case result of
    Nothing            -> throwError err404
    Just (Left ())     -> throwError err409
    Just (Right board) -> pure board
 where
  statement = do
    let dbQuizId = quizIdToKey quizId
    maybeQuiz <- get dbQuizId
    case maybeQuiz of
      Nothing -> pure Nothing
      Just quiz
        | not (Db.quizActive quiz) -> pure $ Just (Left ())
        | otherwise -> do
            -- Upsert the score
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
            let board =
                  ScoreBoard $
                    Map.fromList
                      [ ((TeamNumber (Db.teamRoundScoreTeamNumber score), RoundNumber (Db.teamRoundScoreRoundNumber score)), Points (Db.teamRoundScorePoints score))
                      | Entity _ score <- scoreEntities
                      ]
            pure $ Just (Right board)

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
