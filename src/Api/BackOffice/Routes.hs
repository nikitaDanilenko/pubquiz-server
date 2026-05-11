{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Api.BackOffice.Routes where

import           Api.BackOffice.Types        (AuthenticatedUser (..),
                                              CorrectScoreCommand (..),
                                              QuizMetaData (..),
                                              RecordRoundScoresCommand (..),
                                              RenameTeamCommand (..),
                                              SetTeamActiveCommand (..),
                                              TeamScore (..))
import           Api.FromDb                  (dbToScoreBoard, quizKeyToId,
                                              quizToIdentifier)
import           Api.ToDb                    (identifierToQuiz, quizIdToKey,
                                              roundToDb, teamRoundScoreToDb,
                                              teamToDb)
import           Api.Types                   (NumberOfQuestions (..),
                                              Place (..), Points (..),
                                              Quiz (..), QuizId (..),
                                              QuizIdentifier (..),
                                              QuizName (..), QuizSettings (..),
                                              QuizSummary (..),
                                              Round (..), RoundNumber (..),
                                              ScoreBoard (..),
                                              Team (..), TeamName (..),
                                              TeamNumber (..))
import           Control.Monad               (forM, forM_, unless)
import           Data.List                   (nub, sortOn)
import           Data.Maybe                  (isJust)
import           Data.Pool                   (Pool)
import           Database.Persist            (Entity (..), (=.), (==.))
import           Database.Persist.Postgresql (get, insert, selectList, update,
                                              upsert)
import           Database.Persist.Sql        (SqlBackend, SqlPersistT)
import qualified Db.Schema                   as Db
import           Db.Util                     (runDb)
import           Servant
import           Servant.Auth.Server

-- Workaround: "Post '[JSON] NoContent" is returns a 200 code, but we want 204.
-- API-wise this solution is already good enough, but the OpenAPI specification
-- will still create a JSON response with an empty body, and no schema.
-- OpenAPI generators may not handle this correctly,
-- so we will address the issue by manually editing the generated OpenAPI specification.
type Post204 = Verb 'POST 204 '[JSON] NoContent

-- Most bodies are empty, because these are the domain actions.
type BackOfficeRoutes =
  "whoami" :> Get '[JSON] AuthenticatedUser
    :<|> ReqBody '[JSON] QuizMetaData :> Post '[JSON] Quiz
    :<|> Capture "quizId" QuizId :> "change-settings" :> ReqBody '[JSON] QuizMetaData :> Post204
    :<|> Capture "quizId" QuizId :> "record-round-scores" :> ReqBody '[JSON] RecordRoundScoresCommand :> Post204
    :<|> Capture "quizId" QuizId :> "correct-score" :> ReqBody '[JSON] CorrectScoreCommand :> Post204
    :<|> Capture "quizId" QuizId :> "rename-team" :> ReqBody '[JSON] RenameTeamCommand :> Post204
    :<|> Capture "quizId" QuizId :> "set-team-active" :> ReqBody '[JSON] SetTeamActiveCommand :> Post204
    :<|> Capture "quizId" QuizId :> "lock" :> Post204
    :<|> Capture "quizId" QuizId :> "unlock" :> Post204

type BackOfficeApi = "backoffice" :> Auth '[Cookie, JWT] AuthenticatedUser :> BackOfficeRoutes

backOfficeApi :: Proxy BackOfficeApi
backOfficeApi = Proxy

-- Handlers

backOfficeServer :: Pool SqlBackend -> AuthResult AuthenticatedUser -> Server BackOfficeRoutes
backOfficeServer pool (Authenticated user) =
  whoami user
    :<|> createQuiz pool
    :<|> changeSettings pool
    :<|> recordRoundScores pool
    :<|> correctScore pool
    :<|> renameTeam pool
    :<|> setTeamActive pool
    :<|> lockQuiz pool
    :<|> unlockQuiz pool user
backOfficeServer _ _ = throwAll err401

whoami :: AuthenticatedUser -> Handler AuthenticatedUser
whoami = pure

createQuiz :: Pool SqlBackend -> QuizMetaData -> Handler Quiz
createQuiz pool request = do
  quizKey <- runDb pool statement
  let qpr = request.settings.questionsPerRound
      initialTeams = [ Team (TeamNumber n) (TeamName "") True | n <- [1 .. request.settings.numberOfTeams] ]
      initialRounds = [ Round (RoundNumber n) (Points (fromIntegral (unNumberOfQuestions q))) q
                      | (n, q) <- zip [1 ..] qpr ]
  pure $
    Quiz
      { summary = QuizSummary
          { quizId = quizKeyToId quizKey
          , identifier = request.identifier
          , active = True
          }
      , rounds = initialRounds
      , scoreBoard = ScoreBoard { teams = initialTeams, scores = [] }
      }
 where
  statement = do
    quizId <- insert $ identifierToQuiz request.identifier
    forM_ [1 .. request.settings.numberOfTeams] $ \num ->
      insert $ teamToDb quizId Team
        { number = TeamNumber num
        , name = TeamName ""
        , active = True
        }
    forM_ (zip [1 ..] request.settings.questionsPerRound) $ \(num, nq) ->
      insert $ roundToDb quizId num nq
    pure quizId

data CommandResult = QuizNotFound | QuizLocked | CommandSuccess

-- Helper for command handlers that require an active quiz
withActiveQuiz :: Pool SqlBackend -> QuizId -> SqlPersistT IO CommandResult -> Handler NoContent
withActiveQuiz pool quizId action = do
  result <- runDb pool $ do
    maybeQuiz <- get (quizIdToKey quizId)
    case maybeQuiz of
      Nothing -> pure QuizNotFound
      Just quiz
        | not (Db.quizActive quiz) -> pure QuizLocked
        | otherwise -> action
  case result of
    QuizNotFound   -> throwError err404
    QuizLocked     -> throwError err409
    CommandSuccess -> pure NoContent

changeSettings :: Pool SqlBackend -> QuizId -> QuizMetaData -> Handler NoContent
changeSettings pool quizId cmd = withActiveQuiz pool quizId $ do
  let dbQuizId = quizIdToKey quizId

  update dbQuizId
    [ Db.QuizName =. unQuizName cmd.identifier.name
    , Db.QuizPlace =. unPlace cmd.identifier.place
    , Db.QuizDate =. cmd.identifier.date
    ]

  teamEntities <- selectList [Db.TeamQuizId ==. dbQuizId] []
  let maxTeamInDb = if null teamEntities then 0 else maximum $ map (Db.teamNumber . entityVal) teamEntities
      newTeamCount = cmd.settings.numberOfTeams
  forM_ [maxTeamInDb + 1 .. newTeamCount] $ \num ->
    insert $ teamToDb dbQuizId Team
      { number = TeamNumber num
      , name = TeamName ""
      , active = True
      }
  scoreEntities <- selectList [Db.TeamRoundScoreQuizId ==. dbQuizId] []
  let roundsWithScores = nub $ map (Db.teamRoundScoreRoundNumber . entityVal) scoreEntities
  forM_ [maxTeamInDb + 1 .. newTeamCount] $ \teamNum ->
    forM_ roundsWithScores $ \roundNum ->
      insert $ Db.TeamRoundScore
        { teamRoundScoreQuizId = dbQuizId
        , teamRoundScoreTeamNumber = teamNum
        , teamRoundScoreRoundNumber = roundNum
        , teamRoundScorePoints = 0
        }

  roundEntities <- selectList [Db.RoundQuizId ==. dbQuizId] []
  let sortedRounds = sortOn (Db.roundNumber . entityVal) roundEntities
      existingCount = length sortedRounds
      newRounds = cmd.settings.questionsPerRound
  forM_ (zip sortedRounds newRounds) $ \(roundEntity, nq) ->
    update (entityKey roundEntity)
      [Db.RoundNumberOfQuestions =. fromIntegral (unNumberOfQuestions nq)]
  forM_ (zip [existingCount + 1 ..] (drop existingCount newRounds)) $ \(num, nq) ->
    insert $ roundToDb dbQuizId num nq

  pure CommandSuccess

recordRoundScores :: Pool SqlBackend -> QuizId -> RecordRoundScoresCommand -> Handler NoContent
recordRoundScores pool quizId cmd = withActiveQuiz pool quizId $ do
  let dbQuizId = quizIdToKey quizId
  forM_ cmd.scores $ \teamScore ->
    insert $ teamRoundScoreToDb dbQuizId teamScore.teamNumber cmd.roundNumber teamScore.points
  pure CommandSuccess

correctScore :: Pool SqlBackend -> QuizId -> CorrectScoreCommand -> Handler NoContent
correctScore pool quizId cmd = withActiveQuiz pool quizId $ do
  let dbQuizId = quizIdToKey quizId
  upsert
    (teamRoundScoreToDb dbQuizId cmd.teamNumber cmd.roundNumber cmd.points)
    [Db.TeamRoundScorePoints =. unPoints cmd.points]
  pure CommandSuccess

renameTeam :: Pool SqlBackend -> QuizId -> RenameTeamCommand -> Handler NoContent
renameTeam pool quizId cmd = withActiveQuiz pool quizId $ do
  update (Db.TeamKey (quizIdToKey quizId) (unTeamNumber cmd.teamNumber))
    [Db.TeamName =. unTeamName cmd.newName]
  pure CommandSuccess

setTeamActive :: Pool SqlBackend -> QuizId -> SetTeamActiveCommand -> Handler NoContent
setTeamActive pool quizId cmd = withActiveQuiz pool quizId $ do
  update (Db.TeamKey (quizIdToKey quizId) (unTeamNumber cmd.teamNumber))
    [Db.TeamActive =. cmd.active]
  pure CommandSuccess

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
