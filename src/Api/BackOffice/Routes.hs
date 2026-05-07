{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Api.BackOffice.Routes where

import           Api.BackOffice.Types        (AddTeamsCommand (..),
                                              AuthenticatedUser (..),
                                              ChangeSettingsCommand (..),
                                              CorrectScoreCommand (..),
                                              QuizMetaData (..),
                                              RecordRoundScoresCommand (..),
                                              RenameTeamCommand (..),
                                              SetTeamActiveCommand (..),
                                              TeamScore (..))
import           Api.FromDb                  (dbToScoreBoard, quizKeyToId,
                                              quizToIdentifier)
import           Api.ToDb                    (identifierToQuiz, quizIdToKey,
                                              teamRoundScoreToDb, teamToDb)
import           Api.Types                   (Place (..), Points (..),
                                              Quiz (..), QuizId (..),
                                              QuizIdentifier (..),
                                              QuizName (..), QuizSettings (..),
                                              QuizSummary (..),
                                              RoundNumber (..), ScoreBoard (..),
                                              Team (..), TeamName (..),
                                              TeamNumber (..))
import           Control.Monad               (forM, forM_, unless)
import           Data.List                   (nub)
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
    :<|> Capture "quizId" QuizId :> "change-settings" :> ReqBody '[JSON] ChangeSettingsCommand :> Post204
    :<|> Capture "quizId" QuizId :> "add-teams" :> ReqBody '[JSON] AddTeamsCommand :> Post204
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
    :<|> addTeams pool
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
  let initialTeams = [ Team (TeamNumber n) (TeamName "") True | n <- [1 .. request.settings.numberOfTeams] ]
  pure $
    Quiz
      { summary = QuizSummary
          { quizId = quizKeyToId quizKey
          , identifier = request.identifier
          , active = True
          }
      , rounds = []
      , scoreBoard = ScoreBoard { teams = initialTeams, scores = [] }
      }
 where
  statement = do
    -- Insert quiz record
    quizId <- insert $ identifierToQuiz request.identifier

    -- Insert default teams
    forM_ [1 .. request.settings.numberOfTeams] $ \num -> do
      insert $ teamToDb quizId Team
        { number = TeamNumber num
        , name = TeamName ""
        , active = True
        }

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

changeSettings :: Pool SqlBackend -> QuizId -> ChangeSettingsCommand -> Handler NoContent
changeSettings pool quizId cmd = withActiveQuiz pool quizId $ do
  update (quizIdToKey quizId)
    [ Db.QuizName =. unQuizName cmd.newIdentifier.name
    , Db.QuizPlace =. unPlace cmd.newIdentifier.place
    , Db.QuizDate =. cmd.newIdentifier.date
    ]
  pure CommandSuccess

addTeams :: Pool SqlBackend -> QuizId -> AddTeamsCommand -> Handler NoContent
addTeams pool quizId cmd = withActiveQuiz pool quizId $ do
  let dbQuizId = quizIdToKey quizId

  -- Find current max team number
  teamEntities <- selectList [Db.TeamQuizId ==. dbQuizId] []
  let maxTeamInDb = if null teamEntities
                    then 0
                    else maximum $ map (Db.teamNumber . entityVal) teamEntities

  -- Add new teams
  forM_ [maxTeamInDb + 1 .. maxTeamInDb + cmd.additionalTeams] $ \num ->
    insert $ teamToDb dbQuizId Team
      { number = TeamNumber num
      , name = TeamName ""
      , active = True
      }

  -- Add zero scores for new teams in rounds that already have scores
  scoreEntities <- selectList [Db.TeamRoundScoreQuizId ==. dbQuizId] []
  let roundsWithScores = nub $ map (Db.teamRoundScoreRoundNumber . entityVal) scoreEntities
  forM_ [maxTeamInDb + 1 .. maxTeamInDb + cmd.additionalTeams] $ \teamNum ->
    forM_ roundsWithScores $ \roundNum ->
      insert $ Db.TeamRoundScore
        { teamRoundScoreQuizId = dbQuizId
        , teamRoundScoreTeamNumber = teamNum
        , teamRoundScoreRoundNumber = roundNum
        , teamRoundScorePoints = 0
        }

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
