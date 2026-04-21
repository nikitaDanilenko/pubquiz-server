{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Api.BackOffice.Routes where

import           Api.BackOffice.Types        (QuizSummary)
import           Api.Util                    (runDb)
import           Control.Monad               (forM_)
import           Core.Domain                 (Place (..), Points, Quiz (..),
                                              QuizId (..), QuizIdentifier (..),
                                              QuizName (..), QuizSettings (..),
                                              QuizState (..), RoundNumber,
                                              ScoreBoard (..), SomeQuiz,
                                              Team (..), TeamName (..),
                                              TeamNumber (..))
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Pool                   (Pool)
import           Data.Text                   (Text)
import           Database.Persist.Postgresql (insert)
import           Database.Persist.Sql        (SqlBackend, fromSqlKey)
import qualified Db.Schema                   as Db (Quiz (..), QuizId (..),
                                                    Round (..), Team (..))
import           GHC.Generics                (Generic)
import           Servant
import           Servant.Auth.Server

-- JWT payload: authenticated organizer
data AuthenticatedUser = AuthenticatedUser
  { organizerName :: Text
  , isAdmin       :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuthenticatedUser

instance FromJSON AuthenticatedUser

instance ToJWT AuthenticatedUser

instance FromJWT AuthenticatedUser

data CreateQuizRequest = CreateQuizRequest
  { identifier :: QuizIdentifier
  , settings   :: QuizSettings
  }
  deriving (Show, Eq, Generic)

instance FromJSON CreateQuizRequest

newtype UpdateQuizSettingsRequest = UpdateQuizSettingsRequest
  { settings :: QuizSettings
  }
  deriving (Show, Eq, Generic)

instance FromJSON UpdateQuizSettingsRequest

data UpdateScoreRequest = UpdateScoreRequest
  { teamNumber  :: TeamNumber
  , roundNumber :: RoundNumber
  , points      :: Points
  }
  deriving (Show, Eq, Generic)

instance FromJSON UpdateScoreRequest

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
    :<|> Capture "quizId" QuizId :> "lock" :> Post '[JSON] (Quiz 'Locked)
    :<|> Capture "quizId" QuizId :> "unlock" :> Post '[JSON] (Quiz 'Active)

-- Wrap routes with JWT authentication
type BackOfficeApi = "backoffice" :> Auth '[JWT] AuthenticatedUser :> BackOfficeRoutes

backOfficeApi :: Proxy BackOfficeApi
backOfficeApi = Proxy

-- Handlers (placeholder implementations)

backOfficeServer :: Pool SqlBackend -> AuthResult AuthenticatedUser -> Server BackOfficeRoutes
backOfficeServer pool (Authenticated user) =
  listQuizzes user
    :<|> createQuiz pool user
    :<|> getQuiz user
    :<|> updateSettings user
    :<|> updateScore user
    :<|> lockQuiz user
    :<|> unlockQuiz user
backOfficeServer _ _ = throwAll err401

listQuizzes :: AuthenticatedUser -> Handler [QuizSummary]
listQuizzes = undefined

createQuiz :: Pool SqlBackend -> AuthenticatedUser -> CreateQuizRequest -> Handler (Quiz 'Active)
createQuiz pool _user request = do
  quizId <- runDb pool statement
  pure $
    Quiz
    -- todo: The conversion should be extracted.
      { quizId = QuizId (fromIntegral $ fromSqlKey quizId)
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

getQuiz :: AuthenticatedUser -> QuizId -> Handler SomeQuiz
getQuiz = undefined

updateSettings :: AuthenticatedUser -> QuizId -> UpdateQuizSettingsRequest -> Handler (Quiz 'Active)
updateSettings = undefined

updateScore :: AuthenticatedUser -> QuizId -> UpdateScoreRequest -> Handler ScoreBoard
updateScore = undefined

lockQuiz :: AuthenticatedUser -> QuizId -> Handler (Quiz 'Locked)
lockQuiz = undefined

unlockQuiz :: AuthenticatedUser -> QuizId -> Handler (Quiz 'Active)
unlockQuiz = undefined
