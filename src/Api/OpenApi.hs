{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Api.OpenApi
  ( OpenApiApi
  , openApiServer
  ) where

import           Api.Auth              (AuthApi, LoginRequest,
                                        LoginResponse (..))
import           Api.BackOffice.Routes (BackOfficeApi)
import           Api.BackOffice.Types  (AddTeamsCommand, AuthenticatedUser (..),
                                        ChangeSettingsCommand,
                                        CorrectScoreCommand, QuizMetaData,
                                        RecordRoundScoresCommand,
                                        RenameTeamCommand, SetTeamActiveCommand,
                                        TeamScore)
import           Api.Public.Routes     (PublicApi)
import           Api.Types             (NumberOfQuestions, Place, Points, Quiz,
                                        QuizId, QuizIdentifier, QuizName,
                                        QuizSettings, QuizState (..),
                                        QuizSummary, Round, RoundNumber,
                                        ScoreBoard, ScoreEntry, SomeQuiz (..),
                                        Team, TeamName, TeamNumber)
import           Data.Aeson            (ToJSON (..))
import           Data.Function         ((&))
import           Data.Functor.Identity (Identity (..))
import           Data.OpenApi          (HasComponents (..), HasSchema (..),
                                        HasSchemas (..), OpenApi,
                                        ToParamSchema (..), ToSchema,
                                        declareNamedSchema, declareSchemaRef,
                                        genericDeclareNamedSchema)
import qualified Data.OpenApi          as OpenApi
import           Data.Proxy            (Proxy (..))
import           Data.Text             (Text)
import           GHC.Generics          (Generic)
import           Numeric.Natural       (Natural)
import           Servant               (Get, Handler, JSON, Server, (:<|>),
                                        (:>))
import           Servant.Auth          (Auth, JWT)
import           Servant.OpenApi       (HasOpenApi (..), toOpenApi)

-- HasOpenApi instance for Auth - just delegate to the underlying API
-- (Auth doesn't change the API structure, it just adds authentication)
instance HasOpenApi api => HasOpenApi (Auth auths user :> api) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy api)

type OpenApiApi = "openapi.json" :> Get '[JSON] OpenApi

openApiServer :: Server OpenApiApi
openApiServer = pure openApiSpec

-- Combined API type for schema generation (excluding OpenAPI endpoint itself)
type DocumentedApi = PublicApi :<|> BackOfficeApi :<|> AuthApi

-- Generate the OpenAPI spec
openApiSpec :: OpenApi
openApiSpec =
  toOpenApi (Proxy :: Proxy DocumentedApi)
    & set (OpenApi.info . OpenApi.title) "PubQuiz API"
    & set (OpenApi.info . OpenApi.version) "1.0"

-- Simple lens setter (avoids full lens dependency)
set :: ((a -> Identity a) -> s -> Identity s) -> a -> s -> s
set l a s = runIdentity (l (const (Identity a)) s)

-- ToSchema instances for newtypes (delegate to inner type's schema)
instance ToSchema QuizName where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

instance ToSchema Place where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

instance ToSchema TeamName where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

instance ToSchema RoundNumber where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int)

instance ToSchema Points where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Double)

instance ToSchema QuizId where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int)

instance ToParamSchema QuizId where
  toParamSchema _ = toParamSchema (Proxy :: Proxy Int)

instance ToSchema TeamNumber where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int)

instance ToSchema NumberOfQuestions where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Natural)

-- ToSchema instances for data types
instance ToSchema QuizIdentifier
instance ToSchema QuizSettings
instance ToSchema Round
instance ToSchema Team
instance ToSchema QuizSummary
instance ToSchema ScoreEntry

-- ScoreBoard now uses a list of ScoreEntry, which serializes naturally.
instance ToSchema ScoreBoard where
  declareNamedSchema = genericDeclareNamedSchema OpenApi.defaultSchemaOptions

-- Quiz has a phantom type parameter, but the JSON is the same regardless.
-- We define instances for the specific states.
instance ToSchema (Quiz 'Active) where
  declareNamedSchema = genericDeclareNamedSchema OpenApi.defaultSchemaOptions

instance ToSchema (Quiz 'Locked) where
  declareNamedSchema = genericDeclareNamedSchema OpenApi.defaultSchemaOptions

-- SomeQuiz is a GADT that wraps Quiz - schema is the same as Quiz
instance ToSchema SomeQuiz where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy (Quiz 'Active))

-- Auth types
instance ToSchema LoginRequest
instance ToSchema LoginResponse
instance ToSchema AuthenticatedUser

-- BackOffice command types
instance ToSchema QuizMetaData
instance ToSchema ChangeSettingsCommand
instance ToSchema AddTeamsCommand
instance ToSchema TeamScore
instance ToSchema RecordRoundScoresCommand
instance ToSchema CorrectScoreCommand
instance ToSchema RenameTeamCommand
instance ToSchema SetTeamActiveCommand
