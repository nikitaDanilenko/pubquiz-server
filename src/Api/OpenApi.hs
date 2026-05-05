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

import           Api.Auth                   (AuthApi, LoginRequest)
import           Api.BackOffice.Routes      (BackOfficeApi)
import           Api.BackOffice.Types       (AddTeamsCommand,
                                             AuthenticatedUser (..),
                                             ChangeSettingsCommand,
                                             CorrectScoreCommand, QuizMetaData,
                                             RecordRoundScoresCommand,
                                             RenameTeamCommand,
                                             SetTeamActiveCommand, TeamScore)
import           Api.Public.Routes          (PublicApi)
import           Api.Types                  (NumberOfQuestions, Place, Points,
                                             Quiz, QuizId, QuizIdentifier,
                                             QuizName, QuizSettings,
                                             QuizState (..), QuizSummary, Round,
                                             RoundNumber, ScoreBoard,
                                             ScoreEntry, SomeQuiz (..), Team,
                                             TeamName, TeamNumber)
import           Data.Aeson                 (ToJSON (..), Value (..))
import qualified Data.Aeson.Key             as Key
import qualified Data.Aeson.KeyMap          as KM
import           Data.Function              ((&))
import           Data.Functor.Const         (Const (..))
import           Data.Functor.Identity      (Identity (..))
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import           Data.OpenApi               (HasComponents (..), HasSchema (..),
                                             HasSchemas (..), HasSecurity (..),
                                             OpenApi, SecurityRequirement (..),
                                             SecurityScheme (..),
                                             SecuritySchemeType (..),
                                             ToParamSchema (..), ToSchema,
                                             declareNamedSchema,
                                             declareSchemaRef,
                                             genericDeclareNamedSchema)
import qualified Data.OpenApi               as OpenApi
import           Data.Proxy                 (Proxy (..))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           GHC.Generics               (Generic)
import           Numeric.Natural            (Natural)
import           Servant                    (Get, Handler, JSON, Server, (:<|>),
                                             (:>))
import           Servant.Auth               (Auth, JWT)
import           Servant.OpenApi            (HasOpenApi (..), toOpenApi)

-- HasOpenApi instance for Auth - just delegate to the underlying API
-- (Auth doesn't change the API structure, it just adds authentication)
instance HasOpenApi api => HasOpenApi (Auth auths user :> api) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy api)

-- Serve the OpenAPI spec as a raw JSON Value so we can post-process it
type OpenApiApi = "openapi.json" :> Get '[JSON] Value

openApiServer :: Server OpenApiApi
openApiServer = pure $ cleanMediaTypes $ clean204Responses $ toJSON openApiSpec

-- Combined API type for schema generation (excluding OpenAPI endpoint itself)
type DocumentedApi = PublicApi :<|> BackOfficeApi :<|> AuthApi

-- Generate the OpenAPI spec
openApiSpec :: OpenApi
openApiSpec =
  toOpenApi (Proxy :: Proxy DocumentedApi)
    & set (OpenApi.info . OpenApi.title) "PubQuiz API"
    & set (OpenApi.info . OpenApi.version) "1.0"
    & set (OpenApi.components . OpenApi.securitySchemes) securitySchemes
    & over OpenApi.paths addSecurityToBackOfficePaths

securitySchemes :: OpenApi.SecurityDefinitions
securitySchemes = OpenApi.SecurityDefinitions $ InsOrd.fromList
  [("cookieAuth", SecurityScheme (SecuritySchemeApiKey (OpenApi.ApiKeyParams "JWT-Cookie" OpenApi.ApiKeyCookie)) Nothing)]

cookieAuthSecurity :: [SecurityRequirement]
cookieAuthSecurity = [SecurityRequirement $ InsOrd.singleton "cookieAuth" []]

addSecurityToBackOfficePaths :: InsOrd.InsOrdHashMap FilePath OpenApi.PathItem -> InsOrd.InsOrdHashMap FilePath OpenApi.PathItem
addSecurityToBackOfficePaths = InsOrd.mapWithKey addSecurityIfBackOffice
  where
    addSecurityIfBackOffice :: FilePath -> OpenApi.PathItem -> OpenApi.PathItem
    addSecurityIfBackOffice path item
      | "/backoffice" `T.isPrefixOf` T.pack path && "/backoffice/login" /= path =
          item { OpenApi._pathItemPost = fmap addSecurity (OpenApi._pathItemPost item)
               , OpenApi._pathItemGet = fmap addSecurity (OpenApi._pathItemGet item)
               , OpenApi._pathItemPut = fmap addSecurity (OpenApi._pathItemPut item)
               , OpenApi._pathItemDelete = fmap addSecurity (OpenApi._pathItemDelete item)
               , OpenApi._pathItemPatch = fmap addSecurity (OpenApi._pathItemPatch item)
               }
      | otherwise = item

    addSecurity :: OpenApi.Operation -> OpenApi.Operation
    addSecurity = set OpenApi.security cookieAuthSecurity

-- Lens getter
(^.) :: s -> ((a -> Const a a) -> s -> Const a s) -> a
s ^. l = getConst (l Const s)

-- Lens over
over :: ((a -> Identity a) -> s -> Identity s) -> (a -> a) -> s -> s
over l f s = runIdentity (l (Identity . f) s)

-- Simple lens setter (avoids full lens dependency)
set :: ((a -> Identity a) -> s -> Identity s) -> a -> s -> s
set l a s = runIdentity (l (const (Identity a)) s)

-- Remove charset suffix from media type keys in the OpenAPI spec
cleanMediaTypes :: Value -> Value
cleanMediaTypes (Object obj) = Object $ KM.mapKeyVal cleanKey cleanMediaTypes obj
  where
    cleanKey k
      | Key.toText k == "application/json;charset=utf-8" = Key.fromText "application/json"
      | otherwise = k
cleanMediaTypes (Array arr) = Array $ fmap cleanMediaTypes arr
cleanMediaTypes v = v

-- Remove content from 204 responses: NoContent should have no body
clean204Responses :: Value -> Value
clean204Responses (Object obj) =
  case KM.lookup "204" obj of
    Just (Object response) ->
      let cleanedResponse = Object $ KM.delete "content" response
      in Object $ KM.insert "204" cleanedResponse $ KM.mapKeyVal id clean204Responses obj
    _ -> Object $ KM.mapKeyVal id clean204Responses obj
clean204Responses (Array arr) = Array $ fmap clean204Responses arr
clean204Responses v = v

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
