{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Api.PublicSpec (spec) where

import           Api.FromDb                  (dbToQuizSummary)
import           Api.Public.Routes           (PublicApi, publicServer)
import           Api.Types                   (Points (..), Quiz (..),
                                              QuizId (..), QuizIdentifier,
                                              QuizSummary, Round (..))
import           Api.Types                   as Api
import           Control.Applicative         ((<*>))
import           Control.Monad.Logger        (runNoLoggingT)
import           Data.Aeson                  (FromJSON (parseJSON),
                                              eitherDecode, withObject, (.:))
import           Data.List                   (sortOn)
import           Data.Time.Calendar          (Day, fromGregorian)
import           Database.Persist            (Entity (..), insert_)
import           Database.Persist.Postgresql (runSqlPool, toSqlKey)
import qualified Db.Schema                   as Db
import           GHC.Generics                (Generic)
import           Network.Wai                 (Application)
import           Network.Wai.Test            (SResponse (..), defaultRequest,
                                              request, runSession, setPath)
import           Numeric.Natural             (Natural)
import           Servant                     (Proxy (..), serve)
import           Test.Database               (TestDb (..), withTestDatabase)
import           Test.Hspec

-- Possibly simplified counterparts of Api.Types, but with a decoder instead of an encoder.

data QuizSummaryResponse = QuizSummaryResponse
  { quizId     :: Int
  , identifier :: QuizIdentifier
  , active     :: Bool
  }
  deriving (Show, Eq, Generic, FromJSON)

data RoundResponse = RoundResponse
  { number            :: Int
  , displayMaxPoints  :: Double
  , numberOfQuestions :: Natural
  }
  deriving (Show, Eq, Generic, FromJSON)

data QuizResponse = QuizResponse
  { quizId     :: Int
  , identifier :: QuizIdentifier
  , rounds     :: [RoundResponse]
  }
  deriving (Show, Eq, Generic, FromJSON)

toResponseSummary :: QuizSummary -> QuizSummaryResponse
toResponseSummary (QuizSummary qid ident act) = QuizSummaryResponse
  { quizId = unQuizId qid
  , identifier = ident
  , active = act
  }

toResponseRound :: Round -> RoundResponse
toResponseRound round = RoundResponse
  { number = unRoundNumber round.number
  , displayMaxPoints = unPoints round.displayMaxPoints
  , numberOfQuestions = unNumberOfQuestions round.numberOfQuestions
  }

toResponseQuiz :: QuizSummary -> QuizResponse
toResponseQuiz (QuizSummary quizId identifier _) = QuizResponse
  { quizId = unQuizId quizId
  , identifier = identifier
  -- only for testing
  , rounds = []
  }

mkApp :: TestDb -> Application
mkApp testDb = serve (Proxy :: Proxy PublicApi) (publicServer (testDbPool testDb))

insertQuiz :: TestDb -> Entity Db.Quiz -> IO ()
insertQuiz testDb (Entity _ quiz) = runNoLoggingT $ runSqlPool (insert_ quiz) (testDbPool testDb)

sampleActiveQuiz :: Entity Db.Quiz
sampleActiveQuiz = Entity (toSqlKey 1) Db.Quiz
  { Db.quizPlace = "Test Venue"
  , Db.quizDate = fromGregorian 2345 6 7
  , Db.quizName = "Sample Quiz"
  , Db.quizActive = True
  }

sampleInactiveQuiz :: Entity Db.Quiz
sampleInactiveQuiz = Entity (toSqlKey 2) Db.Quiz
  { Db.quizPlace = "Another Place"
  , Db.quizDate = fromGregorian 2020 1 1
  , Db.quizName = "Old Quiz"
  , Db.quizActive = False
  }

spec :: Spec
spec = around withTestDatabase $ do
  describe "GET /public" $ do
    it "returns an empty list when no quizzes exist" $ \testDb -> do
      response <- runSession (request $ setPath defaultRequest "/public") (mkApp testDb)
      simpleStatus response `shouldBe` toEnum 200
      eitherDecode (simpleBody response) `shouldBe` Right ([] :: [QuizSummaryResponse])

    it "returns quizzes when they exist" $ \testDb -> do
      insertQuiz testDb sampleActiveQuiz
      insertQuiz testDb sampleInactiveQuiz
      response <- runSession (request $ setPath defaultRequest "/public") (mkApp testDb)
      simpleStatus response `shouldBe` toEnum 200
      case eitherDecode (simpleBody response) :: Either String [QuizSummaryResponse] of
        Right quizzes -> sortOn (\q -> q.quizId) quizzes `shouldBe` sortOn (\q -> q.quizId) (map (toResponseSummary . dbToQuizSummary) [sampleActiveQuiz, sampleInactiveQuiz])
        other -> expectationFailure $ "Expected two quiz summaries, got: " <> show other

  describe "GET /public/:id" $ do
    it "returns 404 for non-existent quiz" $ \testDb -> do
      response <- runSession (request $ setPath defaultRequest "/public/999") (mkApp testDb)
      simpleStatus response `shouldBe` toEnum 404

    it "returns quiz details when quiz exists" $ \testDb -> do
      insertQuiz testDb sampleActiveQuiz
      response <- runSession (request $ setPath defaultRequest "/public/1") (mkApp testDb)
      simpleStatus response `shouldBe` toEnum 200
      case eitherDecode (simpleBody response) :: Either String QuizResponse of
        Right quiz -> quiz `shouldBe` toResponseQuiz (dbToQuizSummary sampleActiveQuiz)
        Left err   -> expectationFailure $ "Failed to decode quiz: " <> err
