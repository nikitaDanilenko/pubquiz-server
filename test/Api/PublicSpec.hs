{-# LANGUAGE OverloadedStrings #-}

module Api.PublicSpec (spec) where

import           Api.Public.Routes (PublicApi, publicServer)
import           Network.Wai       (Application)
import           Network.Wai.Test  (SResponse (..), defaultRequest, request,
                                    runSession, setPath)
import           Servant           (Proxy (..), serve)
import           Test.Database     (TestDb (..), withTestDatabase)
import           Test.Hspec

mkApp :: TestDb -> Application
mkApp testDb = serve (Proxy :: Proxy PublicApi) (publicServer (testDbPool testDb))

spec :: Spec
spec = around withTestDatabase $
  describe "GET /public" $
    it "returns an empty list when no quizzes exist" $ \testDb -> do
      response <- runSession (request $ setPath defaultRequest "/public") (mkApp testDb)
      simpleStatus response `shouldBe` toEnum 200
      simpleBody response `shouldBe` "[]"
