{-# LANGUAGE OverloadedStrings #-}

module Test.Database
  ( withTestDatabase
  , TestDb (..)
  ) where

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Logger        (runNoLoggingT)
import           Data.Function               ((&))
import           Data.Pool                   (Pool)
import           Data.Text                   (pack)
import           Data.Text.Encoding          (encodeUtf8)
import qualified Data.Text.Lazy              as TL
import           Database.Persist.Postgresql (createPostgresqlPool, runSqlPool)
import           Database.Persist.Sql        (SqlBackend, runMigrationSilent)
import           Db.Schema                   (migrateAll)
import qualified TestContainers              as TC
import           TestContainers.Hspec        (withContainers)

newtype TestDb = TestDb
  { testDbPool :: Pool SqlBackend
  }

postgresContainer :: TC.TestContainer (Pool SqlBackend)
postgresContainer = do
  let containerRequest =
        TC.containerRequest (TC.fromTag "postgres:16")
          & TC.setEnv [("POSTGRES_PASSWORD", "test"), ("POSTGRES_DB", "testdb")]
          & TC.setExpose [5432]
          & TC.setWaitingFor
              ( TC.waitUntilTimeout 60 $
                  TC.waitForLogLine TC.Stderr ("database system is ready to accept connections" `TL.isInfixOf`)
              )

  container <- TC.run containerRequest
  let port = TC.containerPort container 5432
      connStr = mconcat
        [ "host=localhost"
        , " dbname=testdb"
        , " user=postgres"
        , " password=test"
        , " port=", show port
        ]

  pool <- liftIO $ runNoLoggingT $ createPostgresqlPool (encodeUtf8 $ pack connStr) 1

  liftIO $ runNoLoggingT $ runSqlPool (runMigrationSilent migrateAll) pool

  pure pool

withTestDatabase :: (TestDb -> IO ()) -> IO ()
withTestDatabase action = withContainers postgresContainer $ \pool ->
  action (TestDb pool)
