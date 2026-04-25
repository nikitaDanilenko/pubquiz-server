module Api.Util where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (ReaderT)
import           Data.Pool              (Pool)
import           Database.Persist.Sql   (SqlBackend, runSqlPool)

runDb :: MonadIO m => Pool SqlBackend -> ReaderT SqlBackend IO a -> m a
runDb pool action = liftIO $ runSqlPool action pool
