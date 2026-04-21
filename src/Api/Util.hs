module Api.Util where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT)
import Data.Pool (Pool)
import Database.Persist.Sql (SqlBackend, runSqlPool)
import Servant (Handler)

runDb :: Pool SqlBackend -> ReaderT SqlBackend IO a -> Handler a
runDb pool action = liftIO $ runSqlPool action pool