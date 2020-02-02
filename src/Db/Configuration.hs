module Db.Configuration where

import           Constants             (configMap, databaseHost, databaseName,
                                        databasePassword, databasePort,
                                        databaseUser)
import           Data.ByteString.Char8 as B (pack)
import           Data.Map.Lazy         ((!?))

data Configuration =
  Configuration
    { host     :: String
    , name     :: String
    , user     :: String
    , password :: String
    , port     :: String
    }

toConnection :: Configuration -> String
toConnection c =
  B.pack $
  unwords
    (zipWith
       (\k v -> concat [k, "=", v])
       ["host", "dbname", "user", "password", "port"]
       [host c, name c, user c, password c, port c])

readConfiguration :: IO Configuration
readConfiguration = do
  settings <- configMap
  let parameters = traverse (settings !?) [databaseHost, databaseName, databaseUser, databasePassword, databasePort]
      config =
        case parameters of
          Just (h:n:u:pw:p:_) -> Configuration h n u pw p
          _ -> error "Database connection not fully specified"
  pure config
