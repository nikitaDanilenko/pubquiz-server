module Db.Configuration where

import           Constants             (configMap, databaseHost, databaseName,
                                        databasePassword, databasePort,
                                        databaseUser)
import qualified Data.ByteString.Char8 as B (ByteString, pack)
import           Data.Map.Lazy         ((!?))

data Configuration =
  Configuration
    { host     :: String
    , name     :: String
    , user     :: String
    , password :: String
    , port     :: String
    }

toConnection :: Configuration -> B.ByteString
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
  let positions = [databaseHost, databaseName, databaseUser, databasePassword, databasePort]
      parameters = traverse (settings !?) positions
      config =
        case parameters of
          Just (h:n:u:pw:p:_) -> Configuration h n u pw p
          _ -> error ("Database connection not fully specified:" ++ show (map (settings !?) positions)) 
  pure config
