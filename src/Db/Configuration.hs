module Db.Configuration where

data Configuration = Configuration {
  host :: String,
  name :: String,
  user :: String,
  password :: String,
  port :: String
}

toConnection :: Configuration -> String
toConnection c =
  unwords
    (zipWith
       (\k v -> concat [k, "=", v])
       ["host", "dbname", "user", "password", "port"]
       [host c, name c, user c, password c, port c])