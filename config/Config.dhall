let DatabaseConfig =
      { host : Text
      , name : Text
      , user : Text
      , password : Text
      , port : Natural
      }

let Config =
      { serverPath : Text
      , database : DatabaseConfig
      }

let config : Config =
      { serverPath = env:SERVER_PATH as Text ? "http://localhost:8080"
      , database =
          { host = env:DATABASE_HOST as Text ? "localhost"
          , name = env:DATABASE_NAME as Text ? "pubquiz"
          , user = env:DATABASE_USER as Text ? "pubquiz"
          , password = env:DATABASE_PASSWORD as Text ? "pubquiz"
          , port = env:DATABASE_PORT ? 5432
          }
      }

in  config
