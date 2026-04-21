let DatabaseConfig =
      { host : Text
      , name : Text
      , user : Text
      , password : Text
      , port : Natural
      }

let Organizer =
      { organizerName : Text
      , passwordHash : Text
      , isAdmin : Bool
      }

let Config =
      { serverPath : Text
      , database : DatabaseConfig
      , organizers : List Organizer
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
      , organizers =
          [ { organizerName = env:ORGANIZER_ADMIN_NAME as Text ? "admin"
            , passwordHash = env:ORGANIZER_ADMIN_HASH as Text
            , isAdmin = True
            }
          , { organizerName = env:ORGANIZER_USER_NAME as Text ? "organizer"
            , passwordHash = env:ORGANIZER_USER_HASH as Text
            , isAdmin = False
            }
          ]
      }

in  config
