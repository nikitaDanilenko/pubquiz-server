let DatabaseConfig =
      { host : Text
      , name : Text
      , user : Text
      , password : Text
      , port : Natural
      }

let Organizer =
      { name : Text
      , passwordHash : Text
      , isAdmin : Bool
      }

let JwtConfig =
      { secret : Text
      , expirationSeconds : Natural
      }

let Config =
      { port : Natural
      , database : DatabaseConfig
      , organizers : List Organizer
      , jwt : JwtConfig
      }

let config : Config =
      { port = env:PORT
      , database =
          { host = env:DATABASE_HOST as Text
          , name = env:DATABASE_NAME as Text
          , user = env:DATABASE_USER as Text
          , password = env:DATABASE_PASSWORD as Text
          , port = env:DATABASE_PORT
          }
      , organizers =
          [ { name = env:ORGANIZER_ADMIN_NAME as Text
            , passwordHash = env:ORGANIZER_ADMIN_HASH as Text
            , isAdmin = True
            }
          , { name = env:ORGANIZER_USER_NAME as Text
            , passwordHash = env:ORGANIZER_USER_HASH as Text
            , isAdmin = False
            }
          ]
      , jwt =
          { secret = env:JWT_SECRET as Text
          , expirationSeconds = env:JWT_EXPIRATION_SECONDS
          }
      }

in  config
