1. Installing with `stack`

   1. Run `stack install pubquiz-server --local-bin-path <path>`
   1. To install with `snap-server` running open SSL, run
      `stack install pubquiz-server --local-bin-path <path> --flag snap-server:openssl`
      (possibly, if this does not work, run `stack clean` and install `snap-server` with
       `stack build --flag snap-server:openssl`, but this may be an artifact of an earlier attempt)
   1. When running as a service, the working directory should contain the folder `log`,
      where `snap` will always automatically put access and error logs.
   1. After installation, there will be an executable file called `pubquiz-server-exe`,
      which can be called from the command line.
      This program needs the usual path and port parameters.

1. The following steps are required, but should be automated:
   1. Run `src/Db/Connection.performMigration`
   1. Add initial user(s) (this should be handled differently anyway)

1. The working directory (for the service configuration) should be the location of the
   `pubquiz-server` project or any other place, where there is a valid `config.txt`,
   which is required for accessing the database.


1. Installing the front end:
   1. Compile `elm make src/QuizInput.elm --output=input/input.js`
   1. Compile `elm make src/QuizOutput.elm --output=quizzes/output.js`
   1. Provide the `js` part of the Chart.js web component,
      i.e. install `npm install blissfully/elm-chartjs-webcomponent` anywhere,
      copy `node_modules/@blissfully` to a location of your choosing and
      set the path to said location in the `index.html` file in `quizzes`
      (to be more precise: in the target folder of the output compilation).