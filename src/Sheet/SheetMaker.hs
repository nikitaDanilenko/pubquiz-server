module Sheet.SheetMaker
  ( createSheetWith
  , safeRemoveFile
  , Ending
  ) where

import qualified Blaze.ByteString.Builder as Builder
import           Control.Exception        (catch)
import           Control.Exception.Base   (IOException)
import           Control.Monad            (void)
import qualified Data.ByteString.Char8    as B
import qualified Data.ByteString.Lazy     as L
import           Data.Text                (Text)
import qualified Data.Text                as T (Text, concat, intercalate, pack,
                                                unpack)
import qualified Data.Text.Encoding       as E
import qualified Data.Text.IO             as I (writeFile)
import           System.Directory         (getCurrentDirectory, removeFile,
                                           setCurrentDirectory)
import           System.Process           (callProcess)

import           Constants                (qrOnlyFileName, quizIdParam,
                                           sheetFileName, sheetsFolderIO,
                                           teamCodeParam, teamNumberParam,
                                           teamQueryParam)
import           Data.Aeson               (encode)
import           Data.List                (sortOn)
import           Data.Time.Calendar       (Day)
import           Db.Connection            (DbQuizId)
import           Db.DbConversion          (QuestionsInQuiz,
                                           TeamQuery (TeamQuery),
                                           mkPathForQuizSheetWith,
                                           questionsInRoundNumberOfQuestions,
                                           questionsInRoundRoundNumber,
                                           teamQueryQuizId, teamQueryTeamCode,
                                           teamQueryTeamNumber)
import           General.Types            (Code, TeamNumber, unwrap)
import           GHC.Natural              (Natural, naturalToInt)
import           Network.HTTP.Types       (encodePathSegments)
import           Sheet.Tex                (mkQROnly,
                                           mkSheetWithArbitraryQuestions)
import           Utils                    (encodePath, separatedFragment)

type ServerPrefix = String

type ServerFolder = String

type Ending = String

--todo use proper types?
-- todo: reduce number of conversions?
-- todo: setting the folder vs. setting the file names could be improved?
createSheetWith ::
     String -> QuestionsInQuiz -> ServerPrefix -> ServerFolder -> [(TeamNumber, Code)] -> Day -> DbQuizId -> IO ()
createSheetWith teamLabel qirs prefix folder numberedCodes day qid = do
  sheetsFolder <- sheetsFolderIO
  currentDir <- getCurrentDirectory
  let tl = T.pack teamLabel
      endings = sortOn fst numberedCodes
      paths = map (mkPath prefix folder . uncurry (TeamQuery qid)) endings
      rounds =
        map
          snd
          (sortOn
             fst
             (map
                (\qir ->
                   ( naturalToInt (unwrap (questionsInRoundRoundNumber qir))
                   , naturalToInt (unwrap (questionsInRoundNumberOfQuestions qir))))
                (unwrap qirs)))
      sht = mkSheetWithArbitraryQuestions tl rounds paths
      sheetFile = mkPathForQuizSheetWith (T.pack "") (T.pack ".") sheetFileName day qid
      qrs = mkQROnly tl paths
      codesFile = mkPathForQuizSheetWith (T.pack "") (T.pack ".") qrOnlyFileName day qid
  setCurrentDirectory (T.unpack sheetsFolder)
  writeAndCleanPDF (T.unpack sheetFile) sht
  writeAndCleanPDF (T.unpack codesFile) qrs
  setCurrentDirectory currentDir

mkPath :: ServerPrefix -> ServerFolder -> TeamQuery -> T.Text
mkPath prefix folder teamQuery =
  E.decodeUtf8
    (B.concat
       [ B.pack prefix
       , encodePath
           (map
              E.decodeUtf8
              [ B.pack folder
              , B.pack "#"
              , quizIdParam
              , L.toStrict (encode (teamQueryQuizId teamQuery))
              , teamNumberParam
              , L.toStrict (encode (teamQueryTeamNumber teamQuery))
              , teamCodeParam
              , E.encodeUtf8 (unwrap (teamQueryTeamCode teamQuery))
              ])
       ])

writeAndCleanPDF :: FilePath -> Text -> IO ()
writeAndCleanPDF mainPath content = do
  I.writeFile texFile content
  createPDF texFile `catch` noPDFLatex
  cleanLatex mainPath
  where
    texFile = mainPath ++ ".tex"
    noPDFLatex :: IOException -> IO ()
    noPDFLatex _ = putStrLn "pdflatex not found or it failed during document creation."

createPDF :: String -> IO ()
createPDF texFile = callProcess "pdflatex" ["-interaction=nonstopmode", texFile]

cleanLatex :: String -> IO ()
cleanLatex sheetFile = mapM_ (safeRemoveFile . (sheetFile ++)) [".log", ".aux", ".tex"]

safeRemoveFile :: String -> IO ()
safeRemoveFile path = removeFile path `catch` noFile
  where
    noFile :: IOException -> IO ()
    noFile _ = void (putStrLn "No file to remove")
