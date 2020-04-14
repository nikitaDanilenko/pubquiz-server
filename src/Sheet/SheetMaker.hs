{-# LANGUAGE TupleSections #-}

module Sheet.SheetMaker
  ( createSheetWith
  , safeRemoveFile
  , Ending
  ) where

import           Control.Exception               (catch)
import           Control.Exception.Base          (IOException)
import           Control.Monad                   (void)
import qualified Data.ByteString.Char8           as B
import qualified Data.ByteString.Lazy            as L
import           Data.Text                       (Text)
import qualified Data.Text                       as T (Text, concat,
                                                       intercalate, pack,
                                                       unpack)
import qualified Data.Text.Encoding              as E
import qualified Data.Text.IO                    as I (writeFile)
import           System.Directory                (getCurrentDirectory,
                                                  removeFile,
                                                  setCurrentDirectory)
import           System.Process                  (callProcess)

import           Codec.Picture                   (savePngImage)
import           Codec.Picture.Types             (DynamicImage (ImageY8))
import qualified Codec.QRCode                    as QR (encode)
import           Codec.QRCode.Data.ErrorLevel    (ErrorLevel (M))
import           Codec.QRCode.Data.QRCodeOptions (defaultQRCodeOptions)
import           Codec.QRCode.Data.TextEncoding  (TextEncoding (Iso8859_1OrUtf8WithoutECI))
import           Constants                       (qrOnlyFileName, quizIdParam,
                                                  sheetFileName, sheetsFolderIO,
                                                  teamCodeParam,
                                                  teamNumberParam,
                                                  teamQueryParam)
import           Control.Monad.Trans.Resource    (MonadThrow)
import           Data.Aeson                      (encode)
import           Data.List                       (sortOn)
import qualified Data.List                       as List (intercalate)
import           Data.List.NonEmpty              (fromList)
import           Data.Time.Calendar              (Day)
import           Db.Connection                   (DbQuizId)
import           Db.DbConversion                 (QuestionsInQuiz,
                                                  TeamQuery (TeamQuery),
                                                  mkPathForQuizSheetWith,
                                                  questionsInRoundNumberOfQuestions,
                                                  questionsInRoundRoundNumber,
                                                  teamQueryQuizId,
                                                  teamQueryTeamCode,
                                                  teamQueryTeamNumber)
import           General.Types                   (Code, TeamNumber, unwrap)
import           GHC.Natural                     (Natural, naturalToInt)
import           Network.HTTP.Types              (encodePathSegments)
import           Sheet.Tex                       (QRPath, mkQROnly,
                                                  mkSheetWithArbitraryQuestions, imagePath)
import           Text.URI                        (render)
import           Utils                           (encodePath,
                                                  mkURIFromSchemePathFragment)

import           Codec.QRCode.JuicyPixels        (toImage)
import           Control.Arrow                   ((&&&))

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
  pathsAndTeamQueries <- mapM ((\tq -> fmap (, tq) (mkPath prefix folder tq)) . uncurry (TeamQuery qid)) endings
  let (paths, teamQueries) = unzip pathsAndTeamQueries
  let rounds =
        map
          snd
          (sortOn
             fst
             (map
                (\qir ->
                   ( naturalToInt (unwrap (questionsInRoundRoundNumber qir))
                   , naturalToInt (unwrap (questionsInRoundNumberOfQuestions qir))))
                (unwrap qirs)))
      sht = mkSheetWithArbitraryQuestions tl rounds teamQueries
      sheetFile = mkPathForQuizSheetWith (T.pack "") (T.pack ".") sheetFileName day qid
      qrs = mkQROnly tl teamQueries
      codesFile = mkPathForQuizSheetWith (T.pack "") (T.pack ".") qrOnlyFileName day qid
  setCurrentDirectory (T.unpack sheetsFolder)
  createQRCodes pathsAndTeamQueries
  writeAndCleanPDF (T.unpack sheetFile) sht
  writeAndCleanPDF (T.unpack codesFile) qrs
  cleanQRCodes teamQueries
  setCurrentDirectory currentDir

mkPath :: MonadThrow m => ServerPrefix -> ServerFolder -> TeamQuery -> m T.Text
mkPath prefix folder teamQuery =
  fmap render (mkURIFromSchemePathFragment (T.pack prefix) (fromList [T.pack folder]) (E.decodeUtf8 fragment))
  where
    fragment =
      encodePath
        (map
           E.decodeUtf8
           [ quizIdParam
           , L.toStrict (encode (teamQueryQuizId teamQuery))
           , teamNumberParam
           , L.toStrict (encode (teamQueryTeamNumber teamQuery))
           , teamCodeParam
           , E.encodeUtf8 (unwrap (teamQueryTeamCode teamQuery))
           ])

createQRCodes :: [(QRPath, TeamQuery)] -> IO ()
createQRCodes = mapM_ (uncurry createCode)
  where
    createCode :: QRPath -> TeamQuery -> IO ()
    createCode path teamQuery =
      case QR.encode (defaultQRCodeOptions M) Iso8859_1OrUtf8WithoutECI path of
        Just image -> savePngImage (imagePath teamQuery) (ImageY8 (toImage 4 4 image))
        Nothing    -> putStrLn (unwords ["Image creation for", imagePath teamQuery, "failed"])

cleanQRCodes :: [TeamQuery] -> IO ()
cleanQRCodes = mapM_ (safeRemoveFile . imagePath)

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
