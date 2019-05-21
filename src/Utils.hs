module Utils where

import Data.Char                            ( chr )
import qualified Data.ByteString.Char8 as B 
import Snap.Snaplet                         ( Handler )
import Snap.Util.CORS                       ( applyCORS, defaultOptions )
import System.Directory                     ( doesFileExist )
import System.Random                        ( newStdGen, randomRs )

readOrCreate :: FilePath -> IO String
readOrCreate filePath = do
    exists <- doesFileExist filePath
    if exists then readFile filePath else return ""

readOrCreateBS :: FilePath -> IO B.ByteString
readOrCreateBS filePath = do
    exists <- doesFileExist filePath
    if exists then B.readFile filePath else return (B.pack "")

(+>) :: B.ByteString -> Handler b service () -> (B.ByteString, Handler b service ())
(+>) = mkRoute

mkRoute :: B.ByteString -> Handler b service () -> (B.ByteString, Handler b service ())
mkRoute path service = (path, mkCORS service)

mkCORS :: Handler b service () -> Handler b service ()
mkCORS = applyCORS defaultOptions

randomStringIO :: IO String
randomStringIO = fmap (map chr . randomRs (33, 126)) newStdGen