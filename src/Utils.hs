module Utils where

import qualified Data.ByteString.Char8 as B 
import Snap.Snaplet                         ( Handler )
import Snap.Util.CORS                       ( applyCORS, defaultOptions )
import System.Directory                     ( doesFileExist )

readOrCreate :: FilePath -> IO String
readOrCreate filePath = do
    exists <- doesFileExist filePath
    if exists then readFile filePath else return ""

(+>) :: B.ByteString -> Handler b service () -> (B.ByteString, Handler b service ())
(+>) = mkRoute

mkRoute :: B.ByteString -> Handler b service () -> (B.ByteString, Handler b service ())
mkRoute path service = (path, mkCORS service)

mkCORS :: Handler b service () -> Handler b service ()
mkCORS = applyCORS defaultOptions