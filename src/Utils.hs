module Utils where

import System.Directory ( doesFileExist )

readOrCreate :: FilePath -> IO String
readOrCreate filePath = do
    exists <- doesFileExist filePath
    if exists then readFile filePath else return ""