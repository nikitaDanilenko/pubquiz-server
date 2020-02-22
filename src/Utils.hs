{-# Language PackageImports #-}

module Utils where

import Control.Arrow                        ( second )
import Control.Monad                        ( when )
import "cryptonite" Crypto.Hash             ( Digest, hash )
import "cryptonite" Crypto.Hash.Algorithms  ( SHA512 )
import Data.Char                            ( chr )
import qualified Data.ByteString.Char8 as B 
import Data.Function                        ( on )
import Data.List                            ( sortBy, groupBy, intercalate, sortOn )
import Data.List.Extra                      ( linesBy )
import Data.Ord                             ( comparing )
import Snap.Snaplet                         ( Handler )
import Snap.Util.CORS                       ( applyCORS, defaultOptions )
import System.Directory                     ( doesFileExist, createDirectoryIfMissing )
import System.FilePath                      ( pathSeparator )
import System.Random                        ( newStdGen, randomRs )

readOrCreateEmpty :: FilePath -> IO String
readOrCreateEmpty = readOrCreateEmptyWith "" writeFile readFile

-- todo can be probably removed
readOrCreateEmptyBS :: FilePath -> IO B.ByteString
readOrCreateEmptyBS = readOrCreateEmptyWith B.empty B.writeFile B.readFile

readOrCreateEmptyWith :: a -> (FilePath -> a -> IO ()) -> (FilePath -> IO a) -> FilePath -> IO a
readOrCreateEmptyWith empty writer reader filePath = do
    exists <- doesFileExist filePath
    if exists 
        then reader filePath 
        else do when (null dir) (createDirectoryIfMissing True dir)
                writer filePath empty
                reader filePath

  where parts = linesBy (pathSeparator ==) filePath
        dir   = intercalate [pathSeparator] (init parts)

(+>) :: B.ByteString -> Handler b service () -> (B.ByteString, Handler b service ())
(+>) = mkRoute

mkRoute :: B.ByteString -> Handler b service () -> (B.ByteString, Handler b service ())
mkRoute path service = (path, mkCORS service)

mkCORS :: Handler b service () -> Handler b service ()
mkCORS = applyCORS defaultOptions

randomStringIO :: IO String
randomStringIO = fmap (map chr . randomRs (33, 126)) newStdGen

hexadecimal :: String
hexadecimal = ['0' .. '9'] ++ ['a' .. 'f']

hexadecimalAmount :: Int
hexadecimalAmount = length hexadecimal

randomHexadecimal :: IO String
randomHexadecimal = fmap (map (hexadecimal !!) . randomRs (0, hexadecimalAmount - 1)) newStdGen

randomDistinctHexadecimal :: Int -> Int -> IO [String]
randomDistinctHexadecimal numberOfStrings size =
    fmap disambiguate (randomNonDistinct numberOfStrings size)

randomNonDistinct :: Int -> Int -> IO [String]
randomNonDistinct numberOfStrings size = do
    randomInfinite <- randomHexadecimal
    let chunks = take numberOfStrings (chunk size randomInfinite)
    return chunks

randomDistinctWithAdditional :: Int -> Int -> [String] -> IO [String]
randomDistinctWithAdditional numberOfStrings size exs = do
    chunks <- randomNonDistinct numberOfStrings size
    let uniqueChunks = disambiguate (exs ++ chunks)
    return uniqueChunks

chunk :: Int -> [a] -> [[a]]
chunk size xs | size <= 0 = [xs]
              | null xs = []
              | otherwise = h : chunk size t where (h, t) = splitAt size xs

disambiguate :: [String] -> [String]
disambiguate =
  map snd .
  sortOn fst .
  concatMap (uncurry zip . second disambiguateList . unzip) . 
  groupBy ((==) `on` snd) . 
  sortOn fst . 
  zip [(0 :: Int) ..]

disambiguateSingle :: Int -> String -> String
disambiguateSingle = (++) . show

disambiguateList :: [String] -> [String]
disambiguateList [] = []
disambiguateList [s] = [s]
disambiguateList xs = zipWith disambiguateSingle [0 ..] xs

type Hashed = Digest SHA512

mkHashed :: B.ByteString -> Hashed
mkHashed = hash