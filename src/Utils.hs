{-# Language PackageImports #-}

module Utils where

import Control.Arrow                        ( second )
import "cryptonite" Crypto.Hash             ( Digest, hash )
import "cryptonite" Crypto.Hash.Algorithms  ( SHA512 )
import Data.Char                            ( chr )
import qualified Data.ByteString.Char8 as B 
import Data.Function                        ( on )
import Data.List                            ( sortBy, groupBy )
import Data.Ord                             ( comparing )
import Snap.Snaplet                         ( Handler )
import Snap.Util.CORS                       ( applyCORS, defaultOptions )
import System.Directory                     ( doesFileExist )
import System.Random                        ( newStdGen, randomRs )

readOrEmpty :: FilePath -> IO String
readOrEmpty filePath = do
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

randomAlphaNumeric :: IO String
randomAlphaNumeric = fmap (map (chars !!) . randomRs (0, charsLength - 1)) newStdGen where
    chars = ['0' .. '9'] ++ ['a' .. 'z']
    charsLength = length chars

randomDistinctAlphaNumeric :: Int -> Int -> IO [String]
randomDistinctAlphaNumeric numberOfStrings size = do
    randomInfinite <- randomAlphaNumeric
    let chunks = take numberOfStrings (chunk size randomInfinite)
        uniqueChunks = disambiguate chunks
    return uniqueChunks

chunk :: Int -> [a] -> [[a]]
chunk size xs | size <= 0 = [xs]
              | null xs = []
              | otherwise = h : chunk size t where (h, t) = splitAt size xs

disambiguate :: [String] -> [String]
disambiguate = 
    map snd . 
        sortBy (comparing fst) .
        concatMap (uncurry zip . second disambiguateList . unzip) . 
        groupBy ((==) `on` snd) . 
        sortBy (comparing snd) . 
        zip [0 ..]

disambiguateSingle :: Int -> String -> String
disambiguateSingle = (++) . show

disambiguateList :: [String] -> [String]
disambiguateList [] = []
disambiguateList [s] = [s]
disambiguateList xs = zipWith disambiguateSingle [0 ..] xs

type Hashed = Digest SHA512

mkHashed :: B.ByteString -> Hashed
mkHashed = hash