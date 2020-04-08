{-# LANGUAGE PackageImports #-}

module Utils where

import qualified Blaze.ByteString.Builder            as Builder (toByteString)
import           Control.Applicative                 (liftA2)
import           Control.Arrow                       (second)
import           Control.Monad                       (when)
import           Control.Monad.Trans.Resource        (MonadThrow)
import           "cryptonite" Crypto.Hash            (Digest, hash)
import           "cryptonite" Crypto.Hash.Algorithms (SHA512)
import qualified Data.ByteString.Char8               as B
import           Data.Char                           (chr)
import           Data.Function                       (on)
import           Data.Functor                        ((<&>))
import           Data.List                           (groupBy, intercalate,
                                                      sortBy, sortOn)
import           Data.List.Extra                     (linesBy)
import           Data.List.NonEmpty                  (NonEmpty)
import           Data.Maybe                          (fromJust, fromMaybe)
import           Data.Ord                            (comparing)
import qualified Data.Text                           as T (Text)
import qualified Elm.Derive                          as E
import           Network.HTTP.Types                  (encodePathSegments)
import           Snap.Snaplet                        (Handler)
import           Snap.Util.CORS                      (applyCORS, defaultOptions)
import           System.Directory                    (createDirectoryIfMissing,
                                                      doesFileExist)
import           System.FilePath                     (pathSeparator)
import           System.Random                       (newStdGen, randomRs)
import           Text.URI                            (Authority (Authority),
                                                      RTextLabel (..),
                                                      URI (URI), authHost,
                                                      authPort, authUserInfo,
                                                      mkFragment, mkHost,
                                                      mkPathPiece, mkScheme,
                                                      uriAuthority, uriFragment,
                                                      uriPath, uriQuery,
                                                      uriScheme)

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
randomDistinctHexadecimal numberOfStrings size = fmap disambiguate (randomNonDistinct numberOfStrings size)

randomNonDistinct :: Int -> Int -> IO [String]
randomNonDistinct numberOfStrings size = fmap (take numberOfStrings . chunk size) randomHexadecimal

chunk :: Int -> [a] -> [[a]]
chunk size xs
  | size <= 0 = [xs]
  | null xs = []
  | otherwise = h : chunk size t
  where
    (h, t) = splitAt size xs

disambiguate :: [String] -> [String]
disambiguate =
  map snd .
  sortOn fst .
  concatMap (uncurry zip . second disambiguateList . unzip) . groupBy ((==) `on` snd) . sortOn snd . zip [(0 :: Int) ..]

disambiguateSingle :: Int -> String -> String
disambiguateSingle = (++) . show

disambiguateList :: [String] -> [String]
disambiguateList []  = []
disambiguateList [s] = [s]
disambiguateList xs  = zipWith disambiguateSingle [0 ..] xs

type Hashed = Digest SHA512

mkHashed :: B.ByteString -> Hashed
mkHashed = hash

elmOptions :: E.Options
elmOptions = E.defaultOptions {E.unwrapUnaryRecords = True}

encodePath :: [T.Text] -> B.ByteString
encodePath = Builder.toByteString . encodePathSegments

mkURIFromSchemePathFragment :: MonadThrow m => T.Text -> T.Text -> NonEmpty T.Text -> T.Text -> m URI
mkURIFromSchemePathFragment scheme domain pathPieces fragment =
  liftA2
    (\host pieces ->
       URI
         { uriScheme = mkScheme scheme
         , uriAuthority = Right (Authority {authUserInfo = Nothing, authHost = host, authPort = Nothing})
         , uriPath = Just (True, pieces)
         , uriQuery = []
         , uriFragment = mkFragment fragment
         })
    (mkHost domain)
    (traverse mkPathPiece pathPieces)
