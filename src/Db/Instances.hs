{-# LANGUAGE FlexibleContexts #-}

module Db.Instances where

import           Data.Text      (pack, unpack)
import           GHC.Natural    (Natural)
import           Web.PathPieces (PathPiece (..))

instance PathPiece Natural where
  fromPathPiece text =
    if isValidNaturalString u
      then Just (read u)
      else Nothing
    where
      u = unpack text
  toPathPiece = pack . show

isValidNaturalString :: String -> Bool
isValidNaturalString = all (\c -> c `elem` ['0' .. '9'])
