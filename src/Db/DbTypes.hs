{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Db.DbTypes where

import           Data.Time.Calendar (Day)
import           GHC.Natural        (Natural)

newtype TeamNumber =
  TeamNumber Natural

newtype RoundNumber =
  RoundNumber Natural

newtype Code =
  Code String

newtype TeamName =
  TeamName String

newtype QuizName =
  QuizName String

newtype Place =
  Place String

newtype QuizDate =
  QuizDate Day

data Activity
  = Active
  | Inactive

newtype RoundLabel =
  RoundLabel String

newtype TeamLabel =
  TeamLabel String

newtype OwnPointsLabel =
  OwnPointsLabel String

newtype MaxReachedLabel =
  MaxReachedLabel String

newtype MaxReachableLabel =
  MaxReachableLabel String

newtype BackToChartViewLabel =
  BackToChartViewLabel String

newtype MainLabel =
  MainLabel String

newtype OwnPageLabel =
  OwnPageLabel String

newtype ViewPreviousLabel =
  ViewPreviousLabel String

newtype CumulativeLabel =
  CumulativeLabel String

newtype IndividualRoundsLabel =
  IndividualRoundsLabel String

newtype ProgressionLabel =
  ProgressionLabel String

newtype PlacementLabel =
  PlacementLabel String

newtype PlaceLabel =
  PlaceLabel String

newtype PointsLabel =
  PointsLabel String

newtype RoundWinnerLabel =
  RoundWinnerLabel String

newtype UserName =
  UserName String

newtype UserSalt =
  UserSalt String

newtype UserHash =
  UserHash String

class Unwrappable t v where
  unwrap :: t -> v

instance Unwrappable TeamNumber Natural where
  unwrap (TeamNumber tn) = tn

instance Unwrappable RoundNumber Natural where
  unwrap (RoundNumber rn) = rn

instance Unwrappable Code String where
  unwrap (Code c) = c

instance Unwrappable TeamName String where
  unwrap (TeamName tn) = tn

instance Unwrappable QuizName String where
  unwrap (QuizName qn) = qn

instance Unwrappable Place String where
  unwrap (Place p) = p

instance Unwrappable Activity Bool where
  unwrap Active   = True
  unwrap Inactive = False

instance Unwrappable QuizDate Day where
  unwrap (QuizDate d) = d

instance Unwrappable RoundLabel String where
  unwrap (RoundLabel l) = l

instance Unwrappable TeamLabel String where
  unwrap (TeamLabel l) = l

instance Unwrappable OwnPointsLabel String where
  unwrap (OwnPointsLabel l) = l

instance Unwrappable MaxReachedLabel String where
  unwrap (MaxReachedLabel l) = l

instance Unwrappable MaxReachableLabel String where
  unwrap (MaxReachableLabel l) = l

instance Unwrappable BackToChartViewLabel String where
  unwrap (BackToChartViewLabel l) = l

instance Unwrappable MainLabel String where
  unwrap (MainLabel l) = l

instance Unwrappable OwnPageLabel String where
  unwrap (OwnPageLabel l) = l

instance Unwrappable ViewPreviousLabel String where
  unwrap (ViewPreviousLabel l) = l

instance Unwrappable CumulativeLabel String where
  unwrap (CumulativeLabel l) = l

instance Unwrappable IndividualRoundsLabel String where
  unwrap (IndividualRoundsLabel l) = l

instance Unwrappable ProgressionLabel String where
  unwrap (ProgressionLabel l) = l

instance Unwrappable PlacementLabel String where
  unwrap (PlacementLabel l) = l

instance Unwrappable PlaceLabel String where
  unwrap (PlaceLabel l) = l

instance Unwrappable PointsLabel String where
  unwrap (PointsLabel l) = l

instance Unwrappable RoundWinnerLabel String where
  unwrap (RoundWinnerLabel l) = l

instance Unwrappable UserName String where
  unwrap (UserName s) = s

instance Unwrappable UserSalt String where
  unwrap (UserSalt s) = s

instance Unwrappable UserHash String where
  unwrap (UserHash s) = s
