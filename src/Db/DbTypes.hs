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
    deriving (Eq, Ord)

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
  wrap :: v -> t

instance Unwrappable TeamNumber Natural where
  unwrap (TeamNumber tn) = tn
  wrap = TeamNumber

instance Unwrappable RoundNumber Natural where
  unwrap (RoundNumber rn) = rn
  wrap = RoundNumber

instance Unwrappable Code String where
  unwrap (Code c) = c
  wrap = Code

instance Unwrappable TeamName String where
  unwrap (TeamName tn) = tn
  wrap = TeamName

instance Unwrappable QuizName String where
  unwrap (QuizName qn) = qn
  wrap = QuizName

instance Unwrappable Place String where
  unwrap (Place p) = p
  wrap = Place

instance Unwrappable Activity Bool where
  unwrap Active   = True
  unwrap Inactive = False
  wrap False = Inactive
  wrap True  = Active

instance Unwrappable QuizDate Day where
  unwrap (QuizDate d) = d
  wrap = QuizDate

instance Unwrappable RoundLabel String where
  unwrap (RoundLabel l) = l
  wrap = RoundLabel

instance Unwrappable TeamLabel String where
  unwrap (TeamLabel l) = l
  wrap = TeamLabel

instance Unwrappable OwnPointsLabel String where
  unwrap (OwnPointsLabel l) = l
  wrap = OwnPointsLabel

instance Unwrappable MaxReachedLabel String where
  unwrap (MaxReachedLabel l) = l
  wrap = MaxReachedLabel

instance Unwrappable MaxReachableLabel String where
  unwrap (MaxReachableLabel l) = l
  wrap = MaxReachableLabel

instance Unwrappable BackToChartViewLabel String where
  unwrap (BackToChartViewLabel l) = l
  wrap = BackToChartViewLabel

instance Unwrappable MainLabel String where
  unwrap (MainLabel l) = l
  wrap = MainLabel

instance Unwrappable OwnPageLabel String where
  unwrap (OwnPageLabel l) = l
  wrap = OwnPageLabel

instance Unwrappable ViewPreviousLabel String where
  unwrap (ViewPreviousLabel l) = l
  wrap = ViewPreviousLabel

instance Unwrappable CumulativeLabel String where
  unwrap (CumulativeLabel l) = l
  wrap = CumulativeLabel

instance Unwrappable IndividualRoundsLabel String where
  unwrap (IndividualRoundsLabel l) = l
  wrap = IndividualRoundsLabel

instance Unwrappable ProgressionLabel String where
  unwrap (ProgressionLabel l) = l
  wrap = ProgressionLabel

instance Unwrappable PlacementLabel String where
  unwrap (PlacementLabel l) = l
  wrap = PlacementLabel

instance Unwrappable PlaceLabel String where
  unwrap (PlaceLabel l) = l
  wrap = PlaceLabel

instance Unwrappable PointsLabel String where
  unwrap (PointsLabel l) = l
  wrap = PointsLabel

instance Unwrappable RoundWinnerLabel String where
  unwrap (RoundWinnerLabel l) = l
  wrap = RoundWinnerLabel

instance Unwrappable UserName String where
  unwrap (UserName s) = s
  wrap = UserName

instance Unwrappable UserSalt String where
  unwrap (UserSalt s) = s
  wrap = UserSalt

instance Unwrappable UserHash String where
  unwrap (UserHash s) = s
  wrap = UserHash
