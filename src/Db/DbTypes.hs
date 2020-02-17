{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Db.DbTypes where

import           Data.Aeson         (FromJSON, ToJSON)
import           Data.Time.Calendar (Day)
import           GHC.Generics       (Generic)
import           GHC.Natural        (Natural)
import           Labels             (Labels)

newtype TeamNumber =
  TeamNumber Natural
  deriving (Generic)

newtype RoundNumber =
  RoundNumber Natural
  deriving (Eq, Ord, Generic)

newtype Code =
  Code String
  deriving (Generic)

newtype TeamName =
  TeamName String
  deriving (Generic)

newtype QuizName =
  QuizName String
  deriving (Generic)

newtype Place =
  Place String
  deriving (Generic)

newtype QuizDate =
  QuizDate Day
  deriving (Generic)

data Activity
  = Active
  | Inactive

newtype RoundLabel =
  RoundLabel String
  deriving (Generic)

newtype TeamLabel =
  TeamLabel String
  deriving (Generic)

newtype OwnPointsLabel =
  OwnPointsLabel String
  deriving (Generic)

newtype MaxReachedLabel =
  MaxReachedLabel String
  deriving (Generic)

newtype MaxReachableLabel =
  MaxReachableLabel String
  deriving (Generic)

newtype BackToChartViewLabel =
  BackToChartViewLabel String
  deriving (Generic)

newtype MainLabel =
  MainLabel String
  deriving (Generic)

newtype OwnPageLabel =
  OwnPageLabel String
  deriving (Generic)

newtype ViewPreviousLabel =
  ViewPreviousLabel String
  deriving (Generic)

newtype CumulativeLabel =
  CumulativeLabel String
  deriving (Generic)

newtype IndividualRoundsLabel =
  IndividualRoundsLabel String
  deriving (Generic)

newtype ProgressionLabel =
  ProgressionLabel String
  deriving (Generic)

newtype PlacementLabel =
  PlacementLabel String
  deriving (Generic)

newtype PlaceLabel =
  PlaceLabel String
  deriving (Generic)

newtype PointsLabel =
  PointsLabel String
  deriving (Generic)

newtype RoundWinnerLabel =
  RoundWinnerLabel String
  deriving (Generic)

newtype UserName =
  UserName String
  deriving (Generic)

newtype UserSalt =
  UserSalt String
  deriving (Generic)

newtype UserHash =
  UserHash String
  deriving (Generic)

class Unwrappable t v where
  unwrap :: t -> v
  wrap :: v -> t

class Fallback t where
  fallback :: t

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

instance ToJSON TeamNumber

instance ToJSON RoundNumber

instance ToJSON Code

instance ToJSON TeamName

instance ToJSON QuizName

instance ToJSON Place

instance ToJSON QuizDate

instance ToJSON RoundLabel

instance ToJSON TeamLabel

instance ToJSON OwnPointsLabel

instance ToJSON MaxReachedLabel

instance ToJSON MaxReachableLabel

instance ToJSON BackToChartViewLabel

instance ToJSON MainLabel

instance ToJSON OwnPageLabel

instance ToJSON ViewPreviousLabel

instance ToJSON CumulativeLabel

instance ToJSON IndividualRoundsLabel

instance ToJSON ProgressionLabel

instance ToJSON PlacementLabel

instance ToJSON PlaceLabel

instance ToJSON PointsLabel

instance ToJSON RoundWinnerLabel

instance ToJSON UserName

instance ToJSON UserSalt

instance ToJSON UserHash

instance FromJSON TeamNumber

instance FromJSON RoundNumber

instance FromJSON Code

instance FromJSON TeamName

instance FromJSON QuizName

instance FromJSON Place

instance FromJSON QuizDate

instance FromJSON RoundLabel

instance FromJSON TeamLabel

instance FromJSON OwnPointsLabel

instance FromJSON MaxReachedLabel

instance FromJSON MaxReachableLabel

instance FromJSON BackToChartViewLabel

instance FromJSON MainLabel

instance FromJSON OwnPageLabel

instance FromJSON ViewPreviousLabel

instance FromJSON CumulativeLabel

instance FromJSON IndividualRoundsLabel

instance FromJSON ProgressionLabel

instance FromJSON PlacementLabel

instance FromJSON PlaceLabel

instance FromJSON PointsLabel

instance FromJSON RoundWinnerLabel

instance FromJSON UserName

instance FromJSON UserSalt

instance FromJSON UserHash

instance Fallback RoundLabel where
  fallback = wrap "Runde"

instance Fallback TeamLabel where
  fallback = wrap "Gruppe"

instance Fallback OwnPointsLabel where
  fallback = wrap "Erreichte Punkte"

instance Fallback MaxReachedLabel where
  fallback = wrap "Erreichte Höchstpunktzahl"

instance Fallback MaxReachableLabel where
  fallback = wrap "Erreichbare Punkte"

instance Fallback BackToChartViewLabel where
  fallback = wrap "Gesamtansicht"

instance Fallback MainLabel where
  fallback = wrap "Pubquiz"

instance Fallback OwnPageLabel where
  fallback = wrap "Eigene Punkte"

instance Fallback ViewPreviousLabel where
  fallback = wrap "Alle Quizzes"

instance Fallback CumulativeLabel where
  fallback = wrap "Gesamtpunkte"

instance Fallback IndividualRoundsLabel where
  fallback = wrap "Punkte pro Runde"

instance Fallback ProgressionLabel where
  fallback = wrap "Verlauf"

instance Fallback PlacementLabel where
  fallback = wrap "Platzierungen"

instance Fallback PlaceLabel where
  fallback = wrap "Platz"

instance Fallback PointsLabel where
  fallback = wrap "Punkte"

instance Fallback RoundWinnerLabel where
  fallback = wrap "Rundensieger"
