{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleContexts #-}

module General.Types where

import           Data.Aeson         (FromJSON, ToJSON)
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Time.Calendar (Day)
import           GHC.Generics       (Generic)
import           GHC.Natural        (Natural)

newtype TeamNumber =
  TeamNumber Natural
  deriving (Generic)

newtype RoundNumber =
  RoundNumber Natural
  deriving (Eq, Ord, Generic)

newtype Code =
  Code Text
  deriving (Generic)

newtype TeamName =
  TeamName Text
  deriving (Generic)

newtype QuizName =
  QuizName Text
  deriving (Generic)

newtype Place =
  Place Text
  deriving (Generic)

newtype QuizDate =
  QuizDate Day
  deriving (Generic)

data Activity
  = Active
  | Inactive

newtype RoundLabel =
  RoundLabel Text
  deriving (Generic)

newtype TeamLabel =
  TeamLabel Text
  deriving (Generic)

newtype OwnPointsLabel =
  OwnPointsLabel Text
  deriving (Generic)

newtype MaxReachedLabel =
  MaxReachedLabel Text
  deriving (Generic)

newtype MaxReachableLabel =
  MaxReachableLabel Text
  deriving (Generic)

newtype BackToChartViewLabel =
  BackToChartViewLabel Text
  deriving (Generic)

newtype MainLabel =
  MainLabel Text
  deriving (Generic)

newtype OwnPageLabel =
  OwnPageLabel Text
  deriving (Generic)

newtype ViewPreviousLabel =
  ViewPreviousLabel Text
  deriving (Generic)

newtype CumulativeLabel =
  CumulativeLabel Text
  deriving (Generic)

newtype IndividualRoundsLabel =
  IndividualRoundsLabel Text
  deriving (Generic)

newtype ProgressionLabel =
  ProgressionLabel Text
  deriving (Generic)

newtype PlacementLabel =
  PlacementLabel Text
  deriving (Generic)

newtype PlaceLabel =
  PlaceLabel Text
  deriving (Generic)

newtype PointsLabel =
  PointsLabel Text
  deriving (Generic)

newtype RoundWinnerLabel =
  RoundWinnerLabel Text
  deriving (Generic)

newtype UserName =
  UserName Text
  deriving (Generic)

newtype UserSalt =
  UserSalt Text
  deriving (Generic)

newtype UserHash =
  UserHash Text
  deriving (Generic)

class Unwrappable t v where
  unwrap :: t -> v
  wrap :: v -> t

-- todo: This should not be necessary once no strings are used.
instance Unwrappable t Text => Unwrappable t String where
  unwrap  = T.unpack . unwrap
  wrap = wrap . T.pack

class Fallback t where
  fallback :: t

instance Unwrappable TeamNumber Natural where
  unwrap (TeamNumber tn) = tn
  wrap = TeamNumber

instance Unwrappable RoundNumber Natural where
  unwrap (RoundNumber rn) = rn
  wrap = RoundNumber

instance Unwrappable Code Text where
  unwrap (Code c) = c
  wrap = Code

instance Unwrappable TeamName Text where
  unwrap (TeamName tn) = tn
  wrap = TeamName

instance Unwrappable QuizName Text where
  unwrap (QuizName qn) = qn
  wrap = QuizName

instance Unwrappable Place Text where
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

instance Unwrappable RoundLabel Text where
  unwrap (RoundLabel l) = l
  wrap = RoundLabel

instance Unwrappable TeamLabel Text where
  unwrap (TeamLabel l) = l
  wrap = TeamLabel

instance Unwrappable OwnPointsLabel Text where
  unwrap (OwnPointsLabel l) = l
  wrap = OwnPointsLabel

instance Unwrappable MaxReachedLabel Text where
  unwrap (MaxReachedLabel l) = l
  wrap = MaxReachedLabel

instance Unwrappable MaxReachableLabel Text where
  unwrap (MaxReachableLabel l) = l
  wrap = MaxReachableLabel

instance Unwrappable BackToChartViewLabel Text where
  unwrap (BackToChartViewLabel l) = l
  wrap = BackToChartViewLabel

instance Unwrappable MainLabel Text where
  unwrap (MainLabel l) = l
  wrap = MainLabel

instance Unwrappable OwnPageLabel Text where
  unwrap (OwnPageLabel l) = l
  wrap = OwnPageLabel

instance Unwrappable ViewPreviousLabel Text where
  unwrap (ViewPreviousLabel l) = l
  wrap = ViewPreviousLabel

instance Unwrappable CumulativeLabel Text where
  unwrap (CumulativeLabel l) = l
  wrap = CumulativeLabel

instance Unwrappable IndividualRoundsLabel Text where
  unwrap (IndividualRoundsLabel l) = l
  wrap = IndividualRoundsLabel

instance Unwrappable ProgressionLabel Text where
  unwrap (ProgressionLabel l) = l
  wrap = ProgressionLabel

instance Unwrappable PlacementLabel Text where
  unwrap (PlacementLabel l) = l
  wrap = PlacementLabel

instance Unwrappable PlaceLabel Text where
  unwrap (PlaceLabel l) = l
  wrap = PlaceLabel

instance Unwrappable PointsLabel Text where
  unwrap (PointsLabel l) = l
  wrap = PointsLabel

instance Unwrappable RoundWinnerLabel Text where
  unwrap (RoundWinnerLabel l) = l
  wrap = RoundWinnerLabel

instance Unwrappable UserName Text where
  unwrap (UserName s) = s
  wrap = UserName

instance Unwrappable UserSalt Text where
  unwrap (UserSalt s) = s
  wrap = UserSalt

instance Unwrappable UserHash Text where
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
  fallback = wrap (T.pack "Runde")

instance Fallback TeamLabel where
  fallback = wrap (T.pack "Gruppe")

instance Fallback OwnPointsLabel where
  fallback = wrap (T.pack "Erreichte Punkte")

instance Fallback MaxReachedLabel where
  fallback = wrap (T.pack "Erreichte Höchstpunktzahl")

instance Fallback MaxReachableLabel where
  fallback = wrap (T.pack "Erreichbare Punkte")

instance Fallback BackToChartViewLabel where
  fallback = wrap (T.pack "Gesamtansicht")

instance Fallback MainLabel where
  fallback = wrap (T.pack "Pubquiz")

instance Fallback OwnPageLabel where
  fallback = wrap (T.pack "Eigene Punkte")

instance Fallback ViewPreviousLabel where
  fallback = wrap (T.pack "Alle Quizzes")

instance Fallback CumulativeLabel where
  fallback = wrap (T.pack "Gesamtpunkte")

instance Fallback IndividualRoundsLabel where
  fallback = wrap (T.pack "Punkte pro Runde")

instance Fallback ProgressionLabel where
  fallback = wrap (T.pack "Verlauf")

instance Fallback PlacementLabel where
  fallback = wrap (T.pack "Platzierungen")

instance Fallback PlaceLabel where
  fallback = wrap (T.pack "Platz")

instance Fallback PointsLabel where
  fallback = wrap (T.pack "Punkte")

instance Fallback RoundWinnerLabel where
  fallback = wrap (T.pack "Rundensieger")
