{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module General.Types where

import           Data.Aeson.TH      (defaultOptions, deriveJSON)
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Time.Calendar (Day)
import           GHC.Natural        (Natural)

newtype TeamNumber =
  TeamNumber Natural

newtype RoundNumber =
  RoundNumber Natural
  deriving (Eq, Ord)

newtype Code =
  Code Text

newtype TeamName =
  TeamName Text

newtype QuizName =
  QuizName Text

newtype Place =
  Place Text

newtype QuizDate =
  QuizDate Day

data Activity
  = Active
  | Inactive

newtype RoundLabel =
  RoundLabel Text

newtype TeamLabel =
  TeamLabel Text

newtype OwnPointsLabel =
  OwnPointsLabel Text

newtype MaxReachedLabel =
  MaxReachedLabel Text

newtype MaxReachableLabel =
  MaxReachableLabel Text

newtype BackToChartViewLabel =
  BackToChartViewLabel Text

newtype MainLabel =
  MainLabel Text

newtype OwnPageLabel =
  OwnPageLabel Text

newtype ViewPreviousLabel =
  ViewPreviousLabel Text

newtype CumulativeLabel =
  CumulativeLabel Text

newtype IndividualRoundsLabel =
  IndividualRoundsLabel Text

newtype ProgressionLabel =
  ProgressionLabel Text

newtype PlacementLabel =
  PlacementLabel Text

newtype PlaceLabel =
  PlaceLabel Text

newtype PointsLabel =
  PointsLabel Text

newtype RoundWinnerLabel =
  RoundWinnerLabel Text

newtype UserName =
  UserName Text

newtype UserSalt =
  UserSalt Text

newtype UserHash =
  UserHash Text
  deriving (Eq)

class Unwrappable t v where
  unwrap :: t -> v
  wrap :: v -> t

-- todo: This should not be necessary once no strings are used.
instance Unwrappable t Text => Unwrappable t String where
  unwrap = T.unpack . unwrap
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

deriveJSON defaultOptions ''TeamNumber

deriveJSON defaultOptions ''RoundNumber

deriveJSON defaultOptions ''Code

deriveJSON defaultOptions ''TeamName

deriveJSON defaultOptions ''QuizName

deriveJSON defaultOptions ''Place

deriveJSON defaultOptions ''QuizDate

deriveJSON defaultOptions ''RoundLabel

deriveJSON defaultOptions ''TeamLabel

deriveJSON defaultOptions ''OwnPointsLabel

deriveJSON defaultOptions ''MaxReachedLabel

deriveJSON defaultOptions ''MaxReachableLabel

deriveJSON defaultOptions ''BackToChartViewLabel

deriveJSON defaultOptions ''MainLabel

deriveJSON defaultOptions ''OwnPageLabel

deriveJSON defaultOptions ''ViewPreviousLabel

deriveJSON defaultOptions ''CumulativeLabel

deriveJSON defaultOptions ''IndividualRoundsLabel

deriveJSON defaultOptions ''ProgressionLabel

deriveJSON defaultOptions ''PlacementLabel

deriveJSON defaultOptions ''PlaceLabel

deriveJSON defaultOptions ''PointsLabel

deriveJSON defaultOptions ''RoundWinnerLabel

deriveJSON defaultOptions ''UserName

deriveJSON defaultOptions ''UserSalt

deriveJSON defaultOptions ''UserHash
