{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

module General.Types where

import           Data.Aeson.TH         (deriveJSON)
import qualified Data.ByteString.Char8 as B
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as E
import           Data.Time.Calendar    (Day)
import           GHC.Natural           (Natural, intToNatural)
import           Utils                 (elmOptions)

newtype TeamNumber =
  TeamNumber Natural
  deriving (Eq, Ord)

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

newtype PlaceInRoundLabel =
  PlaceInRoundLabel Text

newtype PlaceAfterRoundLabel =
  PlaceAfterRoundLabel Text

newtype UserName =
  UserName Text

newtype UserSalt =
  UserSalt Text

newtype UserHash =
  UserHash Text
  deriving (Eq)

newtype Password =
  Password Text

data Action
  = CreateQuizA
  | LockA
  | UpdateSettingsA

newtype NumberOfQuestions =
  NumberOfQuestions Natural

data UserCreation =
  UserCreation
    { userCreationUser     :: UserName
    , userCreationPassword :: Password
    }

class Wrapped t v where
  unwrap :: t -> v
  wrap :: v -> t

instance Wrapped t Text => Wrapped t String where
  unwrap = T.unpack . unwrap
  wrap = wrap . T.pack

instance Wrapped t Text => Wrapped t B.ByteString where
  unwrap = E.encodeUtf8 . unwrap
  wrap = wrap . E.decodeUtf8

instance Wrapped t v => Wrapped (Maybe t) (Maybe v) where
  unwrap = fmap unwrap
  wrap = fmap wrap

class Fallback t where
  fallback :: t

instance Wrapped TeamNumber Natural where
  unwrap (TeamNumber tn) = tn
  wrap = TeamNumber

instance Wrapped RoundNumber Natural where
  unwrap (RoundNumber rn) = rn
  wrap = RoundNumber

instance Wrapped Code Text where
  unwrap (Code c) = c
  wrap = Code

instance Wrapped TeamName Text where
  unwrap (TeamName tn) = tn
  wrap = TeamName

instance Wrapped QuizName Text where
  unwrap (QuizName qn) = qn
  wrap = QuizName

instance Wrapped Place Text where
  unwrap (Place p) = p
  wrap = Place

instance Wrapped Activity Bool where
  unwrap Active   = True
  unwrap Inactive = False
  wrap False = Inactive
  wrap True  = Active

instance Wrapped QuizDate Day where
  unwrap (QuizDate d) = d
  wrap = QuizDate

instance Wrapped RoundLabel Text where
  unwrap (RoundLabel l) = l
  wrap = RoundLabel

instance Wrapped TeamLabel Text where
  unwrap (TeamLabel l) = l
  wrap = TeamLabel

instance Wrapped OwnPointsLabel Text where
  unwrap (OwnPointsLabel l) = l
  wrap = OwnPointsLabel

instance Wrapped MaxReachedLabel Text where
  unwrap (MaxReachedLabel l) = l
  wrap = MaxReachedLabel

instance Wrapped MaxReachableLabel Text where
  unwrap (MaxReachableLabel l) = l
  wrap = MaxReachableLabel

instance Wrapped BackToChartViewLabel Text where
  unwrap (BackToChartViewLabel l) = l
  wrap = BackToChartViewLabel

instance Wrapped OwnPageLabel Text where
  unwrap (OwnPageLabel l) = l
  wrap = OwnPageLabel

instance Wrapped ViewPreviousLabel Text where
  unwrap (ViewPreviousLabel l) = l
  wrap = ViewPreviousLabel

instance Wrapped CumulativeLabel Text where
  unwrap (CumulativeLabel l) = l
  wrap = CumulativeLabel

instance Wrapped IndividualRoundsLabel Text where
  unwrap (IndividualRoundsLabel l) = l
  wrap = IndividualRoundsLabel

instance Wrapped ProgressionLabel Text where
  unwrap (ProgressionLabel l) = l
  wrap = ProgressionLabel

instance Wrapped PlacementLabel Text where
  unwrap (PlacementLabel l) = l
  wrap = PlacementLabel

instance Wrapped PlaceLabel Text where
  unwrap (PlaceLabel l) = l
  wrap = PlaceLabel

instance Wrapped PointsLabel Text where
  unwrap (PointsLabel l) = l
  wrap = PointsLabel

instance Wrapped RoundWinnerLabel Text where
  unwrap (RoundWinnerLabel l) = l
  wrap = RoundWinnerLabel

instance Wrapped PlaceInRoundLabel Text where
  unwrap (PlaceInRoundLabel l) = l
  wrap = PlaceInRoundLabel

instance Wrapped PlaceAfterRoundLabel Text where
  unwrap (PlaceAfterRoundLabel l) = l
  wrap = PlaceAfterRoundLabel

instance Wrapped UserName Text where
  unwrap (UserName s) = s
  wrap = UserName

instance Wrapped UserSalt Text where
  unwrap (UserSalt s) = s
  wrap = UserSalt

instance Wrapped UserHash Text where
  unwrap (UserHash s) = s
  wrap = UserHash

instance Wrapped Password Text where
  unwrap (Password p) = p
  wrap = Password

instance Wrapped NumberOfQuestions Natural where
  unwrap (NumberOfQuestions n) = n
  wrap = NumberOfQuestions

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

instance Fallback PlaceInRoundLabel where
  fallback = wrap (T.pack "Platz in dieser Runde")

instance Fallback PlaceAfterRoundLabel where
  fallback = wrap (T.pack "Platz nach dieser Runde")

instance Fallback NumberOfQuestions where
  fallback = wrap (intToNatural 8)

deriveJSON elmOptions ''TeamNumber

deriveJSON elmOptions ''RoundNumber

deriveJSON elmOptions ''Code

deriveJSON elmOptions ''TeamName

deriveJSON elmOptions ''QuizName

deriveJSON elmOptions ''Place

deriveJSON elmOptions ''QuizDate

deriveJSON elmOptions ''RoundLabel

deriveJSON elmOptions ''TeamLabel

deriveJSON elmOptions ''OwnPointsLabel

deriveJSON elmOptions ''MaxReachedLabel

deriveJSON elmOptions ''MaxReachableLabel

deriveJSON elmOptions ''BackToChartViewLabel

deriveJSON elmOptions ''OwnPageLabel

deriveJSON elmOptions ''ViewPreviousLabel

deriveJSON elmOptions ''CumulativeLabel

deriveJSON elmOptions ''IndividualRoundsLabel

deriveJSON elmOptions ''ProgressionLabel

deriveJSON elmOptions ''PlacementLabel

deriveJSON elmOptions ''PlaceLabel

deriveJSON elmOptions ''PointsLabel

deriveJSON elmOptions ''RoundWinnerLabel

deriveJSON elmOptions ''PlaceInRoundLabel

deriveJSON elmOptions ''PlaceAfterRoundLabel

deriveJSON elmOptions ''UserName

deriveJSON elmOptions ''UserSalt

deriveJSON elmOptions ''UserHash

deriveJSON elmOptions ''Password

deriveJSON elmOptions ''Activity

deriveJSON elmOptions ''Action

deriveJSON elmOptions ''UserCreation

deriveJSON elmOptions ''NumberOfQuestions
