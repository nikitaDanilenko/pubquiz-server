{-# LANGUAGE TemplateHaskell #-}

module General.Labels
  ( Labels
  , mkLabels
  , teamLabel
  , ownPointsLabel
  , maxReachedLabel
  , maxReachableLabel
  , backToChartView
  , ownPageLabel
  , roundLabel
  , viewPrevious
  , cumulativeLabel
  , individualRoundsLabel
  , progressionLabel
  , placementLabel
  , placeLabel
  , pointsLabel
  , roundWinnerLabel
  , fallbackLabels
  ) where

import           Data.Aeson.TH         (deriveJSON)
import           General.Types         (BackToChartViewLabel, CumulativeLabel,
                                        Fallback (fallback),
                                        IndividualRoundsLabel,
                                        MaxReachableLabel, MaxReachedLabel,
                                        OwnPageLabel, OwnPointsLabel,
                                        PlaceLabel, PlacementLabel, PointsLabel,
                                        ProgressionLabel, RoundLabel,
                                        RoundWinnerLabel, TeamLabel,
                                        Unwrappable (wrap), ViewPreviousLabel)
import           Utils                 (elmOptions)

data Labels =
  Labels
    { roundLabel            :: RoundLabel
    , teamLabel             :: TeamLabel
    , ownPointsLabel        :: OwnPointsLabel
    , maxReachedLabel       :: MaxReachedLabel
    , maxReachableLabel     :: MaxReachableLabel
    , backToChartView       :: BackToChartViewLabel
    , ownPageLabel          :: OwnPageLabel
    , viewPrevious          :: ViewPreviousLabel
    , cumulativeLabel       :: CumulativeLabel
    , individualRoundsLabel :: IndividualRoundsLabel
    , progressionLabel      :: ProgressionLabel
    , placementLabel        :: PlacementLabel
    , placeLabel            :: PlaceLabel
    , pointsLabel           :: PointsLabel
    , roundWinnerLabel      :: RoundWinnerLabel
    }

deriveJSON elmOptions ''Labels

fallbackLabels :: Labels
fallbackLabels =
  Labels
    (fallback :: RoundLabel)
    (fallback :: TeamLabel)
    (fallback :: OwnPointsLabel)
    (fallback :: MaxReachedLabel)
    (fallback :: MaxReachableLabel)
    (fallback :: BackToChartViewLabel)
    (fallback :: OwnPageLabel)
    (fallback :: ViewPreviousLabel)
    (fallback :: CumulativeLabel)
    (fallback :: IndividualRoundsLabel)
    (fallback :: ProgressionLabel)
    (fallback :: PlacementLabel)
    (fallback :: PlaceLabel)
    (fallback :: PointsLabel)
    (fallback :: RoundWinnerLabel)

mkLabels ::
     String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> Labels
mkLabels roundLbl teamLbl ownPointsLbl maxReachedLbl maxReachableLbl backLbl ownPageLbl viewPreviousLbl cumulativeLbl individualRoundsLbl progressionLbl placementLbl placeLbl pointsLbl roundWinnerLbl =
  Labels
    { roundLabel = wrap roundLbl
    , teamLabel = wrap teamLbl
    , ownPointsLabel = wrap ownPointsLbl
    , maxReachedLabel = wrap maxReachedLbl
    , maxReachableLabel = wrap maxReachableLbl
    , backToChartView = wrap backLbl
    , ownPageLabel = wrap ownPageLbl
    , viewPrevious = wrap viewPreviousLbl
    , cumulativeLabel = wrap cumulativeLbl
    , individualRoundsLabel = wrap individualRoundsLbl
    , progressionLabel = wrap progressionLbl
    , placementLabel = wrap placementLbl
    , placeLabel = wrap placeLbl
    , pointsLabel = wrap pointsLbl
    , roundWinnerLabel = wrap roundWinnerLbl
    }
