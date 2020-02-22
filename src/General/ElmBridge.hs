{-# LANGUAGE TemplateHaskell #-}

module General.ElmBridge where

import           Elm.Derive         (defaultOptions, deriveElmDef)
import           Elm.Module         (DefineElm (DefineElm), makeElmModule)

import           Data.Proxy         (Proxy (Proxy))

import           Data.Time.Calendar (Day)
import           Elm.TyRep          (EAlias (EAlias), ETCon (ETCon),
                                     EType (ETyCon), ETypeDef (ETypeAlias),
                                     ETypeName (ETypeName), ETVar (ETVar),
                                     IsElmDefinition (compileElmDef), ea_fields,
                                     ea_name, ea_newtype, ea_omit_null,
                                     ea_unwrap_unary)
import           General.Types      (BackToChartViewLabel, Code,
                                     CumulativeLabel, IndividualRoundsLabel,
                                     MainLabel, MaxReachableLabel,
                                     MaxReachedLabel, OwnPageLabel,
                                     OwnPointsLabel, Place, PlaceLabel,
                                     PlacementLabel, PointsLabel,
                                     ProgressionLabel, QuizDate, QuizName,
                                     RoundLabel, RoundNumber, RoundWinnerLabel,
                                     TeamLabel, TeamName, TeamNumber, UserHash,
                                     UserName, UserSalt, ViewPreviousLabel)

deriveElmDef defaultOptions ''TeamNumber

deriveElmDef defaultOptions ''RoundNumber

deriveElmDef defaultOptions ''Code

deriveElmDef defaultOptions ''TeamName

deriveElmDef defaultOptions ''QuizName

deriveElmDef defaultOptions ''Place

deriveElmDef defaultOptions ''QuizDate

deriveElmDef defaultOptions ''RoundLabel

deriveElmDef defaultOptions ''TeamLabel

deriveElmDef defaultOptions ''OwnPointsLabel

deriveElmDef defaultOptions ''MaxReachedLabel

deriveElmDef defaultOptions ''MaxReachableLabel

deriveElmDef defaultOptions ''BackToChartViewLabel

deriveElmDef defaultOptions ''MainLabel

deriveElmDef defaultOptions ''OwnPageLabel

deriveElmDef defaultOptions ''ViewPreviousLabel

deriveElmDef defaultOptions ''CumulativeLabel

deriveElmDef defaultOptions ''IndividualRoundsLabel

deriveElmDef defaultOptions ''ProgressionLabel

deriveElmDef defaultOptions ''PlacementLabel

deriveElmDef defaultOptions ''PlaceLabel

deriveElmDef defaultOptions ''PointsLabel

deriveElmDef defaultOptions ''RoundWinnerLabel

deriveElmDef defaultOptions ''UserName

deriveElmDef defaultOptions ''UserSalt

deriveElmDef defaultOptions ''UserHash

instance IsElmDefinition Day where
  compileElmDef _ =
    ETypeAlias
      (EAlias
         { ea_name = ETypeName "Day" []
         , ea_fields = [("year", ETyCon (ETCon "Int")), ("month", ETyCon (ETCon "Int")), ("day", ETyCon (ETCon "Int"))]
         , ea_omit_null = True
         , ea_newtype = False
         , ea_unwrap_unary = True
         })

main :: String -> IO ()
main path =
  writeFile path $
  makeElmModule
    "Types"
    [ DefineElm (Proxy :: Proxy TeamNumber)
    , DefineElm (Proxy :: Proxy RoundNumber)
    , DefineElm (Proxy :: Proxy Code)
    , DefineElm (Proxy :: Proxy TeamName)
    , DefineElm (Proxy :: Proxy QuizName)
    , DefineElm (Proxy :: Proxy Place)
    , DefineElm (Proxy :: Proxy QuizDate)
    , DefineElm (Proxy :: Proxy RoundLabel)
    , DefineElm (Proxy :: Proxy TeamLabel)
    , DefineElm (Proxy :: Proxy OwnPointsLabel)
    , DefineElm (Proxy :: Proxy MaxReachedLabel)
    , DefineElm (Proxy :: Proxy MaxReachableLabel)
    , DefineElm (Proxy :: Proxy BackToChartViewLabel)
    , DefineElm (Proxy :: Proxy MainLabel)
    , DefineElm (Proxy :: Proxy OwnPageLabel)
    , DefineElm (Proxy :: Proxy ViewPreviousLabel)
    , DefineElm (Proxy :: Proxy CumulativeLabel)
    , DefineElm (Proxy :: Proxy IndividualRoundsLabel)
    , DefineElm (Proxy :: Proxy ProgressionLabel)
    , DefineElm (Proxy :: Proxy PlacementLabel)
    , DefineElm (Proxy :: Proxy PlaceLabel)
    , DefineElm (Proxy :: Proxy PointsLabel)
    , DefineElm (Proxy :: Proxy RoundWinnerLabel)
    , DefineElm (Proxy :: Proxy UserName)
    , DefineElm (Proxy :: Proxy UserSalt)
    , DefineElm (Proxy :: Proxy UserHash)
    , DefineElm (Proxy :: Proxy Day)
    ]
