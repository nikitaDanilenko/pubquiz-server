{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module General.ElmBridge where

import           Elm.Derive         (defaultOptions, deriveElmDef)
import           Elm.Module         (DefineElm (DefineElm), makeElmModule)

import           Data.Proxy         (Proxy (Proxy))

import           Data.Time.Calendar (Day)
import           Db.Connection      (DbQuizId)
import           Db.DbConversion    (Credentials, QuizInfo, QuizPDN,
                                     QuizSettings, Ratings, RoundRating,
                                     TeamRating, TeamInfo, Header)
import           Elm.TyRep          (EAlias (EAlias), EPrimAlias (EPrimAlias),
                                     ETCon (ETCon), ETVar (ETVar),
                                     EType (ETyCon),
                                     ETypeDef (ETypeAlias, ETypePrimAlias),
                                     ETypeName (ETypeName),
                                     IsElmDefinition (compileElmDef), ea_fields,
                                     ea_name, ea_newtype, ea_omit_null,
                                     ea_unwrap_unary, epa_name, epa_type)
import           General.Labels     (Labels)
import           General.Types      (BackToChartViewLabel, Code,
                                     CumulativeLabel, IndividualRoundsLabel,
                                     MainLabel, MaxReachableLabel,
                                     MaxReachedLabel, OwnPageLabel,
                                     OwnPointsLabel, Password, Place,
                                     PlaceLabel, PlacementLabel, PointsLabel,
                                     ProgressionLabel, QuizDate, QuizName,
                                     RoundLabel, RoundNumber, RoundWinnerLabel,
                                     TeamLabel, TeamName, TeamNumber, UserHash,
                                     UserName, UserSalt, ViewPreviousLabel, Activity)

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

deriveElmDef defaultOptions ''TeamRating

deriveElmDef defaultOptions ''RoundRating

deriveElmDef defaultOptions ''Ratings

deriveElmDef defaultOptions ''Credentials

deriveElmDef defaultOptions ''QuizSettings

deriveElmDef defaultOptions ''QuizPDN

deriveElmDef defaultOptions ''QuizInfo

deriveElmDef defaultOptions ''Labels

deriveElmDef defaultOptions ''Password

deriveElmDef defaultOptions ''TeamInfo

deriveElmDef defaultOptions ''Header

deriveElmDef defaultOptions ''Activity

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

instance IsElmDefinition DbQuizId where
  compileElmDef _ = ETypePrimAlias (EPrimAlias {epa_name = ETypeName "DbQuizId" [], epa_type = ETyCon (ETCon "Int")})

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
    , DefineElm (Proxy :: Proxy TeamRating)
    , DefineElm (Proxy :: Proxy RoundRating)
    , DefineElm (Proxy :: Proxy Ratings)
    , DefineElm (Proxy :: Proxy Credentials)
    , DefineElm (Proxy :: Proxy QuizSettings)
    , DefineElm (Proxy :: Proxy QuizPDN)
    , DefineElm (Proxy :: Proxy QuizInfo)
    , DefineElm (Proxy :: Proxy Labels)
    , DefineElm (Proxy :: Proxy DbQuizId)
    , DefineElm (Proxy :: Proxy Password)
    , DefineElm (Proxy :: Proxy TeamInfo)
    , DefineElm (Proxy :: Proxy Header)
    , DefineElm (Proxy :: Proxy Activity)
    ]
