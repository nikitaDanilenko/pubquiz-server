{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module General.ElmBridge where

import           Elm.Derive         (deriveElmDef)
import           Elm.Module         (DefineElm (DefineElm), makeElmModule)

import           Data.Proxy         (Proxy (Proxy))

import           Data.Time.Calendar (Day)
import           Db.Connection      (DbQuizId)
import           Db.DbConversion    (Credentials, Header, QuestionsInQuiz,
                                     QuestionsInRound, QuizIdentifier, QuizInfo,
                                     QuizRatings, QuizSettings, Ratings,
                                     RoundRating, TeamInfo, TeamLine, TeamQuery,
                                     TeamRating, TeamTable, TeamTableInfo)
import           Elm.TyRep          (EAlias (EAlias), EPrimAlias (EPrimAlias),
                                     ETCon (ETCon), ETVar (ETVar),
                                     EType (ETyCon),
                                     ETypeDef (ETypeAlias, ETypePrimAlias),
                                     ETypeName (ETypeName),
                                     IsElmDefinition (compileElmDef), ea_fields,
                                     ea_name, ea_newtype, ea_omit_null,
                                     ea_unwrap_unary, epa_name, epa_type)
import           General.Labels     (Labels)
import           General.Types      (Action, Activity, BackToChartViewLabel,
                                     Code, CumulativeLabel,
                                     IndividualRoundsLabel, MaxReachableLabel,
                                     MaxReachedLabel, NumberOfQuestions,
                                     OwnPageLabel, OwnPointsLabel, Password,
                                     Place, PlaceAfterRoundLabel,
                                     PlaceInRoundLabel, PlaceLabel,
                                     PlacementLabel, PointsLabel,
                                     ProgressionLabel, QuizDate, QuizName,
                                     RoundLabel, RoundNumber, RoundWinnerLabel,
                                     TeamLabel, TeamName, TeamNumber,
                                     UserCreation, UserHash, UserName, UserSalt,
                                     ViewPreviousLabel)
import           Utils              (elmOptions)

deriveElmDef elmOptions ''TeamNumber

deriveElmDef elmOptions ''RoundNumber

deriveElmDef elmOptions ''Code

deriveElmDef elmOptions ''TeamName

deriveElmDef elmOptions ''QuizName

deriveElmDef elmOptions ''Place

deriveElmDef elmOptions ''QuizDate

deriveElmDef elmOptions ''RoundLabel

deriveElmDef elmOptions ''TeamLabel

deriveElmDef elmOptions ''OwnPointsLabel

deriveElmDef elmOptions ''MaxReachedLabel

deriveElmDef elmOptions ''MaxReachableLabel

deriveElmDef elmOptions ''BackToChartViewLabel

deriveElmDef elmOptions ''OwnPageLabel

deriveElmDef elmOptions ''ViewPreviousLabel

deriveElmDef elmOptions ''CumulativeLabel

deriveElmDef elmOptions ''IndividualRoundsLabel

deriveElmDef elmOptions ''ProgressionLabel

deriveElmDef elmOptions ''PlacementLabel

deriveElmDef elmOptions ''PlaceLabel

deriveElmDef elmOptions ''PointsLabel

deriveElmDef elmOptions ''RoundWinnerLabel

deriveElmDef elmOptions ''PlaceInRoundLabel

deriveElmDef elmOptions ''PlaceAfterRoundLabel

deriveElmDef elmOptions ''UserName

deriveElmDef elmOptions ''UserSalt

deriveElmDef elmOptions ''UserHash

deriveElmDef elmOptions ''TeamRating

deriveElmDef elmOptions ''RoundRating

deriveElmDef elmOptions ''Ratings

deriveElmDef elmOptions ''Credentials

deriveElmDef elmOptions ''QuizSettings

deriveElmDef elmOptions ''QuizIdentifier

deriveElmDef elmOptions ''QuizInfo

deriveElmDef elmOptions ''Labels

deriveElmDef elmOptions ''Password

deriveElmDef elmOptions ''TeamInfo

deriveElmDef elmOptions ''Header

deriveElmDef elmOptions ''Activity

deriveElmDef elmOptions ''Action

deriveElmDef elmOptions ''QuizRatings

deriveElmDef elmOptions ''TeamLine

deriveElmDef elmOptions ''TeamTable

deriveElmDef elmOptions ''TeamQuery

deriveElmDef elmOptions ''TeamTableInfo

deriveElmDef elmOptions ''UserCreation

deriveElmDef elmOptions ''NumberOfQuestions

deriveElmDef elmOptions ''QuestionsInQuiz

deriveElmDef elmOptions ''QuestionsInRound

instance IsElmDefinition Day where
  compileElmDef _ = ETypePrimAlias (EPrimAlias {epa_name = ETypeName "Day" [], epa_type = ETyCon (ETCon "String")})

instance IsElmDefinition DbQuizId where
  compileElmDef _ = ETypePrimAlias (EPrimAlias {epa_name = ETypeName "DbQuizId" [], epa_type = ETyCon (ETCon "Int")})

main :: String -> IO ()
main path =
  writeFile path $
  makeElmModule
    "Common.Types"
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
    , DefineElm (Proxy :: Proxy OwnPageLabel)
    , DefineElm (Proxy :: Proxy ViewPreviousLabel)
    , DefineElm (Proxy :: Proxy CumulativeLabel)
    , DefineElm (Proxy :: Proxy IndividualRoundsLabel)
    , DefineElm (Proxy :: Proxy ProgressionLabel)
    , DefineElm (Proxy :: Proxy PlacementLabel)
    , DefineElm (Proxy :: Proxy PlaceLabel)
    , DefineElm (Proxy :: Proxy PointsLabel)
    , DefineElm (Proxy :: Proxy RoundWinnerLabel)
    , DefineElm (Proxy :: Proxy PlaceInRoundLabel)
    , DefineElm (Proxy :: Proxy PlaceAfterRoundLabel)
    , DefineElm (Proxy :: Proxy UserName)
    , DefineElm (Proxy :: Proxy UserSalt)
    , DefineElm (Proxy :: Proxy UserHash)
    , DefineElm (Proxy :: Proxy Day)
    , DefineElm (Proxy :: Proxy TeamRating)
    , DefineElm (Proxy :: Proxy RoundRating)
    , DefineElm (Proxy :: Proxy Ratings)
    , DefineElm (Proxy :: Proxy Credentials)
    , DefineElm (Proxy :: Proxy QuizSettings)
    , DefineElm (Proxy :: Proxy QuizIdentifier)
    , DefineElm (Proxy :: Proxy QuizInfo)
    , DefineElm (Proxy :: Proxy Labels)
    , DefineElm (Proxy :: Proxy DbQuizId)
    , DefineElm (Proxy :: Proxy Password)
    , DefineElm (Proxy :: Proxy TeamInfo)
    , DefineElm (Proxy :: Proxy Header)
    , DefineElm (Proxy :: Proxy Activity)
    , DefineElm (Proxy :: Proxy Action)
    , DefineElm (Proxy :: Proxy QuizRatings)
    , DefineElm (Proxy :: Proxy TeamLine)
    , DefineElm (Proxy :: Proxy TeamTable)
    , DefineElm (Proxy :: Proxy TeamQuery)
    , DefineElm (Proxy :: Proxy TeamTableInfo)
    , DefineElm (Proxy :: Proxy UserCreation)
    , DefineElm (Proxy :: Proxy NumberOfQuestions)
    , DefineElm (Proxy :: Proxy QuestionsInQuiz)
    , DefineElm (Proxy :: Proxy QuestionsInRound)
    ]

pathToElm :: String
pathToElm = "../pubquiz-frontend/src/Common/Types.elm"
