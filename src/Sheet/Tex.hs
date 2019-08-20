{-# Language OverloadedStrings #-}

module Sheet.Tex ( mkSheetWithArbitraryQuestions, mkSheetWithConstantQuestions, mkQROnly ) where

import Data.List                    ( intersperse )
import Data.List.Extra              ( chunksOf )
import Data.Text                    ( Text )
import qualified Data.Text as T     ( pack, unwords, concat )

import Text.LaTeX.Base.Class        ( LaTeXC, fromLaTeX, braces, comm1, comm2, liftL )
import Text.LaTeX.Base.Commands     ( documentclass, usepackage, raw, table, centering,
                                      pagestyle, huge2, (&), centering, large2, hline,
                                      tabularnewline, textwidth, newpage, document, medskip,
                                      newline, hfill, vspace )
import Text.LaTeX.Base.Syntax       ( LaTeX ( .. ), protectText, (<>), Measure ( Mm ) )
import Text.LaTeX.Base.Render       ( render ,rendertex )
import Text.LaTeX.Base.Types        ( Pos ( Here, ForcePos ),
                                      TableSpec ( LeftColumn, NameColumn, RightColumn ) )
import Text.LaTeX.Packages.Geometry ( geometry )
import Text.LaTeX.Packages.Inputenc ( inputenc )
import Text.LaTeX.Packages.QRCode   ( qrcode, qr, ErrorLevel ( Low ), CodeOptions ( .. ) )

import Sheet.Interval               ( Interval, Size ( Size ), 
                                      mkBaseInterval, splitTo, isize, toList )

finish :: LaTeX -> Text
finish  = render . (\l -> rendertex l :: LaTeX)

type QRPath = Text

comm0 :: LaTeXC l => String -> l
comm0 = fromLaTeX . TeXCommS

simpleTabularStar :: LaTeXC l => [TableSpec] -> l -> l
simpleTabularStar ts content = liftL (TeXEnv "tabular*" []) inner where
    inner = mconcat [
              braces textwidth,
              braces (mconcat (map (raw . render) ts)),
              content
            ]
           
header :: LaTeXC l => l
header = mconcat [
    documentclass [] "scrartcl",
    usepackage [raw "utf8"] inputenc,
    usepackage [] "mathpazo",
    usepackage [] "array",
    usepackage (map (raw . T.pack) ["left=1cm",
                                    "textwidth=19cm",
                                    "top=1cm",
                                    "textheight=27cm"]) geometry,
    usepackage [] qrcode,

    pagestyle "empty"
    ]

mkFullHeader :: LaTeXC l => Text -> Double -> Maybe Double -> [(Int,  QRPath)] -> l
mkFullHeader teamLabel heightCm mVspace numbersAndPaths = mconcat [
    table [ForcePos, Here] (
        simpleTabularStar [
            LeftColumn,
            NameColumn "@{\\extracolsep{\\fill}}", 
            RightColumn
        ]
        (mconcat (
          intersperse separator (
            map (\(i, path) -> mkSimpleHeader teamLabel i
                               &
                               braces (
                                 comm1 "qrset" (raw (T.concat (map T.pack ["height=", 
                                                                           show heightCm, 
                                                                           "cm"]
                                                              )
                                                     )
                                               )
                                 <>
                                 qr (CodeOptions False False Low) path
                              )
                )
                numbersAndPaths
          )
        ) 
        )
    )
  ]
  where separator = mconcat [maybe mempty (vspace . Mm) mVspace, tabularnewline, hline]

mkSimpleHeader :: LaTeXC l => Text -> Int -> l
mkSimpleHeader teamLabel teamNumber =
    huge2 (raw (T.unwords [teamLabel, T.concat [T.pack (show teamNumber), T.pack ":"]]))

stretch :: Double
stretch = 2.75

heightQR :: Double
heightQR = 1

mkSingleTeamSheet :: LaTeXC l => Text -> QRPath -> [l] -> Int -> l
mkSingleTeamSheet teamLabel qrPath allRounds teamNumber = 
    mconcat (mkFullHeader teamLabel heightQR Nothing [(teamNumber, qrPath)] : rds)
  where rds = intersperse (mconcat [newpage, mkSimpleHeader teamLabel teamNumber]) allRounds

-- | Takes a maximum and a list of intervals and produces a list of lists of lists of integers.
--   Each list of lists denotes a collection of answer lines that fit on one sheet.
--   Each list in such a collection denotes the actual numbers of the lines.
--   The latter may seem redundant, but is necessary when rounds have so many questions
--   that they do no longer fit on one page. 
--   The intervals that are supplied to this function are assumed to be small enough
--   to fit one page already. 
onesOrTwos :: Int -> [Interval] -> [[[Int]]]
onesOrTwos limit = map (map toList) . go where
    go []                                             = [] 
    go (a : b : as) | all ((<= limit) . isize) [a, b] = [a, b] : go as
    go (a : as)                                       = [a] : go as

fittingPerRound :: Int
fittingPerRound = 8

fittingOnPage :: Int
fittingOnPage = 18

mkFullSheet :: LaTeXC l => Text -> [Int] -> [QRPath] -> l
mkFullSheet teamLabel qns paths = mconcat [
    header,
    document (
        mconcat (
          intersperse separator (
            zipWith (\i path -> (mkSingleTeamSheet (protectText teamLabel) path allRounds i)) 
                    [1 ..] 
                    paths
          )
        )
    )
  ] 
  where grouped = onesOrTwos fittingPerRound (concatMap (splitTo fittingOnPage . mkBaseInterval . Size) qns)
        allRounds = map (mconcat . map (mkAnswerTable stretch)) grouped
        separator | even (length grouped) = newpage
                  | otherwise             = mconcat [newpage, hfill, medskip, newline, newpage]

mkSheetWithArbitraryQuestions :: Text -> [Int] -> [QRPath] -> Text
mkSheetWithArbitraryQuestions teamLabel qns paths =
    finish (mkFullSheet teamLabel qns paths :: LaTeX)

mkSheetWithConstantQuestions :: Text -> Int -> [QRPath] -> Text
mkSheetWithConstantQuestions teamLabel n = 
    mkSheetWithArbitraryQuestions teamLabel (replicate n fittingPerRound)

mkAnswerTable :: LaTeXC l => Double -> [Int] -> l
mkAnswerTable sf qs = 
    table [ForcePos, Here] (
        arraystretch sf
        <>
        simpleTabularStar 
                 [LeftColumn]
                 (
                  centering
                  <>
                  mconcat (
                    map (\i -> mconcat [
                                    large2 (raw (T.concat (map T.pack [show i, "."]))),
                                    tabularnewline,
                                    hline
                               ]
                        ) 
                        qs
                  )
                 )
    )

arraystretch :: LaTeXC l => Double -> l
arraystretch = comm2 "renewcommand" (comm0 "arraystretch") . raw . T.pack . show

qrOnlyHeight :: Double
qrOnlyHeight = 2

qrOnlyArrayStretch :: Double
qrOnlyArrayStretch = 5

extraVspaceQR :: Double
extraVspaceQR = 5

qrFitting :: Int
qrFitting = 8

mkQROnlyContent :: LaTeXC l => Text -> [QRPath] -> l
mkQROnlyContent teamLabel paths = mconcat [
    header,
    arraystretch qrOnlyArrayStretch,
    document (
        mconcat (
          intersperse newpage (
              map (mkFullHeader teamLabel qrOnlyHeight (Just extraVspaceQR))
                  (chunksOf qrFitting (zip [1 ..] paths))
          )
        )
    )
  ]

mkQROnly :: Text -> [QRPath] -> Text
mkQROnly teamLabel paths = finish (mkQROnlyContent teamLabel paths)