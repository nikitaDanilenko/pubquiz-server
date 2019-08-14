{-# Language OverloadedStrings #-}

module Sheet.Tex ( mkSheetWithArbitraryQuestions, mkSheetWithConstantQuestions, mkQROnly ) where

import Data.List                    ( intersperse )
import Data.Text                    ( Text )
import qualified Data.Text as T     ( pack, unwords, concat )

import Text.LaTeX.Base.Class        ( LaTeXC, fromLaTeX, braces, comm1, comm2, liftL )
import Text.LaTeX.Base.Commands     ( documentclass, usepackage, raw, table, centering,
                                      pagestyle, huge2, (&), centering, large2, hline,
                                      tabularnewline, textwidth, newpage, document, medskip,
                                      newline, hfill )
import Text.LaTeX.Base.Syntax       ( LaTeX ( .. ), protectText, (<>) )
import Text.LaTeX.Base.Render       ( render ,rendertex )
import Text.LaTeX.Base.Types        ( Pos ( Here, ForcePos ),
                                      TableSpec ( LeftColumn, NameColumn, RightColumn ) )
import Text.LaTeX.Packages.Geometry ( geometry )
import Text.LaTeX.Packages.Inputenc ( inputenc )
import Text.LaTeX.Packages.QRCode   ( qrcode, qr, ErrorLevel ( Low ), CodeOptions ( .. ) )

import Pages.HtmlUtil              ( unEscape )

finish :: LaTeX -> Text
finish  = render . (\l -> rendertex l :: LaTeX)

type Ending = Text

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

mkFullHeader :: LaTeXC l => Text -> Double -> [(Int,  Text)] -> l
mkFullHeader teamLabel heightCm numbersAndPaths = mconcat [
    table [ForcePos, Here] (
        simpleTabularStar [
            LeftColumn,
            NameColumn "@{\\extracolsep{\\fill}}", 
            RightColumn
        ]
        (mconcat (
          intersperse (tabularnewline <> hline) (
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

mkSimpleHeader :: LaTeXC l => Text -> Int -> l
mkSimpleHeader teamLabel teamNumber =
    huge2 (raw (T.unwords [teamLabel, T.concat [T.pack (show teamNumber), T.pack ":"]]))

stretch :: Double
stretch = 2.75

heightQR :: Double
heightQR = 1

mkSingleTeamSheet :: LaTeXC l => Text -> Text -> [l] -> Int -> l
mkSingleTeamSheet teamLabel qrPath allRounds teamNumber = 
    mconcat (mkFullHeader teamLabel heightQR [(teamNumber, qrPath)] : rds)
  where rds = intersperse (mconcat [newpage, mkSimpleHeader teamLabel teamNumber]) allRounds

-- | Takes a maximum and a list of values and produces a sequence of ones and twos.
--   The motivation is that up to two values but at least one value fit into a single container.
--   If they fit (each one is small enough), we take the two values.
--   Otherwise we take only one. 
onesOrTwos :: Int -> [Int] -> [[Int]]
onesOrTwos limit = go where
    go [] = [] 
    go (a : b : as) | all (<= limit) [a, b] = [a, b] : go as
    go (a : as) = [a] : go as

fittingPerRound :: Int
fittingPerRound = 8

mkFullSheet :: LaTeXC l => Text -> [Int] -> [Text] -> l
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
  where grouped = onesOrTwos fittingPerRound qns
        allRounds = map (mconcat . map (mkAnswerTable stretch)) grouped
        separator | even (length grouped) = newpage
                  | otherwise             = mconcat [newpage, hfill, medskip, newline, newpage]

mkSheetWithArbitraryQuestions :: Text -> [Int] -> [Text] -> Text
mkSheetWithArbitraryQuestions teamLabel qns paths =
    finish (mkFullSheet teamLabel qns paths :: LaTeX)

mkSheetWithConstantQuestions :: Text -> Int -> [Text] -> Text
mkSheetWithConstantQuestions teamLabel n = 
    mkSheetWithArbitraryQuestions teamLabel (replicate n fittingPerRound)

mkAnswerTable :: LaTeXC l => Double -> Int -> l
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
                        [1 .. qs]
                  )
                 )
    )

arraystretch :: LaTeXC l => Double -> l
arraystretch = comm2 "renewcommand" (comm0 "arraystretch") . raw . T.pack . show

qrOnlyHeight :: Double
qrOnlyHeight = 2

qrOnlyArrayStretch :: Double
qrOnlyArrayStretch = 10

mkQROnlyContent :: LaTeXC l => Text -> [Text] -> l
mkQROnlyContent teamLabel paths = mconcat [
    header,
    arraystretch qrOnlyArrayStretch,
    document (mkFullHeader teamLabel qrOnlyHeight (zip [1 ..] paths))
  ]

mkQROnly :: Text -> [Text] -> Text
mkQROnly teamLabel paths = finish (mkQROnlyContent teamLabel paths)