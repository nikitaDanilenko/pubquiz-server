{-# Language OverloadedStrings #-}

module Sheet.Tex ( mkSheetWithArbitraryQuestions, mkSheetWithConstantQuestions, mkQROnly, QRPath, imagePath ) where

import Data.List                    ( intersperse, intercalate )
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

import Sheet.Interval               ( Interval, Size ( Size ),
                                      mkBaseInterval, isize, toList, itake, idrop )
import Text.LaTeX.Packages.Graphicx (graphicx, includegraphics)
import Db.DbConversion (TeamQuery, teamQueryTeamNumber, teamQueryQuizId, teamQueryTeamCode)
import GHC.Natural (naturalToInt, Natural)
import General.Types (unwrap)

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
    usepackage [] graphicx,

    pagestyle "empty"
    ]

imagePath :: TeamQuery -> String
imagePath teamQuery =
  intercalate
    "-"
    [ show (teamQueryQuizId teamQuery)
    , show (unwrap (teamQueryTeamNumber teamQuery) :: Natural)
    , show (unwrap (teamQueryTeamCode teamQuery) :: String)
    ] ++ ".png"

mkFullHeader :: LaTeXC l => Text -> Double -> Maybe Double -> [TeamQuery] -> l
mkFullHeader teamLabel heightCm mVspace numbersAndPaths = mconcat [
    table [ForcePos, Here] (
        simpleTabularStar [
            LeftColumn,
            NameColumn "@{\\extracolsep{\\fill}}",
            RightColumn
        ]
        (mconcat (
          intersperse separator (
            map (\teamQuery -> mkSimpleHeader teamLabel (teamNumberOfQuery teamQuery)
                               &
                               braces (
                                 includegraphics [] (imagePath teamQuery)
                              )
                )
                numbersAndPaths
          )
        )
        )
    )
  ]
  where separator = mconcat [maybe mempty (vspace . Mm) mVspace, tabularnewline, hline]

teamNumberOfQuery :: TeamQuery -> Int
teamNumberOfQuery = naturalToInt . unwrap . teamQueryTeamNumber

mkSimpleHeader :: LaTeXC l => Text -> Int -> l
mkSimpleHeader teamLabel teamNumber =
    huge2 (raw (T.unwords [teamLabel, T.concat [T.pack (show teamNumber), T.pack ":"]]))

stretch :: Double
stretch = 2.75

heightQR :: Double
heightQR = 2

mkSingleTeamSheet :: LaTeXC l => Text -> TeamQuery -> [l] -> l
mkSingleTeamSheet teamLabel teamQuery allRounds =
    mconcat (mkFullHeader teamLabel heightQR Nothing [teamQuery] : rds)
  where rds = intersperse (mconcat [newpage, mkSimpleHeader teamLabel (teamNumberOfQuery teamQuery)]) allRounds

fittingPerRound :: Int
fittingPerRound = 8

fittingOnPage :: Int
fittingOnPage = 18

data Remainder = Full | Partial Int

-- | Creates groups of intervals, where each group of intervals fits on one page.
--   There are at most two intervals per page.
--   If there are two intervals, at least one of these has a size of at most fittingPerRound.
--   This means that having sizes [24, 8], we get the interval groups [1, 18] and [[19, 24], [1, 8]].
--   The reason for restricting the number to two per page is two-fold.
--   First, it is more legible in the resulting document.
--   Second, determining whether a new table with a specific header fits on a page is not simple
--   to do in LaTeX and may require manual computations (i.e. do [5, 5, 5] fit on one page?).
--   Since this is a likely uninteresting corner case, we use a simple implementation here.
mkIntervals :: [Int] -> [[Interval]]
mkIntervals = reverse . (\(x, _, _) -> x) . go ([], [], Full) . map (mkBaseInterval . Size) . filter (> 0) where
  go acc@(is, page, _) []
    | null page = acc
    | otherwise = (reverse page : is, [], Full)
  go (is, _, Full) (int : ints)
    | sz > fittingOnPage  = go ([start] : is, [], Full) (idrop fittingOnPage int : ints)
    | sz == fittingOnPage = go ([start] : is, [], Full) ints
    | otherwise           = go (is, [int], Partial (fittingOnPage - sz)) ints
    where sz = isize int
          start = itake fittingOnPage int
  go (is, page, Partial rest) l@(int : ints) = go (reverse newPage : is, [], Full) newInts where
    (newPage, newInts)
      | rest >= fittingPerRound && sz <= fittingPerRound = (int : page, ints)
      | rest >= fittingPerRound && sz > fittingPerRound  = (itake fittingPerRound int : page, idrop fittingPerRound int : ints)
      | otherwise = (page, l)
    sz = isize int

mkFullSheet :: LaTeXC l => Text -> [Int] -> [TeamQuery] -> l
mkFullSheet teamLabel qns teamQueries =
  mconcat
    [ header
    , document
        (mconcat
           (intersperse
              separator
              (map (\path -> mkSingleTeamSheet (protectText teamLabel) path allRounds) teamQueries)))
    ]
  where
    grouped = map (map toList) (mkIntervals qns)
    allRounds = map (mconcat . map (mkAnswerTable stretch)) grouped
    separator
      | even (length grouped) = newpage
      | otherwise = mconcat [newpage, hfill, medskip, newline, newpage]

mkSheetWithArbitraryQuestions :: Text -> [Int] -> [TeamQuery] -> Text
mkSheetWithArbitraryQuestions teamLabel qns teamQueries =
    finish (mkFullSheet teamLabel qns teamQueries :: LaTeX)

mkSheetWithConstantQuestions :: Text -> Int -> [TeamQuery] -> Text
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

mkQROnlyContent :: LaTeXC l => Text -> [TeamQuery] -> l
mkQROnlyContent teamLabel teamQueries = mconcat [
    header,
    arraystretch qrOnlyArrayStretch,
    document (
        mconcat (
          intersperse newpage (
              map (mkFullHeader teamLabel qrOnlyHeight (Just extraVspaceQR))
                  (chunksOf qrFitting teamQueries)
          )
        )
    )
  ]

mkQROnly :: Text -> [TeamQuery] -> Text
mkQROnly teamLabel = finish . mkQROnlyContent teamLabel