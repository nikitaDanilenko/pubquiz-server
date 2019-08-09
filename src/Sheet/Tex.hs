{-# Language OverloadedStrings #-}

module Sheet.Tex ( mkSheet, mkSheetWithArbitraryQuestions, mkSheetWithConstantQuestions ) where

import Data.List       ( intercalate, intersperse )
import Data.List.Split ( chunksOf )
import Data.Text                     ( Text )
import qualified Data.Text as T      ( pack, unwords, concat, unpack )

import Text.LaTeX.Base.Class         ( LaTeXC, fromLaTeX, braces, comm1, comm2, liftL )
import Text.LaTeX.Base.Commands      ( documentclass, article, usepackage, raw, table, centering,
                                       tabular, pagestyle, huge2, (&), centering, large2, hline,
                                       tabularnewline, textwidth, newpage, document )
import Text.LaTeX.Base.Syntax        ( Measure ( CustomMeasure ), LaTeX ( .. ), protectText,
                                       TeXArg ( FixArg ), (<>) )
import Text.LaTeX.Base.Render        ( render ,rendertex )
import Text.LaTeX.Base.Types         ( Pos ( Here, ForcePos ),
                                       TableSpec ( LeftColumn, NameColumn, RightColumn ) )
import Text.LaTeX.Packages.Babel     ( babel )
import Text.LaTeX.Packages.Geometry  ( geometry )
import Text.LaTeX.Packages.Inputenc  ( inputenc )
import Text.LaTeX.Packages.LTableX   ( tabularx, ltablex )
import Text.LaTeX.Packages.QRCode    ( qrcode, qr, ErrorLevel ( Low ), CodeOptions ( .. ) )

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
           
headerH :: LaTeXC l => l
headerH = mconcat [
    documentclass [] "scrartcl",
    usepackage [] inputenc,
    usepackage [raw "ngerman"] babel,
    usepackage [] "mathpazo",
    usepackage [] "array",
    usepackage [] ltablex,
    usepackage (map (raw . T.pack) ["left=1cm",
                                    "textwidth=19cm",
                                    "top=1cm",
                                    "textheight=27cm"]) geometry,
    usepackage [] qrcode,

    pagestyle "empty"
    ]

mkFullHeader :: LaTeXC l => Text -> Double -> Int -> Text -> l
mkFullHeader teamLabel heightCm teamNumber pathForQRLink = mconcat [
    table [ForcePos, Here] (
        simpleTabularStar [
            LeftColumn,
            NameColumn "@{\\extracolsep{\\fill}}", 
            RightColumn
        ]
        (
          mkSimpleHeader teamLabel teamNumber
          &
          braces (
            comm1 "qrset" (raw (T.concat (map T.pack ["height=", show heightCm, "cm"])))
            <>
            qr (CodeOptions False False Low) pathForQRLink
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
    mconcat (mkFullHeader teamLabel heightQR teamNumber qrPath : rds)
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
    headerH,
    document (
        mconcat (
            zipWith (\i path -> (mkSingleTeamSheet (protectText teamLabel) path allRounds i)) 
                    [1 ..] 
                    paths
        )
    )
  ] 
  where allRounds = map (mconcat . map (mkAnswerTable stretch)) (onesOrTwos fittingPerRound qns)

mkQRPath :: Text -> Ending -> Text
mkQRPath _ _ = T.pack "undefined"

mkSheetWithArbitraryQuestions :: Text -> [Int] -> [Text] -> Text
mkSheetWithArbitraryQuestions teamLabel qns paths =
    finish (mkFullSheet teamLabel qns paths :: LaTeX)

mkSheetWithConstantQuestions :: Text -> Int -> [Text] -> Text
mkSheetWithConstantQuestions teamLabel n = 
    mkSheetWithArbitraryQuestions teamLabel (replicate n fittingPerRound)

mkSheet :: String -> Int -> String
mkSheet teamLabel n = concat [
    header,
    grp,
    centre,
    perGroup n,
    footer teamLabel
    ]
    where grp = unwords ["    {\\Huge", 
                         teamLabel, 
                         "#1:} & \\includegraphics[align=c, scale = 0.35]{./#2.png}"]

mkAnswerTable :: LaTeXC l => Double -> Int -> l
mkAnswerTable sf qs = 
    table [ForcePos, Here] (
        comm2 "renewcommand" (comm0 "arraystretch") (raw (T.pack (show sf)))
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

header :: String
header = unlines [
    "\\documentclass[]{scrartcl}",
    "\\usepackage{inputenc}",
    "\\usepackage[ngerman]{babel}",
    "\\usepackage{amssymb,amsmath}",
    "\\usepackage{graphbox}",
    "",
    "\\usepackage{mathpazo, geometry, forloop, float, graphicx}",
    "",
    "\\geometry{left=1cm,textwidth=19cm,top=1cm,textheight=27cm}",
    "\\newcounter{question}",
    "",
    "\\pagestyle{empty}",
    "",
    "\\newcommand{\\mkTable}{%",
    "\\renewcommand{\\arraystretch}{2.75}",
    "\\begin{table}[H]",
    " \\centering",
    " \\begin{tabular*}{\\textwidth}{l}",
    " \\forloop{question}{1}{\\value{question} < 9}{",
    "  {\\Large \\arabic{question}.}\\\\\\hline",
    " }",
    " \\end{tabular*}",
    "",
    "\\end{table}",
    "}",
    "",
    "\\newcommand{\\mkHeader}[2]{",
    "\\begin{table}[H]",
    " \\centering",
    " \\begin{tabular*}{\\textwidth}{l @{\\extracolsep{\\fill}} r}"
    ]

centre :: String
centre = unlines [
    " \\end{tabular*}",
    "\\end{table}",
    "}",
    "",
    "\\newcommand{\\mkRounds}[2]{%",
    "\\mkHeader{#1}{#2}"
    ]
    
simpleHeader :: String -> String
simpleHeader teamLabel = unlines [
        "\\newcommand{\\simpleHeader}[1]{%",
        unwords ["    {\\Huge", teamLabel, "#1:}"],
        "}"
    ]

perGroup :: Int -> String
perGroup n = unlines (intercalate ["\\newpage", "\\simpleHeader{#1}"] 
                                  (chunksOf 2 (replicate n "\\mkTable")) ++ ["\\newpage"])

footer :: String -> String
footer teamLabel = unlines [
    "}",
    "",
    simpleHeader teamLabel,
    "",
    "\\begin{document}",
    "",
    "\\input{listVariable}",
    "",
    "\\end{document}"
    ]