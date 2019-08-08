{-# Language OverloadedStrings #-}

module Sheet.Tex ( mkSheet ) where

import Data.List       ( intercalate )
import Data.List.Split ( chunksOf )
import Data.Text                     ( Text )
import qualified Data.Text as T      ( pack, unwords, concat, unpack )

import Text.LaTeX.Base.Class         ( LaTeXC, fromLaTeX )
import Text.LaTeX.Base.Commands      ( documentclass, article, usepackage, raw, table, centering,
                                       tabular, pagestyle, huge2, (&), centering, large2, hline,
                                       tabularnewline, textwidth )
import Text.LaTeX.Base.Syntax        ( Measure ( CustomMeasure ), LaTeX ( .. ),
                                       TeXArg ( FixArg ), (<>) )
import Text.LaTeX.Base.Render        ( render ,rendertex )
import Text.LaTeX.Base.Types         ( Pos ( Here ),
                                       TableSpec ( LeftColumn, NameColumn, RightColumn ) )
import Text.LaTeX.Packages.Babel     ( babel )
import Text.LaTeX.Packages.Geometry  ( geometry )
import Text.LaTeX.Packages.Inputenc  ( inputenc )
import Text.LaTeX.Packages.LTableX   ( tabularx, ltablex )
import Text.LaTeX.Packages.QRCode    ( qrcode, qr, ErrorLevel ( Low ), CodeOptions ( .. ) )

finish :: LaTeX -> Text
finish  = render . (\l -> rendertex l :: LaTeX)

mkString :: LaTeX -> String
mkString = T.unpack . finish

simpleTabularStar :: LaTeXC l => [TableSpec] -> l -> l
simpleTabularStar ts content = mconcat [
    fromLaTeX (TeXComm "begin" [tabularStar]),
    braced textwidth,
    braced (mconcat (map (raw . render) ts)),
    content,
    fromLaTeX (TeXComm "end" [tabularStar])
  ]
  where tabularStar = FixArg "tabular*"

headerH :: LaTeXC l => l
headerH = mconcat [
    documentclass [] "scrartcl",
    usepackage [] inputenc,
    usepackage [raw "ngerman"] babel,
    usepackage [] "mathpazo",
    usepackage [] "array",
    usepackage [] ltablex,
    usepackage [] geometry,
    usepackage [] qrcode,

    pagestyle "empty"
    ]

mkFullHeader :: LaTeXC l => Text -> Int -> Text -> l
mkFullHeader teamLabel teamNumber pathForQRLink = mconcat [
    table [Here] (
        simpleTabularStar [
            LeftColumn,
            NameColumn "@{\\extracolsep{\\fill}}", 
            RightColumn
        ]
        (
          mkSimpleHeader teamLabel teamNumber
          &
          qr (CodeOptions False False Low) pathForQRLink 
        )
    )
  ]

mkSimpleHeader :: LaTeXC l => Text -> Int -> l
mkSimpleHeader teamLabel teamNumber =
    huge2 (raw (T.unwords [teamLabel, T.concat [T.pack (show teamNumber), T.pack ":"]]))

braced :: LaTeXC l => l -> l
braced text = mconcat [raw "{", text, raw "}"]

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
    table [Here] (
        fromLaTeX (TeXComm "renewcommand" [FixArg (TeXCommS "arraystretch"), 
                                           FixArg (raw (T.pack (show sf)))])
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