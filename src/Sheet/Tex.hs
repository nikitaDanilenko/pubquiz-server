module Sheet.Tex ( mkSheet ) where

import Data.List       ( intersperse, intercalate )
import Data.List.Split ( chunksOf )

mkSheet :: String -> Int -> String
mkSheet groupLabel n = concat [ 
    header,
    grp,
    centre,
    perGroup n,
    footer groupLabel
    ]
    where grp = unwords ["    {\\Huge", 
                         groupLabel, 
                         "#1:} & \\includegraphics[align=c, scale = 0.35]{./#2.png}"]

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
simpleHeader groupLabel = unlines [
        "\\newcommand{\\simpleHeader}[1]{%",
        unwords ["    {\\Huge", groupLabel, "#1:}"],
        "}"
    ]

perGroup :: Int -> String
perGroup n = unlines ((intercalate ["\\newpage", "\\simpleHeader{#1}"] 
                                   (chunksOf 2 (replicate n "\\mkTable"))) ++ ["\\newpage"])

footer :: String -> String
footer groupLabel = unlines [
    "}",
    "",
    simpleHeader groupLabel,
    "",
    "\\begin{document}",
    "",
    "\\input{listVariable}",
    "",
    "\\end{document}"
    ]