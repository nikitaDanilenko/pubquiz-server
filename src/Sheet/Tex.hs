module Sheet.Tex ( mkSheet ) where

import Data.List       ( intersperse, intercalate )
import Data.List.Split ( chunksOf )

mkSheet :: String -> Int -> String
mkSheet groupLabel n = concat [ 
    header,
    grp,
    centre,
    perGroup n,
    footer 
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
    

perGroup :: Int -> String
perGroup n = unlines (map (\ts -> unlines (ts ++ ["\\newpage"])) 
                          (chunksOf 2 (replicate n "\\mkTable")))

footer :: String
footer = unlines [
    "}",
    "",
    "\\begin{document}",
    "",
    "\\input{listVariable}",
    "",
    "\\end{document}"
    ]