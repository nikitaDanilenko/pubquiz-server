1. The file `SheetMaker.hs` is a separate script, which can be compiled with
   ~~~~
   ghc --make SheetMaker.hs -o sheetMaker
   ~~~~
   The resulting script takes as first parameter the full prefix of the online resource where
   the scores will be visible e.g. `https://www.example.com/`.
   Every following parameter is interpreted as anonymous group ending.
   The script then creates QR codes for these addresses,
   creates a `.tex` file,
   and compiles this file to obtain the necessary sheets.
   Afterwards, the QR codes and the standard latex noise is cleaned.

   This script requires `pdflatex` and `qrencode`, which are both available free of charge
   for linux.