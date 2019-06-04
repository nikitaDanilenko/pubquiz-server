# Readme

## Features

### Usability

1. When you create a new quiz, a sheet for the regular rounds is automatically created.
   This sheet contains the QR codes to quickly get to the corresponding sites.
   To get the sheet, you need `pdflatex` with some standard libraries,
   and `qrencode`.
   Both tools are available free of charge under Ubuntu.
   If either of these is missing, you will get a corresponding failure message and 
   no quiz sheet will be created.
   If both tools exist, you will get a link to the path on the quiz editing page.

### Technology

1. Sensitive REST requests (i.e. posts) are secured by HMAC.
   This means that you cannot arbitrarily manipulate the quizzes,
   because you need a session key, which is only delivered upon login.
   You can, however, resend a previous update to post an earlier state,
   if you manage a man in the middle scenario.
   Still, this might prove difficult, if the connection is encrypted.