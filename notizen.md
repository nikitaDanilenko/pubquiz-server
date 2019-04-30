* Da eine Cabal-Sandbox vorliegt, muss diese für ghci bekannt gemacht werden.
  Dafür kann man
  
  `ghci -no-user-package-db -package-db .cabal-sandbox\\x86_64-windows-ghc-8.6.3-packages.conf.d`

  in dem Ordner mit der Sandbox ausführen.

* GHC(i) weiß nicht, wo weitere Module gesucht werden können. Dazu kann man es mit dem Parameter `-i<folder>`
  aufrufen, wobei `<folder>` der Ordner ist, in dem gesucht werden soll.