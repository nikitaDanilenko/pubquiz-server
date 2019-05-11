* Da eine Cabal-Sandbox vorliegt, muss diese für ghci bekannt gemacht werden.
  Dafür kann man
  
  `ghci -no-user-package-db -package-db .cabal-sandbox\\x86_64-windows-ghc-8.6.3-packages.conf.d`

  in dem Ordner mit der Sandbox ausführen.

* GHC(i) weiß nicht, wo weitere Module gesucht werden können. Dazu kann man es mit dem Parameter `-i<folder>`
  aufrufen, wobei `<folder>` der Ordner ist, in dem gesucht werden soll.

* Wenn das Projekt steht, kann es auf dem Server direkt gestartet werden und läuft dann als Dienst.
  Für SSL müssen folgende Schritte bedacht werden:

  1. Man muss `snap-server` mit der Option `-fopenssl` installieren.
     Das sorgt für benötigte Abhängigkeiten (unter einem sehr frischen Ubuntu muss man
     evtl. noch `libssl-dev` installieren).
     Dafür kann man dieses neu installieren.
     Ggf. kommt eine Problemmeldung, die man aber durch Löschen der vorhandenen .exe beseitigen kann.

  1. Das Projekt muss mit `cabal build -fopenssl` gebaut werden.
     Damit werden die Optionen für SSL freigeschaltet (?).

  1. Nach dem Kompilieren muss der Dienst mit fünf Parametern gestartet werden:

     1. `--ssl-address=0.0.0.0` damit läuft das als Dienst (?).

     1. `--ssl-port=9000` oder ein anderer, offener (!) Port. 
        443 ist nicht möglich, weil dieser schon belegt ist.

     1. `--ssl-cert=<certificate>`, wobei `<certificate>` das von SSL verwendete Zertifikat ist.
        Normalerweise ist es in `/etc/ssl/certs/` zu finden.
        Welches man verwendet kann man in `/etc/apache2/sites-enabled/default-ssl.conf`
        nachlesen.

     1. `--no-ssl-chain-cert` (bzw. `--ssl-chain-cert`, falls man ein Chain-Zertifikat hat).

     1. `--ssl-key=<privatekey>`, wobei `<privatekey>` der zum Zertifikat gehörige private Schlüssel
        ist. Welchen man verwendet, kann man ebenfalls in der Datei
        `/etc/apache2/sites-enabled/default-ssl.conf` nachlesen.