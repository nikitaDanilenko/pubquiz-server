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

* Um ein Programm als `root` auszuführen, gehe man wie folgt vor:

  1. Lege `/etc/systemd/system/myservice.service` an.

  1. In diese Datei schreibe

     ~~~~
     [Unit]
     Description=<Describe what your service does>
     [Service]
     WorkingDirectory=/path/to/dir/of/your/program/
     ExecStart=/path/to/dir/of/your/program/precise/program arguments
     [Install]
     WantedBy=multi.user.target
     ~~~~

     So wird das Programm als `root` ausgeführt, ansonsten muss man bei `Service` noch einen
     Benutzer setzen.

  1. Erlaube Nutzung `sudo systemctl enable myservice.service`.

  1. Starte den Service-Provider neu: `sudo systemctl daemon-reload`.

  1. Starte den Service `sudo systemctl start myservice.service`

  1. Den Status des Programms kann man sich ansehen `sudo systemctl status myservice.service`.

1. `curl` findet nicht auf Anhieb die richtigen Zertifikate für SSL.
   Man kann die Verifikation mit dem Parameter `-k` (bzw. `--insecure`) unterdrücken.

1. Wichtig: Snap muss mit `-fopenssl` installiert werden.
   Erst danach kann man mit `sudo cabal install -fopenssl` das eigentliche Projekt bauen.
   Falls es mit SSL sofort nach dem Start beendet, heißt das wahrscheinlich,
   dass genau die SSL-Bibliothek fehlt.