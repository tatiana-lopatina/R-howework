### ----------------------------------------------------------------------------
### Uebungsblatt 1
### Tatiana Lopatina
### ----------------------------------------------------------------------------


# Zu Beginn ausfuehren, um zu testen, ob der Code einwandfrei durchlaeuft
rm(list = ls())



### ----------------------------------------------------------------------------
### Beispiel 1
### Generierung von Sequenzen
### ----------------------------------------------------------------------------


### 1a) Sequenzen mit festem gap

# Parameter definieren
n <- 4
k <- 3

# Zahlenfolge
basis <- 1:(n * k)

# Blockweise verschieben
verschiebung <- rep(0:(n - 1), each = k)

# Sequenz zusammensetzen
folge <- basis + verschiebung
folge


### 1b) Gap verallgemeinern

# Parameter definieren
n <- 4
k <- 3
gap <- 3.5

# Zahlenfolge
basis <- 1:(n * k)

# Abstand zwischen den Bloecken anpassen
verschiebung <- rep(0:(n - 1), each = k) * (gap - 1)  

# Sequenz zusammensetzen
folge <- basis + verschiebung
folge

#Fuer welche Anwendung(en) aus der Statistik koennte dieser Code hilfreich sein?
 # Der Code kann Messzeitpunkte mit regelmaessigen Pausen festlegen.
 # Zum Beispiel: Planung von Therapietagen
    # n ist die Anzahl der Therapiebloecke
    # k ist die Anzahl aufeinanderfolgender Therapietage pro Block
    # gap ist der Abstand vom letzten Therapietag eines Blocks
    # zum ersten Therapietag des naechsten Blocks.
    # Bei gap = 2 liegt zum Beispiel ein freier Tag zwischen den Bloecken.
    # Die erzeugten Zahlen geben an, an welchen Tagen die Therapie stattfindet.


### ----------------------------------------------------------------------------
### Beispiel 2
### Wetterdaten analysieren
### ----------------------------------------------------------------------------


### Vorbereitung: Daten laden

# Arbeitsverzeichnis in jenen Ordner wechseln, in dem sich die Daten
# befinden und geladene Objekte anzeigen
setwd()
obj <- load("GeoSphereAustria.RData")
obj


### 2a) Temperatur zwischen 0 und 10 Grad, mind. 5mm Niederschlag

# Parameter definieren
temp.untergrenze <- 0
temp.obergrenze <- 10
nied.untergrenze <- 5

# Bedingungen pruefen
bool.temp <- temp >= temp.untergrenze & temp <= temp.obergrenze
bool.nied <- nied >= nied.untergrenze

# Tage zaehlen, an denen genau eine Bedingung zutrifft
anzahl.tage <- sum(xor(bool.temp, bool.nied))
anzahl.tage


### 2b) An welchen Tagen groesste Temperaturveraenderung zu Vortag?

# Betrag der Temperaturunterschiede zum Vortag
temp.aenderung <- abs(diff(temp))

# +1, weil die erste Differenz zum zweiten Tag gehoert
tage <- datum[which(temp.aenderung == max(temp.aenderung)) + 1]
tage


### 2c) Niederschlagsmenge pro Woche

# Positionen der letzten Tage jeder Woche
wochenende <- seq(from = 7, to = length(nied), by = 7)

# Niederschlag bis zum jeweiligen Tag aufsummieren
kumuliert <- cumsum(nied)

# Wochensummen aus den Differenzen berechnen
wochen.nied <- diff(c(0, kumuliert[wochenende]))
wochen.nied


### 2d) NA-Werte schaetzen

# Positionen der fehlenden Werte finden
ind.na <- which(is.na(temp.na))

# Mittelwert aus Vortag und Folgetag berechnen
schaetzungen <- (temp.na[ind.na - 1] + temp.na[ind.na + 1]) / 2

# Fehlende Werte durch Schaetzungen ersetzen
temp.na[ind.na] <- schaetzungen
schaetzungen

# Tatsaechliche und geschaetzte Temperaturen vergleichen
fehler <- temp[ind.na] - schaetzungen
summary(fehler)

# Was wuerde passieren, wenn der erste oder/und letzte Tag fehlen wuerde?
  # Falls der erste oder letzte Tag fehlt, kann die Temperatur
  # nicht als Mittelwert der beiden Nachbartage geschaetzt werden,
  # da einer der Nachbarwerte nicht existiert.


### 2e) Freiwillige Zusatzaufgabe

# Parameter definieren
temp.grenze <- 24

# Positionen der letzten Tage jeder Woche
ind.wochenende <- seq(from = 7, to = length(temp), by = 7)

# Tage mit Temperaturen unter temp.grenze kumuliert zaehlen
kumuliert.kuehl <- cumsum(temp < temp.grenze)

# Anzahl der kuehlen Tage pro Woche berechnen
kuehle.tage <- diff(c(0, kumuliert.kuehl[ind.wochenende]))

# Wochen ohne kuehle Tage bestimmen
warme.wochen <- which(kuehle.tage == 0)
warme.wochen

# Wochennummern sowie Anfangs- und Enddaten ausgeben
datum[7 * warme.wochen - 6]
datum[7 * warme.wochen]


### ----------------------------------------------------------------------------
### Beispiel 3
### Zufallszahlen fuer die Laplace-Verteilung
### ----------------------------------------------------------------------------


### 3a) Zufallszahlen generieren

# Parameter definieren
n <- 100000         # Anzahl der Zufallszahlen
mu <- 5             # Lageparameter
sigma <- 2          # Skalenparameter
lambda <- 1 / sigma

# Zufallszahlen reproduzierbar machen
set.seed(24102026)

# Zwei unabhaengige exponentialverteilte Zufallsvektoren erzeugen
exp1 <- rexp(n, rate = lambda)
exp2 <- rexp(n, rate = lambda)

# Laplace-verteilte Zufallszahlen berechnen
laplace <- mu + exp1 - exp2

# Mittelwert und Varianz ausgeben
mean(laplace)
var(laplace)

# Wie gross sollten der Mittelwert und die Varianz 
# der erzeugten Zahlen ungefaehr sein?
  # E(X) = mu = 5
  # Var(X) = 2 * sigma^2 = 8


### 3b) Plausibilitaetscheck

# Parameter definieren
p <- 0.5             # Gewuenschten Anteil

# Quantil der Exponentialverteilung berechnen
grenze <- qexp(p, rate = lambda)

# Intervallgrenzen festlegen
untergrenze <- mu - grenze
obergrenze <- mu + grenze

# Werte innerhalb des Intervalls bestimmen
im.intervall <- laplace > untergrenze & laplace < obergrenze

# Tatsaechlichen Anteil berechnen
anteil <- mean(im.intervall)

# Intervallgrenzen und Anteil ausgeben
c(untergrenze, obergrenze)
anteil
