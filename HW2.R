### ----------------------------------------------------------------------------
### Uebungsblatt 2
### Tatiana Lopatina
### ----------------------------------------------------------------------------


# Zu Beginn ausfuehren, um zu testen, ob der Code einwandfrei durchlaeuft
rm(list = ls())



### ----------------------------------------------------------------------------
### Beispiel 1
### Gestutzte geometrische Verteilung
### ----------------------------------------------------------------------------

### Parameter definieren
p <- 0.3
k.max <- 10
m <- 12
n <- 100000

# Zufallszahlen reproduzierbar machen
set.seed(03112026)

### 1a) Zufallszahlen erzeugen

# Geometrisch verteilte Zufallszahlen erzeugen
y.werte <- rgeom(n * m, prob = p)

# Werte auf k.max begrenzen
y.werte <- pmin(y.werte, k.max)

# Zufallszahlen in einer Matrix anordnen
y.matrix <- matrix(y.werte, nrow = n, ncol = m)

# Maximum jeder Zeile bestimmen
y.max.werte <- apply(y.matrix, MARGIN = 1, FUN = max)

# Erwartungswert und Varianz schaetzen
erwartungswert <- mean(y.max.werte)
varianz <- var(y.max.werte)

erwartungswert
varianz



### 1b) Wahrscheinlichkeitsfunktion des Maximums schaetzen

# Lueckenlose Haeufigkeitstabelle vorbereiten
haeufigkeiten <- rep(0, k.max + 1)
names(haeufigkeiten) <- 0:k.max

# Beobachtete Haeufigkeiten eintragen
haeufigkeiten[names(table(y.max.werte))] <- table(y.max.werte) 

# Absolute und relative Haeufigkeiten zusammenstellen
haeufigkeitstabelle <- cbind(haeufigkeiten, haeufigkeiten/sum(haeufigkeiten))
colnames(haeufigkeitstabelle) <- 
  c("absolute Haeufigkeit", "relative Haeufigkeit")

# Summe der relativen Haeufigkeiten ueberpruefen
all.equal(sum(haeufigkeitstabelle[, "relative Haeufigkeit"]), 1)

haeufigkeitstabelle



### ----------------------------------------------------------------------------
### Beispiel 2
### Olympische Winterspiele 2026
### ----------------------------------------------------------------------------


### Vorbereitung: Daten laden

# Arbeitsverzeichnis in jenen Ordner wechseln, in dem sich die Daten
# befinden und geladene Objekte anzeigen
setwd("")
obj <- load("Olympia_2026.RData")
obj

# Parameter definieren
gewaehlter.tag <- 1
vortag <- max(1, gewaehlter.tag - 1)

### 2a) daily.tables: Tagesbezogener Medaillenspiegel

# Kumulierte Medaillen des gewaehlten Tages
heute <- daily.tables[[gewaehlter.tag]]

# Medaillendifferenz zum Vortag berechnen
# Sonderfall Tag1: Multiplikation der Vortagssumme mit 0
medaillen.diff <- heute[rownames(daily.tables[[vortag]]), ,drop = FALSE] -
                  daily.tables[[vortag]] * (gewaehlter.tag > 1)

# Tagesmedaillen berechnen
heute[rownames(medaillen.diff), ] <- medaillen.diff

# Laender ohne Medaillen entfernen
heute <- heute[rowSums(heute) > 0, , drop = FALSE]

# Nach Gold, Silber und Bronze sortieren
heute <- heute[
  order(heute[ ,"Gold"],
        heute[ ,"Silver"],
        heute[ ,"Bronze"], decreasing = TRUE), , drop = FALSE]

heute



### 2b) daily.tables: Laender mit erster Medaille an einem Tag

# Laender bis zum gewaehlten Tag bestimmen
laender.tag <- rownames(daily.tables[[gewaehlter.tag]])

# Laender bis zum Vortag bestimmen
# Sonderfall Tag1: keine Laender des Vortages
laender.vortag <- rownames(daily.tables[[vortag]])[gewaehlter.tag > 1]

# Laender auswaehlen, die am gewaehlten Tag erstmals erscheinen
erste.medaille <- laender.tag[!laender.tag %in% laender.vortag]

erste.medaille



### 2c) daily winners: Tagesbezogener Medaillenspiegel

# Medaillengewinner des gewaehlten Tages bestimmen
gewinner.tag <- daily.winners[[gewaehlter.tag]]

# Leere Medaillentabelle erstellen
medaillen.matrix <- matrix(0, nrow = length(unique(unlist(gewinner.tag))), 
                              ncol = length(gewinner.tag))
rownames(medaillen.matrix) <- unique(unlist(gewinner.tag))
colnames(medaillen.matrix) <- names(gewinner.tag)

# Anzahl der Medaillen pro Land und Medaillenart eintragen
medaillen.matrix[names(table(gewinner.tag$Gold)), "Gold"] <- 
  table(gewinner.tag$Gold)
medaillen.matrix[names(table(gewinner.tag$Silver)), "Silver"] <- 
  table(gewinner.tag$Silver)
medaillen.matrix[names(table(gewinner.tag$Bronze)), "Bronze"] <- 
  table(gewinner.tag$Bronze)

# Nach Gold, Silber und Bronze sortieren
tagesmedaillen <- medaillen.matrix[
  order(medaillen.matrix[ ,"Gold"],
        medaillen.matrix[ ,"Silver"],
        medaillen.matrix[ ,"Bronze"], decreasing = TRUE),
  , drop = FALSE]

tagesmedaillen



### 2d) daily.winners: Laender mit erster Medaille an einem Tag

# Laender mit Medaillen am gewaehlten Tag bestimmen
laender.tag <- unique(unlist(daily.winners[[gewaehlter.tag]], 
                      use.names = FALSE))

# Laender bis zum Vortag bestimmen
# Sonderfall Tag1: keine Laender des Vortages
laender.vortage <- unique(unlist(daily.winners[1:vortag], 
                                use.names = FALSE))[gewaehlter.tag > 1]

# Laender auswaehlen, die am gewaehlten Tag erstmals erscheinen
erste.medaille <- sort(laender.tag[!laender.tag %in% laender.vortage])

erste.medaille



### 2e) Jedes Land: An welchem Tag erste Medaille gewonnen?

# Laendernamen aller Tageslisten sammeln
laender.pro.tag <- lapply(daily.tables, rownames)

# Anzahl der Auftritte jedes Landes bestimmen
anzahl.auftreten <- table(unlist(laender.pro.tag, use.names = FALSE))

# Tag der ersten Medaille berechnen
erster.tag <- length(daily.tables) - anzahl.auftreten + 1

erster.tag



### 2f) Kreuztabelle Tag vs. Anzahl Medaillen eines Landes

# Land auswaehlen
gewaehlter.land <- "Individual Neutral Athletes"

# Medaillengewinner mit dem gewaehlten Land vergleichen
bool.land <- lapply(daily.winners, FUN = lapply, "==", gewaehlter.land)

# Anzahl der Gold-, Silber- und Bronzemedaillen pro Tag berechnen
medaillen.land <- t(sapply(bool.land, FUN = sapply, sum))

# Ergebnis mit dem kumulierten Medaillenspiegel ueberpruefen
letzte.tag <- daily.tables[[length(daily.tables)]]
all.equal(colSums(medaillen.land), letzte.tag[gewaehlter.land, ])

medaillen.land

