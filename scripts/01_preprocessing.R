# ============================================================
# Religiöse Sprache in deutscher Dichtung – Korpusanalyse
# ============================================================
setwd("C:/Users/laura/OneDrive/Skrivbord/AnnasProjekt/corpusNeu")
# --- Pakete laden ---

install.packages("quanteda")
install.packages("readtext")
install.packages("stringr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggridges")
install.packages("proxy")
install.packages("quanteda.textstats")


require(quanteda)
require(readtext)
require(stringr)
require(ggplot2)
require(tidyr)
require(dplyr)
require(ggridges)
require(proxy)
require(quanteda.textstats)

# ============================================================
# 1. KORPUS EINLESEN
# ============================================================

texts <- readtext::readtext("C:/Users/laura/OneDrive/Skrivbord/AnnasProjekt/corpusNeu")


# ============================================================
# 2. ORTHOGRAPHISCHE NORMALISIERUNG
# Historische Schreibvarianten werden auf moderne
# Standardformen vereinheitlicht
# ============================================================

texts$text <- gsub("cathol",                    "kathol",      texts$text, ignore.case = TRUE)
texts$text <- gsub("closter",                   "kloster",     texts$text, ignore.case = TRUE)
texts$text <- gsub("bethleen",                  "bethlehem",   texts$text, ignore.case = TRUE)
texts$text <- gsub("kirke",                     "kirche",      texts$text, ignore.case = TRUE)
texts$text <- gsub("nazaret",                   "nazareth",    texts$text, ignore.case = TRUE)
texts$text <- gsub("teuffel|teuffell",          "teufel",      texts$text, ignore.case = TRUE)
texts$text <- gsub("gnedig",                    "gnädig",      texts$text, ignore.case = TRUE)
texts$text <- gsub("himlisch|hymmlisch|hymlisch","himmlisch",  texts$text, ignore.case = TRUE)
texts$text <- gsub("behten",                    "beten",       texts$text, ignore.case = TRUE)
texts$text <- gsub("gebeht",                    "gebet",       texts$text, ignore.case = TRUE)
texts$text <- gsub("opffer",                    "opfer",       texts$text, ignore.case = TRUE)
texts$text <- gsub("goliath",                   "goliat",      texts$text, ignore.case = TRUE)
texts$text <- gsub("lucifer",                   "luzifer",     texts$text, ignore.case = TRUE)
texts$text <- gsub("paradis|paradeiß|paradiß|paradieß", "paradies", texts$text, ignore.case = TRUE)
texts$text <- gsub("solomon",                   "salomon",     texts$text, ignore.case = TRUE)
texts$text <- gsub("samson",                    "simson",      texts$text, ignore.case = TRUE)
texts$text <- gsub("seelig",                    "selig",       texts$text, ignore.case = TRUE)
texts$text <- gsub("communion",                 "kommunion",   texts$text, ignore.case = TRUE)
texts$text <- gsub("confirmation",              "konfirmation",texts$text, ignore.case = TRUE)
texts$text <- gsub("hierusalem",                "jerusalem",   texts$text, ignore.case = TRUE)
texts$text <- gsub("teuflich",                  "teuflisch",   texts$text, ignore.case = TRUE)

# Vokalersetzungen (ae/oe/ue/ey) und langes ſ
texts$text <- gsub("ae", "ä", texts$text, ignore.case = TRUE)
texts$text <- gsub("oe", "ö", texts$text, ignore.case = TRUE)
texts$text <- gsub("ue", "ü", texts$text, ignore.case = TRUE)
texts$text <- gsub("ey", "ei", texts$text, ignore.case = TRUE)
texts$text <- gsub("himel", "himmel", texts$text, ignore.case = TRUE)
texts$text <- gsub("ſ",    "s",       texts$text, ignore.case = TRUE)


# ============================================================
# 3. WÖRTERLISTEN (religiöse Terminologie nach Kategorie)
# ============================================================

institut_liste <- tolower(c(
  "Altar", "Abt", "Abtei", "Äbtissin", "Beichtstuhl", "Bischof", "Baptist",
  "Ablass", "Bistum", "Dekan", "Dom", "Diakon", "Domherr", "Diakonisse",
  "Eucharistie", "Erzbischof", "Gemeinde", "evangelisch", "Kantate", "Kirche",
  "Kirchlich", "Kapelle", "Kanzel", "Kirchturm", "Kantor", "Kardinal",
  "Kathedrale", "Kaplan", "Katholik", "katholisch", "Kloster", "Konfession",
  "Kollekte", "Klerus", "Kommunion", "Konfirmation", "Liturgie", "Luther",
  "Lutheraner", "Mönch", "Nonne", "Ökumene", "Orden", "Papst", "Priester",
  "Pfarrer", "Pastor", "Pater", "Predigt", "Prediger", "Protestant",
  "protestantisch", "Prozession", "Reformation", "reformiert", "Reliquie",
  "Requiem", "Sakrament", "Sakristei", "Sekte", "Vikar"
))

rituale_liste <- tolower(c(
  "Andacht", "Abendmahl", "anbeten", "Anbetung", "Abendmesse", "Absolution",
  "Beichte", "Begräbnis", "Beerdigung", "beichten", "Buße", "büßen", "beten",
  "fasten", "Fürbitte", "Gottesdienst", "Gebet", "pilgern", "Salbung",
  "salben", "segnen", "Taufe", "taufen", "Wallfahrt"
))

feiertage_liste <- tolower(c(
  "Advent", "Fronleichnam", "Gründonnerstag", "Heiligabend", "Himmelfahrt",
  "Karfreitag", "Karsamstag", "Karwoche", "Ostern", "Pfingsten", "Schabbat", "Sabbat"
))

grundbegriffe_liste <- tolower(c(
  "Arche", "Aberglaube", "abergläubig", "Amen", "Auferstehung", "auferstehen",
  "Bibel", "biblisch", "Bekehrung", "bekehren", "Apostel", "Christ", "christlich",
  "Christus", "Christentum", "Cherub", "Dämon", "dämonisch", "dreieinig",
  "Dreieinigkeit", "Dornenkrone", "Erlöser", "Erlösung", "erlösen", "Engel",
  "Evangelium", "Ewigkeit", "ewig", "Erzengel", "Fegefeuer", "fromm",
  "Frömmigkeit", "Gott", "göttlich", "Geist", "geistlich", "geistig", "Gnade",
  "gnädig", "Gebot", "Genesis", "Halleluja", "Hölle", "höllisch", "heilig",
  "Heil", "Häresie", "Heide", "Heiland", "himmlisch", "Jahwe", "Jünger",
  "Jesus", "Jesu", "Ketzer", "ketzerei", "ketzerisch", "Kreuzigung", "Krippe",
  "Kruzifix", "Losung", "Luzifer", "Manna", "Messias", "Nächstenliebe",
  "Offenbarung", "Opfer", "opfern", "Orgel", "Paradies", "paradiesisch",
  "Pharisäer", "Pilgerweg", "Prophet", "prophetisch", "Psalm", "Religion",
  "religiös", "Rosenkranz", "Rosarium", "Seele", "Seelisch", "Sünde", "sündig",
  "Sünder", "Segen", "Sühne", "selig", "Seligkeit", "Sintflut", "Tempel",
  "Trinität", "Teufel", "teuflisch", "Vaterunser", "Weihe", "weihen", "Wunder",
  "Wiedergeburt", "Wiedergeboren", "Zölibat", "Zungenrede"
))

namen_liste <- tolower(c(
  "Hiob", "Judas", "Pilatus", "Petrus", "Maria", "Abraham", "Adam", "Eva",
  "Kain", "Abel", "Salomon", "David", "Goliat", "Isaak", "Lazarus",
  "Matthäus", "Paulus", "Simson", "Saul"
))

orte_liste <- tolower(c(
  "Nazareth", "Bethlehem", "Babylon", "Babel", "Jerusalem",
  "Israel", "Juda", "Zion", "Bethel"
))

# Alle Kategorien zu einem quanteda-Wörterbuch zusammenfassen
listen <- dictionary(list(
  institutionell = institut_liste,
  rituale        = rituale_liste,
  feiertage      = feiertage_liste,
  grundbegriffe  = grundbegriffe_liste,
  namen          = namen_liste,
  orte           = orte_liste
))

corp <- corpus(texts)
