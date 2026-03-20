# ============================================================
# 6. FREQUENZANALYSE
# Häufigkeiten der religiösen Kategorien werden berechnet,
# absolut und relativ (pro 1000 Wörter)
# ============================================================

# Document-Feature-Matrix erstellen und auf Wörterliste einschränken
dfm_all    <- dfm(tokens_lemma)
dfm_listen <- dfm_lookup(dfm_all, dictionary = listen)

# Absolute Häufigkeiten
df_table <- convert(dfm_listen, to = "data.frame")

#absolute Zahlen exportieren
write.csv(df_table,"C:/Users/laura/OneDrive/Skrivbord/AnnasProjekt/dfm_all.csv")

####TABELLE MIT ABSOLUTEN WERTEN ERSTELLEN####
require(flextable)
require(officer)

#columns erschaffen, jahr und autor aus dateinamen extrahieren
df_table$year   <- as.numeric(str_extract(df_table$doc_id, "\\d{4}"))
df_table$author <- str_extract(df_table$doc_id, "^[a-z]+")
df_table$total_religious <- rowSums(df_table[, c("institutionell", "rituale", "feiertage", "grundbegriffe", "namen", "orte")])


#summary table erstellen, eine reihe pro dok, 2 dezimalstellen
df_summary <- df_table %>%
  select(doc_id, year, author, institutionell, rituale, feiertage, grundbegriffe, namen, orte, total_religious) %>%
  mutate(across(where(is.numeric), ~ round(.,2))) %>%
  arrange(year)

#flextable daraus machen
ft <- flextable(df_summary) %>%
  set_header_labels(
    doc_id = "Dokument",
    year = "Jahr",
    author = "Autor",
    institutionell = "Institutionell",
    rituale = "Rituale",
    feiertage = "Feiertage",
    grundbegriffe = "Grundbegriffe",
    namen = "Namen", 
    orte = "Orte",
    total_religious = "Gesamt"
  ) %>%
  theme_vanilla() %>%
  autofit()

#zu word exportieren
doc <- read_docx() %>% 
  body_add_par("Religiöse Begriffe in deutscher Dichtung - Absolute Zahlen", style = "heading 1") %>%
  body_add_flextable(ft)
print (doc, target = "C:/Users/laura/OneDrive/Skrivbord/AnnasProjekt/TabelleAbsFreq.docx")
    


# Relative Häufigkeiten (pro 1000 Wörter)
####dfm_rel1000  <- dfm_weight(dfm_listen, scheme = "prop") * 1000
####df_rel_table <- convert(dfm_rel1000, to = "data.frame")
####das hier wären dann werte pro 1000 reli-begriffe, nicht pro 1000 wörter im dok

#total amount of tokens per doc
total_tokens <- ntoken(dfm_all)

#divide category counts by total tokens and *1000
dfm_rel1000 <- dfm_listen/ total_tokens *1000
df_rel_table <- convert(dfm_rel1000, to ="data.frame")

# Jahr und Autor aus Dateinamen extrahieren
df_rel_table$year   <- as.numeric(str_extract(df_rel_table$doc_id, "\\d{4}"))
df_rel_table$author <- str_extract(df_rel_table$doc_id, "^[a-z]+")

# Dokumente chronologisch sortieren
df_rel_table        <- df_rel_table[order(df_rel_table$year), ]
df_rel_table$doc_id <- factor(df_rel_table$doc_id, levels = df_rel_table$doc_id)

# Jahr- und Titelkombination als Label für die x-Achse
df_rel_table$label <- paste0(df_rel_table$year, " – ", df_rel_table$doc_id)

# Ins Long-Format umwandeln (für ggplot)
df_long <- pivot_longer(
  df_rel_table,
  cols      = c(institutionell, rituale, feiertage, grundbegriffe, namen, orte),
  names_to  = "category",
  values_to = "frequency"
)
df_long       <- df_long[order(df_long$year), ]
df_long$label <- factor(df_long$label, levels = unique(df_long$label[order(df_long$year)]))

# Metadaten auch in tokens_lemma hinterlegen (für Kollokationsanalyse)
docvars(tokens_lemma, "year")   <- df_rel_table$year
docvars(tokens_lemma, "author") <- df_rel_table$author
docvars(tokens_lemma, "period") <- cut(
  df_rel_table$year,
  breaks = c(1599, 1650, 1700, 1750, 1800, 1850, 1900, 1950),
  labels = c("1600-1650", "1651-1700", "1701-1750", "1751-1800", "1801-1850", "1851-1900", "1901-1950")
)

categories <- c("institutionell", "rituale", "feiertage", "grundbegriffe", "namen")
df_rel_table_z <- df_rel_table
df_rel_table_z[, categories] <- scale(df_rel_table[, categories])

df_rel_table$total_religious <- rowSums(df_rel_table[, c("institutionell", "rituale", "feiertage", "grundbegriffe", "namen", "orte")])

# 1. add period to df_rel_table
df_rel_table$period <- cut(df_rel_table$year,
                           breaks = c(1599, 1650, 1700, 1750, 1800, 1850, 1900, 1950),
                           labels = c("1600-1650", "1651-1700", "1701-1750", "1751-1800", "1801-1850", "1851-1900", "1901-1950"))

#relative werte als dokument exportieren####

#summary table erstellen, eine reihe pro dok, 2 dezimalstellen
df_summary <- df_rel_table %>%
  select(doc_id, year, author, institutionell, rituale, feiertage, grundbegriffe, namen, orte, total_religious) %>%
  mutate(across(where(is.numeric), ~ round(.,2))) %>%
  arrange(year)

#flextable daraus machen
ft <- flextable(df_summary) %>%
  set_header_labels(
    doc_id = "Dokument",
    year = "Jahr",
    author = "Autor",
    institutionell = "Institutionell",
    rituale = "Rituale",
    feiertage = "Feiertage",
    grundbegriffe = "Grundbegriffe",
    namen = "Namen", 
    orte = "Orte",
    total_religious = "Gesamt"
  ) %>%
  theme_vanilla() %>%
  autofit()

#zu word exportieren
doc <- read_docx() %>% 
  body_add_par("Religiöse Begriffe in deutscher Dichtung - Absolute Zahlen", style = "heading 1") %>%
  body_add_flextable(ft)
print (doc, target = "C:/Users/laura/OneDrive/Skrivbord/AnnasProjekt/TabelleRelFreq.docx")
