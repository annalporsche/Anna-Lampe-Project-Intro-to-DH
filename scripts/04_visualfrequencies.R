# ============================================================
# 7. VISUALISIERUNGEN – FREQUENZEN
# ============================================================

all_religious_terms <- c(institut_liste, rituale_liste, feiertage_liste, grundbegriffe_liste, namen_liste, orte_liste)

# Streudiagramm: Grundbegriffe über die Zeit (mit Trendlinie)
ggplot(df_rel_table, aes(x = year, y = grundbegriffe)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    x     = "Jahr",
    y     = "Grundbegriffe pro 1000 Wörter",
    title = "Religiöse Referenzen in deutscher Dichtung - Grundbegriffe (1600–1900)"
  )

# Streudiagramm: Instit. begriffe über die Zeit (mit Trendlinie)
ggplot(df_rel_table, aes(x = year, y = institutionell)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    x     = "Jahr",
    y     = "Grundbegriffe pro 1000 Wörter",
    title = "Religiöse Referenzen in deutscher Dichtung - institutionelle Begriffe (1600–1900)"
  )

# Streudiagramm: Feiertage über die Zeit (mit Trendlinie)
ggplot(df_rel_table, aes(x = year, y = feiertage)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    x     = "Jahr",
    y     = "Grundbegriffe pro 1000 Wörter",
    title = "Religiöse Referenzen in deutscher Dichtung - Feiertage (1600–1900)"
  )

# Streudiagramm: Ritualbegriffe über die Zeit (mit Trendlinie)
ggplot(df_rel_table, aes(x = year, y = rituale)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    x     = "Jahr",
    y     = "Grundbegriffe pro 1000 Wörter",
    title = "Religiöse Referenzen in deutscher Dichtung - Ritualbegriffe (1600–1900)"
  )

# Streudiagramm: Ortsbegriffe über die Zeit (mit Trendlinie)
ggplot(df_rel_table, aes(x = year, y = orte)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    x     = "Jahr",
    y     = "Grundbegriffe pro 1000 Wörter",
    title = "Religiöse Referenzen in deutscher Dichtung - Orte (1600–1900)"
  )

# Streudiagramm: biblische Namen über die Zeit (mit Trendlinie)
ggplot(df_rel_table, aes(x = year, y = namen)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    x     = "Jahr",
    y     = "Grundbegriffe pro 1000 Wörter",
    title = "Religiöse Referenzen in deutscher Dichtung - Namen (1600–1900)"
  )
#gesamtwerte Begriffe mit Trendlinie

ggplot(df_rel_table, aes(x=year, y =total_religious)) + 
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    x= "jahr",
    y= "Religiöse Begriffe pro 1000 Wörter",
    title= "Religiöse Referenzen in deutscher Dichtung (1600-1900)"
    
  )

# Balkendiagramm: Alle Kategorien pro Werk (facettiert, chronologisch)
ggplot(df_long, aes(x = factor(year), y = frequency, fill = category)) +
  geom_col() +
  facet_wrap(~ category, scales = "free_y", ncol = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
  labs(
    x    = "Text (chronologisch)",
    y    = "Häufigkeit pro 1000 Wörter",
    fill = "Kategorie"
  )

