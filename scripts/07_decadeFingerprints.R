# ── create decade column ───────────────────────────────────────────────────
df_rel_table$decade <- paste0(floor(df_rel_table$year / 10) * 10, "s")

# ── 1. average total religious frequency per decade ───────────────────────
df_decade_summary <- df_rel_table %>%
  filter(!is.na(decade)) %>%
  group_by(decade) %>%
  summarise(
    mean_total = mean(total_religious),
    sd_total   = sd(total_religious),
    n_books    = n(),
    .groups    = "drop"
  )

ggplot(df_decade_summary, aes(x = decade, y = mean_total, fill = decade)) +
  geom_col(alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_total - sd_total,
                    ymax = mean_total + sd_total),
                width = 0.3) +
  scale_fill_viridis_d(option = "turbo") +
  labs(
    title = "Durchschnittliche religiöse Häufigkeit pro Jahrzehnt",
    x     = "Jahrzehnt",
    y     = "Ø religiöse Begriffe pro 1000 Wörter"
  ) +
  theme_minimal() +
  theme(
    legend.position  = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

