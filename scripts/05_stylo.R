
####MFW UND RELIGIÖSE FREQUENZEN #######
# ── stylometric vs religious frequency scatterplot ────────────────────────

# 1. get the 200 most frequent words across the corpus (stylometric features)
mfw_200 <- names(topfeatures(dfm_all, 200))

# 2. calculate per-document scores

# general stylometric score: mean frequency of the 200 MFW
# (captures overall style/register)
dfm_mfw_docs <- dfm_select(dfm_all, pattern = mfw_200)
stylo_scores <- stylo_scores <- pca_result$x[, 1]

# religious score: total religious terms per 1000 words (already in df_rel_table)
relig_scores <- rowSums(df_rel_table[, c("institutionell", "rituale", "feiertage",
                                         "grundbegriffe", "namen", "orte")])

# 3. combine into one dataframe
stylo_relig_df <- data.frame(
  doc_id     = df_rel_table$doc_id,
  year       = df_rel_table$year,
  author     = df_rel_table$author,
  period     = df_rel_table$period,
  stylo_score = stylo_scores,
  relig_score = relig_scores
)

# ── visualisation 1: overall scatterplot ──────────────────────────────────
ggplot(stylo_relig_df, aes(x = stylo_score, y = relig_score, 
                           color = period, label = as.character(year))) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text(size = 2.5, vjust = -0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  scale_color_viridis_d(option = "turbo") +
  labs(
    title = "Stilometrie vs. religiöse Sprache",
    x     = "Stilometrischer Score (MFW pro 1000 Wörter)",
    y     = "Religiöse Begriffe pro 1000 Wörter",
    color = "Periode"
  ) +
  theme_minimal()

