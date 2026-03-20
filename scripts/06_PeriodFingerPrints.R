
# average total religious frequency per period
df_period_summary <- df_rel_table %>%
  filter(!is.na(period)) %>%
  group_by(period) %>%
  summarise(
    mean_total    = mean(total_religious),
    sd_total      = sd(total_religious),
    n_books       = n(),
    .groups       = "drop"
  )

# bar chart with error bars
ggplot(df_period_summary, aes(x = period, y = mean_total, fill = period)) +
  geom_col(alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_total - sd_total, 
                    ymax = mean_total + sd_total), 
                width = 0.3) +
  scale_fill_viridis_d(option = "turbo") +
  labs(
    title = "Durchschnittliche religiöse Häufigkeit pro Zeitraum",
    x     = "Periode",
    y     = "Ø religiöse Begriffe pro 1000 Wörter"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



##Category profile per period — multidimensional radar/spider plot

require(ggradar)
require(scales)

# calculate mean frequency per category per period
df_radar <- df_rel_table %>%
  filter(!is.na(period)) %>%
  group_by(period) %>%
  summarise(
    institutionell = mean(institutionell),
    rituale        = mean(rituale),
    feiertage      = mean(feiertage),
    grundbegriffe  = mean(grundbegriffe),
    namen          = mean(namen),
    orte           = mean(orte),
    .groups        = "drop"
  ) %>%
  # rescale each category to 0-1 for radar plot
  mutate(across(where(is.numeric), rescale))

# radar plot — each period gets its own polygon
ggradar(
  plot.data =df_radar,
  values.radar           = c("0", "0.5", "1"),
  grid.min               = 0,
  grid.mid               = 0.5,
  grid.max               = 1,
  group.line.width       = 1,
  group.point.size       = 3,
  group.colours          = viridis::viridis(nrow(df_radar)),
  background.circle.colour = "white",
  gridline.mid.colour    = "grey",
  legend.position        = "right",
  plot.title = "Religiöses Kategorienprofil pro Zeitraum"
) 
