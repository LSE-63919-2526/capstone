library(tidyverse)
library(readr)

# ── Load raw NYT primary counts ───────────────────────────────────────────────

nyt_raw <- read_csv("data_raw/nyt_primary_counts.csv", show_col_types = FALSE)

cat("Raw NYT data dimensions:", nrow(nyt_raw), "x", ncol(nyt_raw), "\n")
cat("Issues:", unique(nyt_raw$issue), "\n")
cat("Year range:", min(nyt_raw$year), "to", max(nyt_raw$year), "\n")

# ── Check for missing years per issue ─────────────────────────────────────────

cat("\n── Coverage check ──\n")
nyt_raw |>
  group_by(issue) |>
  summarise(
    n_years    = n(),
    year_min   = min(year),
    year_max   = max(year),
    n_missing  = sum(is.na(total_hits)),
    n_zero     = sum(total_hits == 0, na.rm = TRUE),
    .groups    = "drop"
  ) |>
  print()

# ── Flag any suspiciously low counts ─────────────────────────────────────────
# Zero counts are almost certainly API errors not genuine absence of coverage

cat("\n── Zero or NA counts (likely API errors) ──\n")
nyt_raw |>
  filter(is.na(total_hits) | total_hits == 0) |>
  print()

# ── Normalize to 0-100 within each issue ─────────────────────────────────────
# Normalization is within-issue so values are comparable across time
# but NOT directly comparable across issues (by design)
# Formula: (x - min) / (max - min) * 100

nyt_normalized <- nyt_raw |>
  filter(!is.na(total_hits)) |>
  group_by(issue) |>
  mutate(
    nyt_normalized = (total_hits - min(total_hits, na.rm = TRUE)) /
      (max(total_hits, na.rm = TRUE) - min(total_hits, na.rm = TRUE)) * 100
  ) |>
  ungroup() |>
  select(issue, term, year, total_hits, nyt_normalized)

write_csv(nyt_normalized, "data_processed/nyt_normalized.csv")
cat("\n✓ Normalized NYT data saved to data_processed/nyt_normalized.csv\n")

# ── NYT-Google Trends correlation check for 2004-2024 ────────────────────────

gtrends <- read_csv("data_processed/google_trends_normalized.csv",
                    show_col_types = FALSE)

# Join on issue and year for overlap period
overlap <- nyt_normalized |>
  filter(year >= 2004) |>
  inner_join(
    gtrends |> select(issue, year, gtrends_normalized),
    by = c("issue", "year")
  )

cat("\n── NYT vs Google Trends correlation (2004-2024) ──\n")
overlap |>
  group_by(issue) |>
  summarise(
    r       = cor(nyt_normalized, gtrends_normalized, use = "complete.obs"),
    n_years = n(),
    .groups = "drop"
  ) |>
  print()

# ── Build final combined salience file ───────────────────────────────────────
# One row per issue per year, 1972-2024
# NYT normalized is your primary salience variable throughout
# Google Trends included as supplementary column for 2004-2024

final_salience <- nyt_normalized |>
  left_join(
    gtrends |> select(issue, year, gtrends_normalized),
    by = c("issue", "year")
  ) |>
  arrange(issue, year)

write_csv(final_salience, "data_processed/salience_final.csv")
cat("✓ Final salience index saved to data_processed/salience_final.csv\n")

# ── Sanity check: plot all four issues ───────────────────────────────────────

final_salience |>
  ggplot(aes(x = year, y = nyt_normalized)) +
  geom_line(colour = "#2c7bb6", linewidth = 0.8) +
  geom_line(
    data = final_salience |> filter(!is.na(gtrends_normalized)),
    aes(y = gtrends_normalized),
    colour = "#d7191c", linewidth = 0.8
  ) +
  facet_wrap(~ issue, scales = "free_y") +
  labs(
    title    = "Salience over time by issue",
    subtitle = "Blue = NYT normalized (1972-2024)  |  Red = Google Trends (2004-2024)",
    x        = "Year",
    y        = "Normalized salience (0-100)"
  ) +
  theme_minimal()

ggsave("figures/fig_j_salience_plot.png", width = 12, height = 7, dpi = 150)
cat("✓ Salience plot saved to figures/fig_j_salience_plot.png\n")