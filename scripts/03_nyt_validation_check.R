library(tidyverse)
library(readr)

# ── Load both datasets ────────────────────────────────────────────────────────

primary    <- read_csv("data_raw/nyt_primary_counts.csv",    show_col_types = FALSE)
validation <- read_csv("data_raw/nyt_validation_counts_v2.csv", show_col_types = FALSE)

# ── Join on validation years only ────────────────────────────────────────────

comparison <- primary |>
  inner_join(validation, by = c("issue", "year")) |>
  rename(
    primary_hits        = total_hits,
    deduplicated_count  = count_deduplicated
  )

cat("── Comparison table ──\n")
print(comparison)

# ── Correlation per issue ─────────────────────────────────────────────────────

cat("\n── Pearson correlation by issue ──\n")
correlations <- comparison |>
  group_by(issue) |>
  summarise(
    r        = cor(primary_hits, deduplicated_count, use = "complete.obs"),
    n_years  = n(),
    .groups  = "drop"
  )

print(correlations)

# ── Interpretation guide printed to console ───────────────────────────────────

cat("\n── Interpretation ──\n")
cat("r > 0.90 : excellent - primary measure is robust, proceed with confidence\n")
cat("r 0.70-0.90 : acceptable - note limitation, proceed\n")
cat("r < 0.70 : investigate - trend shapes diverge, reconsider primary term\n")

# NOTE: Abortion correlation (r = 0.523) is artificially low due to pagination
# ceiling in validation script. For high-salience years (1992, 2004, 2022),
# all three validation terms hit the 100-article cap, meaning deduplicated
# counts reflect only ~100 articles while total_hits captures the true volume
# of 1,000-3,000+ articles. The primary measure is more reliable for abortion
# than the correlation suggests. This limitation is documented in methods.

# ── Plot for visual inspection ────────────────────────────────────────────────

comparison |>
  ggplot(aes(x = primary_hits, y = deduplicated_count, label = year)) +
  geom_point(size = 3, colour = "#2c7bb6") +
  geom_smooth(method = "lm", se = FALSE, colour = "grey50", linewidth = 0.7) +
  ggrepel::geom_text_repel(size = 3) +
  facet_wrap(~ issue, scales = "free") +
  labs(
    title    = "Validation: Primary total_hits vs deduplicated multi-term counts",
    subtitle = "Each point is one validation year",
    x        = "Primary measure (total_hits, single term)",
    y        = "Validation measure (deduplicated headline count)"
  ) +
  theme_minimal()

ggsave("data_raw/nyt_validation_plot.png", width = 10, height = 7, dpi = 150)
cat("\n✓ Plot saved to data_raw/nyt_validation_plot.png\n")