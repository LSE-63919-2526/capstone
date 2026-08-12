library(gtrendsR)
library(tidyverse)
library(readr)

OUTPUT_PATH <- "data_raw/google_trends_raw.csv"

# ── Search terms: matched to NYT primary terms ────────────────────────────────

terms <- c(
  "abortion",
  "assisted suicide",
  "police brutality",
  "feminism"
)

issue_labels <- c(
  "abortion"         = "abortion",
  "assisted suicide" = "suicide_permissibility",
  "police brutality" = "police_violence",
  "feminism"         = "gender_roles"
)

# ── Helper: fetch one term, full 2004-2024 range ──────────────────────────────
# gtrendsR returns weekly data for long ranges so we aggregate to annual
# geo = "US" restricts to United States searches only

fetch_trends <- function(term) {

  cat(sprintf("Fetching: %s ... ", term))

  result <- tryCatch(
    gtrends(
      keyword    = term,
      geo        = "US",
      time       = "2004-01-01 2024-12-31",
      onlyInterest = TRUE
    ),
    error = function(e) {
      cat("ERROR:", conditionMessage(e), "\n")
      return(NULL)
    }
  )

  if (is.null(result)) return(NULL)

  # Extract interest over time, aggregate weekly to annual mean
  annual <- result$interest_over_time |>
    mutate(year = as.integer(format(as.Date(date), "%Y"))) |>
    group_by(year) |>
    summarise(
      gtrends_score = mean(hits, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(term = term)

  cat("done\n")
  Sys.sleep(5)  # be polite to Google's servers

  return(annual)
}

# ── Collect all terms ─────────────────────────────────────────────────────────

all_trends <- map(terms, fetch_trends) |>
  bind_rows() |>
  mutate(issue = issue_labels[term]) |>
  select(issue, term, year, gtrends_score)

write_csv(all_trends, OUTPUT_PATH)
cat("\n✓ Raw Google Trends data saved to", OUTPUT_PATH, "\n")

# ── Normalize to 0-100 within each issue ─────────────────────────────────────
# Google Trends already returns 0-100 relative to each term's own peak
# Renormalize within our 2004-2024 window for consistency with NYT

trends_normalized <- all_trends |>
  group_by(issue) |>
  mutate(
    gtrends_normalized = (gtrends_score - min(gtrends_score, na.rm = TRUE)) /
      (max(gtrends_score, na.rm = TRUE) - min(gtrends_score, na.rm = TRUE)) * 100
  ) |>
  ungroup()

write_csv(trends_normalized, "data_processed/google_trends_normalized.csv")
cat("✓ Normalized data saved to data_processed/google_trends_normalized.csv\n")

# ── Sanity check: print annual scores for abortion ───────────────────────────

cat("\n── Sanity check: abortion Google Trends by year ──\n")
trends_normalized |>
  filter(issue == "abortion") |>
  select(year, gtrends_score, gtrends_normalized) |>
  print(n = 21)