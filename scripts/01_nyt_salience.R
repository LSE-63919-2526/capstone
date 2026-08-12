library(httr2)
library(jsonlite)
library(tidyverse)
library(readr)

API_KEY  <- Sys.getenv("NYT_API_KEY")
BASE_URL <- "https://api.nytimes.com/svc/search/v2/articlesearch.json"
OUTPUT_PATH <- "data_raw/nyt_primary_counts.csv"

# ── Primary terms: one per issue ──────────────────────────────────────────────
# These are chosen as the single most unambiguous representative term
# per issue. Rationale for each should be documented in your methods:
#   abortion         - dominant, unambiguous, consistent across all years
#   euthanasia       - more precise than "suicide" for the moral debate
#   police brutality - consistent pre/post 2014, avoids movement-specific terms
#   feminism         - broadest consistent term for gender role debate

primary_terms <- tribble(
  ~issue,                   ~term,              ~start, ~end,
  "abortion",               "abortion",          1972,   2024,
  "suicide_permissibility", "euthanasia",        1972,   1989,
  "suicide_permissibility", "assisted suicide",  1990,   2024,
  "police_violence",        "police brutality",  1972,   2024,
  "gender_roles",           "feminism",          1972,   2024
)

# ── Helper: get total_hits for one term, one year ─────────────────────────────

get_total_hits <- function(term, year, api_key) {

  resp <- request(BASE_URL) |>
    req_url_query(
      q          = term,
      begin_date = paste0(year, "0101"),
      end_date   = paste0(year, "1231"),
      `api-key`  = api_key
    ) |>
    req_retry(max_tries = 3, backoff = ~ 15) |>
    req_perform()

  Sys.sleep(12)  # do not reduce - NYT rate limit is strict

  resp |> resp_body_json() |> _$response$meta$hits
}

# ── Load existing progress if resuming ───────────────────────────────────────

if (file.exists(OUTPUT_PATH)) {
  results <- read_csv(OUTPUT_PATH, show_col_types = FALSE)
  cat("Resuming -", nrow(results), "rows already collected\n")
} else {
  results <- tibble(
    issue = character(),
    term  = character(),
    year  = integer(),
    total_hits = integer()
  )
}

# ── Main loop ─────────────────────────────────────────────────────────────────

for (i in seq_len(nrow(primary_terms))) {

  row   <- primary_terms[i, ]
  years <- seq(row$start, row$end)

  cat(sprintf("\n── %s ('%s') ──\n", row$issue, row$term))

  for (yr in years) {

    # Skip if already collected
    already_done <- results |>
      filter(issue == row$issue, year == yr) |>
      nrow()

    if (already_done > 0) {
      cat(sprintf("  %d - skipping (already collected)\n", yr))
      next
    }

    cat(sprintf("  %d ... ", yr))

    hits <- tryCatch(
      get_total_hits(row$term, yr, API_KEY),
      error = function(e) {
        cat("ERROR:", conditionMessage(e), "\n")
        NA_integer_
      }
    )

    cat(hits, "\n")

    results <- results |>
      add_row(
        issue      = row$issue,
        term       = row$term,
        year       = yr,
        total_hits = hits
      )

    write_csv(results, OUTPUT_PATH)
  }
}

cat("\n✓ Primary collection complete. Saved to", OUTPUT_PATH, "\n")