library(httr2)
library(jsonlite)
library(tidyverse)
library(readr)

API_KEY        <- Sys.getenv("NYT_API_KEY")
BASE_URL       <- "https://api.nytimes.com/svc/search/v2/articlesearch.json"
HEADLINES_PATH <- "data_raw/nyt_validation_headlines_v2.csv"
COUNTS_PATH    <- "data_raw/nyt_validation_counts_v2.csv"
MAX_PAGES      <- 10

# ── One row per term per validation year ──────────────────────────────────────
# Terms are called individually (no Boolean OR) to avoid the API parsing problem
# Deduplication happens at the end by headline text across all terms

validation_queries <- tribble(
  ~issue,                   ~year, ~term,
  
  # Abortion 
  "abortion",               1980,  "abortion",
  "abortion",               1980,  "pro-life",
  "abortion",               1980,  "pro-choice",
  "abortion",               1992,  "abortion",
  "abortion",               1992,  "pro-life",
  "abortion",               1992,  "pro-choice",
  "abortion",               2004,  "abortion",
  "abortion",               2004,  "pro-life",
  "abortion",               2004,  "pro-choice",
  "abortion",               2022,  "abortion",
  "abortion",               2022,  "pro-life",
  "abortion",               2022,  "pro-choice",
  "abortion",               2022,  "Dobbs",

  # Suicide permissibility
  "suicide_permissibility", 1980,  "euthanasia",
  "suicide_permissibility", 1980,  "assisted suicide",
  "suicide_permissibility", 1995,  "euthanasia",
  "suicide_permissibility", 1995,  "assisted suicide",
  "suicide_permissibility", 1995,  "right to die",
  "suicide_permissibility", 2005,  "euthanasia",
  "suicide_permissibility", 2005,  "assisted suicide",
  "suicide_permissibility", 2005,  "right to die",
  "suicide_permissibility", 2020,  "euthanasia",
  "suicide_permissibility", 2020,  "assisted suicide",
  "suicide_permissibility", 2020,  "death with dignity",

  # Police violence
  "police_violence",        1980,  "police brutality",
  "police_violence",        1991,  "police brutality",
  "police_violence",        1991,  "excessive force",
  "police_violence",        2014,  "police brutality",
  "police_violence",        2014,  "Black Lives Matter",
  "police_violence",        2020,  "police brutality",
  "police_violence",        2020,  "Black Lives Matter",
  "police_violence",        2020,  "defund the police",

  # Gender roles
  "gender_roles",           1980,  "feminism",
  "gender_roles",           1980,  "women's rights",
  "gender_roles",           1992,  "feminism",
  "gender_roles",           1992,  "equal pay",
  "gender_roles",           2004,  "feminism",
  "gender_roles",           2004,  "gender equality",
  "gender_roles",           2017,  "feminism",
  "gender_roles",           2017,  "gender equality",
  "gender_roles",           2017,  "equal pay"
)

# ── Helper: fetch one page for one term ───────────────────────────────────────

fetch_page <- function(term, year, page, api_key) {

  resp <- request(BASE_URL) |>
    req_url_query(
      q          = term,
      begin_date = paste0(year, "0101"),
      end_date   = paste0(year, "1231"),
      page       = page,
      `api-key`  = api_key
    ) |>
    req_retry(max_tries = 3, backoff = ~ 15) |>
    req_perform()

  Sys.sleep(12)

  content <- resp |> resp_body_json()
  docs     <- content$response$docs

  if (length(docs) == 0) return(NULL)

  list(
    headlines  = map_chr(docs, ~ .x$headline$main %||% NA_character_),
    total_hits = content$response$meta$hits
  )
}

# ── Load existing progress ────────────────────────────────────────────────────

if (file.exists(HEADLINES_PATH)) {
  all_headlines <- read_csv(HEADLINES_PATH, show_col_types = FALSE)
  cat("Resuming -", nrow(all_headlines), "rows already collected\n")
} else {
  all_headlines <- tibble(
    issue    = character(),
    year     = integer(),
    term     = character(),
    page     = integer(),
    headline = character()
  )
}

# ── Collection loop ───────────────────────────────────────────────────────────

for (i in seq_len(nrow(validation_queries))) {

  row <- validation_queries[i, ]

  # Skip if already collected
  already_done <- all_headlines |>
    filter(issue == row$issue, year == row$year, term == row$term, page == 0) |>
    nrow()

  if (already_done > 0) {
    cat(sprintf("%-25s %d %-20s - skipping\n",
                row$issue, row$year, row$term))
    next
  }

  cat(sprintf("%-25s %d %-25s pages: ", row$issue, row$year, row$term))

  for (pg in 0:(MAX_PAGES - 1)) {

    result <- tryCatch(
      fetch_page(row$term, row$year, pg, API_KEY),
      error = function(e) {
        cat(sprintf("\n  ERROR page %d: %s\n", pg, conditionMessage(e)))
        NULL
      }
    )

    if (is.null(result) || length(result$headlines) == 0) break

    cat(pg, "")

    all_headlines <- bind_rows(all_headlines, tibble(
      issue    = row$issue,
      year     = row$year,
      term     = row$term,
      page     = pg,
      headline = result$headlines
    ))

    write_csv(all_headlines, HEADLINES_PATH)

    if ((pg + 1) * 10 >= result$total_hits) break
  }

  cat("\n")
}

cat("\n✓ Collection complete\n")

# ── Deduplicate across terms within each issue-year ───────────────────────────
# An article appearing under both "abortion" and "pro-choice" searches
# will have the same headline text and be counted only once after distinct()

validation_counts <- all_headlines |>
  filter(!is.na(headline)) |>
  distinct(issue, year, headline) |>    # deduplication happens here
  count(issue, year, name = "count_deduplicated")

write_csv(validation_counts, COUNTS_PATH)
cat("✓ Deduplicated counts saved to", COUNTS_PATH, "\n")
print(validation_counts)