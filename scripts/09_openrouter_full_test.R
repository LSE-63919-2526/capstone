# ==============================================================================
# 09_openrouter_full_test.R
#
# PURPOSE
# Full Condition A data collection across all 11 models in the final list
# (verbatim GSS wording, "typical American adult" persona). Covers all four
# issues (abortion, gender roles, police violence, suicide/euthanasia), each
# variable run across every year the GSS actually fielded it. This is the
# real study run, not a smoke test - the pipeline itself was validated in
# 08_openrouter_condition_a_pilot.R on the 3 cheapest models first.
#
# WHAT CHANGED FROM 08 (THE SMOKE TEST) AND WHY
#   1. Model list expanded from the 3 cheapest models to all 11 in the final
#      list. Every slug was verified directly against OpenRouter's live model
#      catalog (GET https://openrouter.ai/api/v1/models) (two of them were 
#      ambiguous enough that guessing would have silently pointed to the wrong 
#      model: Gemma has both a paid "google/gemma-4-31b-it" and a 
#      separately-hosted, rate-limited "google/gemma-4-31b-it:free"; Mistral 
#      Small 4's actual slug is "mistral-small-2603", not "mistral-small-4" as 
#      the model's display name would suggest. Using the paid, non-free-tier 
#      variant of each keeps pricing consistent with the original cost estimate 
#      and avoids free-tier rate limits interrupting a run this size.
#   2. Abortion wording pulled directly from the pilot_prompts.csv (the file
#      used for the manual Claude/Gemini/ChatGPT pilot) - confirmed
#      identical to what was used in the smoke test, so nothing drifted.
#   3. Gender roles, police violence, and suicide/euthanasia wording pulled
#      from the official GSS codebook/questionnaires (gss.norc.org,
#      thearda.com), since none of the uploaded files contained the full
#      verbatim text for these three issues - only NORC's short internal
#      variable labels.
#   4. Response formats vary by item, not hardcoded to Yes/No:
#        - abortion, suicide/euthanasia, police violence: Yes/No
#        - FEPOL (gender roles): Agree/Disagree
#        - FECHLD, FEPRESCH, FEFAM (gender roles): 4-point Strongly
#          agree/Agree/Disagree/Strongly disagree
#   5. Years are PER VARIABLE, pulled from the non-NA years in your
#      gss_yearly_summary.csv - i.e. every year that specific item was
#      actually fielded. Deliberately NOT the salience index's year range:
#      salience_final.csv has annual coverage back to 1972, but the GSS only
#      asks most of these items in specific survey years, and not every item
#      in every survey year. Using salience years would generate prompts for
#      years with no GSS marginal to validate against.
#   6. CONFIRMED (previously an open assumption): the binary and 4-point
#      recodes in 04_gss_clean.R preserve the GSS codebook's original option
#      ordering (option 1 = Yes/Agree/Strongly agree), matching the option
#      order used in build_prompt() below. Verified two ways - a diagnostic
#      pass confirmed every binary variable's non-NA values are exactly
#      "YES"/"NO" with nothing silently falling into NA, and a direct read
#      of 04_gss_clean.R's case_when() blocks confirmed the numeric coding
#      direction matches for both the binary items and the FECHLD/FEPRESCH/
#      FEFAM 4-point scale. Comparison values below can be trusted as-is.
#
# WHAT THIS COSTS AND HOW LONG IT TAKES
# 507 unique variable-year combinations x 11 models x 5 runs = 27,885 base
# calls (plus a re-prompt on any refusal). Per-model cost estimates from the
# funding spreadsheet were built on this exact call volume, summing to
# roughly £110.60 across all 11 models - load £140-150 of credit to leave
# buffer for retries and FX conversion (OpenRouter bills in USD). Claude
# Sonnet 4.6 alone accounts for over half that estimate (~£63.62), so it's
# the one to watch if actual usage runs above estimate.
# ==============================================================================

library(httr2)
library(jsonlite)
library(tidyverse)
library(readr)
library(glue)

# ── Config ────────────────────────────────────────────────────────────────────

OPENROUTER_KEY <- Sys.getenv("OPENROUTER_API_KEY")
BASE_URL       <- "https://openrouter.ai/api/v1/chat/completions"

if (identical(OPENROUTER_KEY, "")) {
  stop("OPENROUTER_API_KEY is not set. See the header comment for how to set it.")
}

RAW_OUTPUT_PATH   <- "data_raw/condition_a_pilot_responses.csv"
CLEAN_OUTPUT_PATH <- "data_processed/condition_a_pilot_responses_clean.csv"
COMPARISON_PATH   <- "data_processed/condition_a_pilot_comparison.csv"
GSS_SUMMARY_PATH  <- "data_processed/gss_yearly_summary.csv"

dir.create("data_raw",       showWarnings = FALSE)
dir.create("data_processed", showWarnings = FALSE)

N_RUNS <- 5
SUBSET_YEARS_FOR_SPEED <- FALSE  # set TRUE for a quick first pass (3 years/variable)

# ── Final 11-model list ─────────────────────────────────────────────────────
model_map <- c(
  "qwen3_235b"      = "qwen/qwen3-235b-a22b-2507",
  "gemma4_31b"      = "google/gemma-4-31b-it",
  "deepseek_v32"    = "deepseek/deepseek-v3.2",
  "claude_4_6"      = "anthropic/claude-sonnet-4.6",
  "gemini_3_flash"  = "google/gemini-3-flash-preview",
  "gpt5_mini"       = "openai/gpt-5-mini",
  "grok_4_3"        = "x-ai/grok-4.3",
  "mistral_small4"  = "mistralai/mistral-small-2603",
  "llama4_maverick" = "meta-llama/llama-4-maverick",
  "command_r"       = "cohere/command-r-08-2024",
  # Olmo dropped - repeated "no endpoints found" 404s on both the 3.0 and
  # 3.1 Think variants, on two separate days. Not treating this as a
  # transient outage - document as: originally planned, unavailable via
  # OpenRouter at time of data collection, excluded from final model list.
  "mistral_nemo"    = "mistralai/mistral-nemo",
  "llama31_8b"      = "meta-llama/llama-3.1-8b-instruct",
  "gpt_oss_120b"    = "openai/gpt-oss-120b",
  "llama33_70b"     = "meta-llama/llama-3.3-70b-instruct",
  "gemini25_lite"   = "google/gemini-2.5-flash-lite",
  "deepseek_v31"    = "deepseek/deepseek-chat-v3.1",
  "cogito_v21_671b" = "deepcogito/cogito-v2.1-671b",
  "glm_4_6"         = "z-ai/glm-4.6",
  "gpt5"            = "openai/gpt-5",
  "jamba_large"     = "ai21/jamba-large-1.7",
  "kimi_k26"        = "moonshotai/kimi-k2.6",
  "gpt4o_mini"      = "openai/gpt-4o-mini",
  "mistral_large3"  = "mistralai/mistral-large-2512",
  "qwen3_next_80b"  = "qwen/qwen3-next-80b-a3b-instruct",
  "deepseek_v3_0324"= "deepseek/deepseek-chat-v3-0324"
)

# ── Verbatim GSS wording, response format, and GSS comparison column ────────
# per item. `gss_col` / `comparison_type` are used at the very end to check
# simulated responses against real GSS marginals.
#
# response_type "binary"  -> options are exactly 2, compare as prop choosing option 1
# response_type "scale4"  -> options are 4, compare as mean of 1-4

verbatim_questions <- tribble(
  ~issue,             ~variable,   ~question_text,                                                                                                                                                                                    ~response_type, ~option_labels_str,                                     ~gss_col,
  "abortion",         "abany",     "Please tell me whether or not you think it should be possible for a pregnant woman to obtain a legal abortion if the woman wants it for any reason?",                                            "binary",       "Yes,No",                                               "prop_abany",
  "abortion",         "abdefect",  "Please tell me whether or not you think it should be possible for a pregnant woman to obtain a legal abortion if there is a strong chance of serious defect in the baby?",                        "binary",       "Yes,No",                                               "prop_abdefect",
  "abortion",         "abhlth",    "Please tell me whether or not you think it should be possible for a pregnant woman to obtain a legal abortion if the woman's own health is seriously endangered by the pregnancy?",               "binary",       "Yes,No",                                               "prop_abhlth",
  "abortion",         "abnomore",  "Please tell me whether or not you think it should be possible for a pregnant woman to obtain a legal abortion if she is married and does not want any more children?",                            "binary",       "Yes,No",                                               "prop_abnomore",
  "abortion",         "abpoor",    "Please tell me whether or not you think it should be possible for a pregnant woman to obtain a legal abortion if the family has a very low income and cannot afford any more children?",           "binary",       "Yes,No",                                               "prop_abpoor",
  "abortion",         "abrape",    "Please tell me whether or not you think it should be possible for a pregnant woman to obtain a legal abortion if she became pregnant as a result of rape?",                                        "binary",       "Yes,No",                                               "prop_abrape",
  "abortion",         "absingle",  "Please tell me whether or not you think it should be possible for a pregnant woman to obtain a legal abortion if she is not married and does not want to marry the man?",                          "binary",       "Yes,No",                                               "prop_absingle",
  "suicide",          "letdie1",   "When a person has a disease that cannot be cured, do you think doctors should be allowed by law to end the patient's life by some painless means if the patient and his family request it?",       "binary",       "Yes,No",                                               "prop_letdie1",
  "suicide",          "suicide1",  "Do you think a person has the right to end his or her own life if this person has an incurable disease?",                                                                                          "binary",       "Yes,No",                                               "prop_suicide1",
  "police_violence",  "polhitok",  "Are there any situations you can imagine in which you would approve of a policeman striking an adult male citizen?",                                                                               "binary",       "Yes,No",                                               "prop_polhitok",
  "police_violence",  "polmurdr",  "Would you approve of a policeman striking an adult male citizen who was being questioned as a suspect in a murder case?",                                                                                      "binary",       "Yes,No",                                               "prop_polmurdr",
  "police_violence",  "polescap",  "Would you approve of a policeman striking an adult male citizen who was attempting to escape from custody?",                                                                                                   "binary",       "Yes,No",                                               "prop_polescap",
  "police_violence",  "polattak",  "Would you approve of a policeman striking an adult male citizen who was attacking the policeman with his fists?",                                                                                              "binary",       "Yes,No",                                               "prop_polattak",
  "gender_roles",     "fepol",     "Tell me if you agree or disagree with this statement: Most men are better suited emotionally for politics than are most women.",                                                                   "binary",       "Agree,Disagree",                                       "prop_fepol_num",
  "gender_roles",     "fechld",    "Please indicate whether you strongly agree, agree, disagree, or strongly disagree with the following statement: A working mother can establish just as warm and secure a relationship with her children as a mother who does not work.", "scale4", "Strongly agree,Agree,Disagree,Strongly disagree", "mean_fechld_num",
  "gender_roles",     "fepresch",  "Please indicate whether you strongly agree, agree, disagree, or strongly disagree with the following statement: A preschool child is likely to suffer if his or her mother works.",                "scale4",       "Strongly agree,Agree,Disagree,Strongly disagree",       "mean_fepresch_num",
  "gender_roles",     "fefam",     "Please indicate whether you strongly agree, agree, disagree, or strongly disagree with the following statement: It is much better for everyone involved if the man is the achiever outside the home and the woman takes care of the home and family.", "scale4", "Strongly agree,Agree,Disagree,Strongly disagree", "mean_fefam_num"
) |>
  mutate(option_labels = str_split(option_labels_str, ","),
         n_options     = lengths(option_labels)) |>
  select(-option_labels_str)

# ── Per-variable year coverage ───────────────────────────────────────────────
# Pulled from the non-NA years in your gss_yearly_summary.csv - i.e. exactly
# the years each item was fielded, NOT the salience index's annual range.

variable_years <- list(
  abany    = c(1977,1978,1980,1982,1983,1984,1985,1987,1988,1989,1990,1991,1993,1994,1996,1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2021,2022,2024),
  abdefect = c(1972,1973,1974,1975,1976,1977,1978,1980,1982,1983,1984,1985,1987,1988,1989,1990,1991,1993,1994,1996,1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2021,2022,2024),
  abhlth   = c(1972,1973,1974,1975,1976,1977,1978,1980,1982,1983,1984,1985,1987,1988,1989,1990,1991,1993,1994,1996,1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2021,2022,2024),
  abnomore = c(1972,1973,1974,1975,1976,1977,1978,1980,1982,1983,1984,1985,1987,1988,1989,1990,1991,1993,1994,1996,1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2021,2022,2024),
  abpoor   = c(1972,1973,1974,1975,1976,1977,1978,1980,1982,1983,1984,1985,1987,1988,1989,1990,1991,1993,1994,1996,1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2021,2022,2024),
  abrape   = c(1972,1973,1974,1975,1976,1977,1978,1980,1982,1983,1984,1985,1987,1988,1989,1990,1991,1993,1994,1996,1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2021,2022,2024),
  absingle = c(1972,1973,1974,1975,1976,1977,1978,1980,1982,1983,1984,1985,1987,1988,1989,1990,1991,1993,1994,1996,1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2021,2022,2024),
  letdie1  = c(1977,1978,1982,1983,1985,1986,1988,1989,1990,1991,1993,1994,1996,1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2021,2022,2024),
  suicide1 = c(1977,1978,1982,1983,1985,1986,1988,1989,1990,1991,1993,1994,1996,1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2021,2022,2024),
  polhitok = c(1973,1975,1976,1978,1980,1983,1984,1986,1987,1988,1989,1990,1991,1993,1994,1996,1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2021,2022,2024),
  polmurdr = c(1973,1975,1976,1978,1980,1983,1984,1986,1987,1988,1989,1990,1991,1993,1994,1996,1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2021,2022,2024),
  polescap = c(1973,1975,1976,1978,1980,1983,1984,1986,1987,1988,1989,1990,1991,1993,1994,1996,1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2021,2022,2024),
  polattak = c(1973,1975,1976,1978,1980,1983,1984,1986,1987,1988,1989,1990,1991,1993,1994,1996,1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2021,2022,2024),
  fepol    = c(1974,1975,1977,1978,1982,1983,1985,1986,1988,1989,1990,1991,1993,1994,1996,1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2022,2024),
  fechld   = c(1977,1985,1986,1988,1989,1990,1991,1993,1994,1996,1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2021,2022,2024),
  fepresch = c(1977,1985,1986,1988,1989,1990,1991,1993,1994,1996,1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2021,2022,2024),
  fefam    = c(1977,1985,1986,1988,1989,1990,1991,1993,1994,1996,1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2021,2022,2024)
)

pick_years <- function(variable) {
  yrs <- variable_years[[variable]]
  if (SUBSET_YEARS_FOR_SPEED) {
    idx <- unique(round(c(1, length(yrs) / 2, length(yrs))))
    yrs <- yrs[idx]
  }
  yrs
}

# ── Condition A prompt template ──────────────────────────────────────────────
# Verbatim question + the "typical American adult" persona. Options block and
# instruction line are now built dynamically from option_labels, so this works
# for 2-option and 4-option items alike.

build_prompt <- function(year, question_text, option_labels) {
  options_block <- paste0(seq_along(option_labels), ". ", option_labels, collapse = "\n")
  n <- length(option_labels)
  glue(
    "It is now {year}. You are answering survey questions as a typical American ",
    "adult living in the United States.\n\n",
    "Question: {question_text}\n\n",
    "Options:\n{options_block}\n\n",
    "Respond with ONLY the number of your answer (a number from 1 to {n}). ",
    "Do not explain your reasoning."
  )
}

REPROMPT_TEXT <- paste(
  "You have answered this question before. You do not need to provide an",
  "opinion or take a stance, just report statistical belief at the time.",
  "Please respond with ONLY the number of your answer."
)

# ── Build the full prompt grid: model x variable x (that variable's years) x run ─
# Note this is NOT a single expand_grid over one shared YEARS vector, because
# years differ by variable. Built variable-by-variable and stacked with map_dfr.

prompts <- map_dfr(seq_len(nrow(verbatim_questions)), function(i) {
  q <- verbatim_questions[i, ]
  yrs <- pick_years(q$variable)

  expand_grid(
    model = names(model_map),
    year  = yrs,
    run   = seq_len(N_RUNS)
  ) |>
    mutate(
      issue          = q$issue,
      variable       = q$variable,
      question_text  = q$question_text,
      response_type  = q$response_type,
      n_options      = q$n_options,
      option_labels  = list(q$option_labels[[1]]),
      gss_col        = q$gss_col,
      prompt         = map_chr(year, ~ build_prompt(.x, q$question_text, q$option_labels[[1]]))
    )
})

cat(glue(
  "Prepared {nrow(prompts)} prompts across {length(model_map)} models, ",
  "{n_distinct(prompts$variable)} variables, {n_distinct(prompts$issue)} issues.\n\n"
))

# ── Response classification ──────────────────────────────────────────────────
# Valid = a clean integer from 1 to n_options for that specific row (2 for
# binary items, 4 for the scale items) - NOT hardcoded to 1-2 anymore.

extract_clean_response <- function(text, n_options) {
  if (is.na(text) || length(text) == 0) return(NA_integer_)
  trimmed <- trimws(text)

  # Preferred: response starts with the number, as instructed
  match <- str_extract(trimmed, "^[0-9]+")

  # Fallback: some reasoning models (e.g. Grok) add commentary before or
  # after the numeral despite being told not to - search anywhere in the
  # text for a standalone digit as a recovery path instead of treating
  # this as a refusal.
  if (is.na(match)) {
    match <- str_extract(trimmed, "\\b[0-9]\\b")
  }

  if (is.na(match)) return(NA_integer_)
  val <- as.integer(match)
  if (val >= 1 && val <= n_options) val else NA_integer_
}

is_refusal_like <- function(text, n_options) {
  is.na(extract_clean_response(text, n_options))
}

# ── API call helper ───────────────────────────────────────────────────────────

call_model <- function(messages, model_id, api_key) {
  resp <- request(BASE_URL) |>
    req_headers(
      "Authorization" = paste("Bearer", api_key),
      "Content-Type"  = "application/json"
    ) |>
    req_body_json(list(
      model       = model_id,
      max_tokens  = 300,
      temperature = 1.0,
      reasoning   = list(effort = "minimal"),
      messages    = messages
    )) |>
    req_retry(max_tries = 3, backoff = ~ 5) |>
    req_perform()

  content <- resp |> resp_body_json()
  msg <- content$choices[[1]]$message

  text <- msg$content
  if (is.null(text) || length(text) == 0 || identical(text, "")) {
    text <- if (!is.null(msg$refusal) && length(msg$refusal) > 0) {
      msg$refusal
    } else {
      NA_character_
    }
  }

  trimws(text)
}

# ── Load existing progress if resuming ───────────────────────────────────────

if (file.exists(RAW_OUTPUT_PATH)) {
  results <- read_csv(
    RAW_OUTPUT_PATH,
    show_col_types = FALSE,
    col_types = cols(
      response_raw_1 = col_character(),
      response_raw_2 = col_character(),
      error_message  = col_character(),
      .default       = col_guess()
    )
  )

  # Reconcile against the freshly-built prompt grid: if a model was swapped
  # (e.g. olmo3_32b -> olmo31_32b) after some progress was already saved,
  # the saved file won't yet contain rows for the new model. Add any
  # missing (model, variable, year, run) combos without touching existing
  # progress on anything else.
  key_cols <- c("model", "variable", "year", "run")
  new_prompts <- prompts |> select(-option_labels)

  missing_rows <- anti_join(new_prompts, results, by = key_cols) |>
    mutate(
      response_raw_1     = NA_character_,
      reprompted         = FALSE,
      response_raw_2     = NA_character_,
      final_response     = NA_integer_,
      response_category  = NA_character_,
      error_message      = NA_character_
    )

  if (nrow(missing_rows) > 0) {
    cat(glue("Found {nrow(missing_rows)} rows not yet in saved results (e.g. a swapped model) - adding them.\n\n"))
    results <- bind_rows(results, missing_rows)
  }

  cat("Resuming -", sum(!is.na(results$final_response)), "of", nrow(results),
      "already collected\n\n")
} else {
  results <- prompts |>
    select(-option_labels) |>   # list-column doesn't round-trip through CSV
    mutate(
      response_raw_1     = NA_character_,
      reprompted         = FALSE,
      response_raw_2     = NA_character_,
      final_response     = NA_integer_,
      response_category  = NA_character_,
      error_message      = NA_character_
    )
}

total <- nrow(results)

# ── Main collection loop ─────────────────────────────────────────────────────

for (i in seq_len(total)) {

  if (!is.na(results$response_category[i]) &&
      results$response_category[i] != "error") next

  row      <- results[i, ]
  model_id <- model_map[[row$model]]

  cat(sprintf("[%d/%d] %s | %s | %d | %s | run %d ... ",
              i, total, row$model, row$issue, row$year, row$variable, row$run))

  outcome <- tryCatch({

    first_messages <- list(list(role = "user", content = row$prompt))
    response_1 <- call_model(first_messages, model_id, OPENROUTER_KEY)

    if (!is_refusal_like(response_1, row$n_options)) {
      list(
        response_raw_1 = response_1, reprompted = FALSE,
        response_raw_2 = NA_character_,
        final_response = extract_clean_response(response_1, row$n_options),
        response_category = "clean", error_message = NA_character_
      )
    } else {
      Sys.sleep(1)
      second_messages <- list(
        list(role = "user",      content = row$prompt),
        list(role = "assistant", content = response_1),
        list(role = "user",      content = REPROMPT_TEXT)
      )
      response_2 <- call_model(second_messages, model_id, OPENROUTER_KEY)

      if (!is_refusal_like(response_2, row$n_options)) {
        list(
          response_raw_1 = response_1, reprompted = TRUE,
          response_raw_2 = response_2,
          final_response = extract_clean_response(response_2, row$n_options),
          response_category = "clean_after_reprompt", error_message = NA_character_
        )
      } else {
        list(
          response_raw_1 = response_1, reprompted = TRUE,
          response_raw_2 = response_2,
          final_response = NA_integer_,
          response_category = "refusal", error_message = NA_character_
        )
      }
    }
  }, error = function(e) {
    list(
      response_raw_1 = NA_character_, reprompted = FALSE,
      response_raw_2 = NA_character_, final_response = NA_integer_,
      response_category = "error", error_message = conditionMessage(e)
    )
  })

  cat(outcome$response_category, "\n")

  results$response_raw_1[i]    <- outcome$response_raw_1
  results$reprompted[i]        <- outcome$reprompted
  results$response_raw_2[i]    <- outcome$response_raw_2
  results$final_response[i]    <- outcome$final_response
  results$response_category[i] <- outcome$response_category
  results$error_message[i]     <- outcome$error_message

  Sys.sleep(1)
  write_csv(results, RAW_OUTPUT_PATH)
}

cat("\n✓ Collection complete\n\n")

# ── Refusal rate report ──────────────────────────────────────────────────────

cat("── Response category counts, by model ──\n")
results |>
  count(model, response_category) |>
  pivot_wider(names_from = response_category, values_from = n, values_fill = 0) |>
  print()

cat("\n── Refusal rate by model x issue ──\n")
results |>
  group_by(model, issue) |>
  summarise(
    n            = n(),
    refusal_rate = mean(response_category == "refusal", na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(refusal_rate)) |>
  print(n = 30)

cat("\n── Refusal rate by model x variable (top 15) ──\n")
results |>
  group_by(model, variable) |>
  summarise(
    n            = n(),
    refusal_rate = mean(response_category == "refusal", na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(refusal_rate)) |>
  print(n = 15)

write_csv(results, CLEAN_OUTPUT_PATH)
cat(glue("\n✓ Full results (raw + classified) saved to {CLEAN_OUTPUT_PATH}\n"))

# ── Compare against GSS marginals ────────────────────────────────────────────
# Binary items -> compare proportion choosing option 1 against gss prop_ column
# Scale4 items (fechld/fepresch/fefam) -> compare mean(1-4) against gss mean_ column

if (file.exists(GSS_SUMMARY_PATH)) {

  gss_long <- read_csv(GSS_SUMMARY_PATH, show_col_types = FALSE) |>
    pivot_longer(-year, names_to = "gss_col", values_to = "gss_value") |>
    mutate(year = as.numeric(year))

  valid_results <- results |> filter(!is.na(final_response))

  simulated <- valid_results |>
    group_by(model, issue, variable, year, response_type, gss_col) |>
    summarise(
      simulated_value = case_when(
        first(response_type) == "binary" ~ mean(final_response == 1, na.rm = TRUE),
        first(response_type) == "scale4" ~ mean(final_response, na.rm = TRUE),
        TRUE ~ NA_real_
      ),
      n_valid = sum(!is.na(final_response)),
      .groups = "drop"
    )

  comparison <- simulated |>
    left_join(gss_long, by = c("year", "gss_col")) |>
    mutate(accuracy_gap = abs(simulated_value - gss_value))

  write_csv(comparison, COMPARISON_PATH)
  cat(glue("✓ Comparison table saved to {COMPARISON_PATH}\n\n"))

  cat("── Mean accuracy gap by model x issue x response type ──\n")
  cat("(binary items: gap on a 0-1 scale. scale4 items: gap on a 0-3 scale - not directly comparable to binary gaps)\n\n")
  comparison |>
    group_by(model, issue, response_type) |>
    summarise(mean_gap = mean(accuracy_gap, na.rm = TRUE), .groups = "drop") |>
    arrange(issue, response_type, model) |>
    print(n = 30)

} else {
  cat(glue(
    "\nNote: {GSS_SUMMARY_PATH} not found, skipping GSS comparison.\n",
    "Copy your uploaded gss_yearly_summary.csv into data_processed/ to enable it.\n"
  ))
}
