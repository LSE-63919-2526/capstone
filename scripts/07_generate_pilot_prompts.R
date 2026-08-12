library(tidyverse)
library(readr)

# ── GSS question definitions ──────────────────────────────────────────────────
# Question text reproduced verbatim from GSS
# Response options numbered to match GSS coding

gss_questions <- tribble(
  ~variable,   ~question_text,                                                                 ~options,
  "abdefect",  "Please tell me whether or not you think it should be possible for a pregnant woman to obtain a legal abortion if there is a strong chance of serious defect in the baby?",
               "1. Yes\n2. No",
  "abnomore",  "Please tell me whether or not you think it should be possible for a pregnant woman to obtain a legal abortion if she is married and does not want any more children?",
               "1. Yes\n2. No",
  "abhlth",    "Please tell me whether or not you think it should be possible for a pregnant woman to obtain a legal abortion if the woman's own health is seriously endangered by the pregnancy?",
               "1. Yes\n2. No",
  "abpoor",    "Please tell me whether or not you think it should be possible for a pregnant woman to obtain a legal abortion if the family has a very low income and cannot afford any more children?",
               "1. Yes\n2. No",
  "abrape",    "Please tell me whether or not you think it should be possible for a pregnant woman to obtain a legal abortion if she became pregnant as a result of rape?",
               "1. Yes\n2. No",
  "absingle",  "Please tell me whether or not you think it should be possible for a pregnant woman to obtain a legal abortion if she is not married and does not want to marry the man?",
               "1. Yes\n2. No",
  "abany",     "Please tell me whether or not you think it should be possible for a pregnant woman to obtain a legal abortion if the woman wants it for any reason?",
               "1. Yes\n2. No"
)

# ── Pilot years ───────────────────────────────────────────────────────────────

pilot_years <- c(1977, 1982, 1987, 1992, 1997, 2002, 2007, 2012, 2017, 2022)

# ── Models ────────────────────────────────────────────────────────────────────

models <- c("claude", "chatgpt", "gemini")

# ── Prompt template ───────────────────────────────────────────────────────────

build_prompt <- function(year, question_text, options) {
  glue::glue(
    "It is now {year}. You are answering survey questions as a typical American adult living in the United States.\n\n",
    "Question: {question_text}\n\n",
    "Options:\n{options}\n\n",
    "Respond with ONLY the number of your answer (e.g., \"1\" or \"2\"). Do not explain your reasoning."
  )
}

# ── Generate all prompt combinations ─────────────────────────────────────────

prompts <- crossing(
  model    = models,
  year     = pilot_years,
  variable = gss_questions$variable,
  run      = 1:5   # 5 runs per condition for pilot
) |>
  left_join(gss_questions, by = "variable") |>
  mutate(
    prompt = pmap_chr(
      list(year, question_text, options),
      build_prompt
    ),
    response    = NA_character_,  # to be filled in
    response_clean = NA_integer_  # 1 or 2 after collection
  )

cat("Total prompts generated:", nrow(prompts), "\n")
cat("Breakdown:\n")
cat(" Models:", length(models), "\n")
cat(" Years:", length(pilot_years), "\n")
cat(" Questions:", nrow(gss_questions), "\n")
cat(" Runs per condition: 5\n")

# ── Save prompt file ──────────────────────────────────────────────────────────

write_csv(prompts, "data_raw/pilot_prompts.csv")
cat("\n✓ Prompt file saved to data_raw/pilot_prompts.csv\n")

# ── Export human-readable prompt sheet for manual entry ──────────────────────
# This creates a text file you can work from if entering responses manually

manual_sheet <- prompts |>
  filter(model == "claude") |>  # one model at a time for manual work
  mutate(
    formatted = paste0(
      "=== ", model, " | ", year, " | ", variable, " | Run ", run, " ===\n",
      prompt, "\n\n",
      "YOUR RESPONSE: ___\n",
      "----------------------------------------\n"
    )
  ) |>
  pull(formatted) |>
  paste(collapse = "\n")

writeLines(manual_sheet, "data_raw/manual_prompt_sheet_claude.txt")
cat("✓ Manual prompt sheet saved to data_raw/manual_prompt_sheet_claude.txt\n")
cat("  (Change filter to 'chatgpt' or 'gemini' to generate sheets for other models)\n")