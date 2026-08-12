library(tidyverse)
library(readxl)
library(readr)

# ── Load raw GSS extract ──────────────────────────────────────────────────────

gss_raw <- read_excel("data_raw/GSS.xlsx")  
# adjust path if GSS.xlsx is elsewhere

cat("Raw dimensions:", nrow(gss_raw), "x", ncol(gss_raw), "\n")

# ── Define missing value strings ──────────────────────────────────────────────
# These are all the non-response codes GSS uses
# Any response starting with "." is treated as missing

missing_patterns <- c(
  ".i:  Inapplicable",
  ".n:  No answer", 
  ".d:  Do not Know/Cannot Choose",
  ".s:  Skipped on Web",
  ".r:  Refused",
  ".x:  Not available in this release",
  ".y:  Not available in this year",
  ".u:  Uncodable",
  ".m:  DK, NA, IAP",
  ".z:  Variable-specific reserve code",
  ".j:  I do not have a job",
  ".p:  Not applicable (I have not faced this decision)/Not imputable"
)

# Helper: recode a column, replacing missing strings with NA
clean_col <- function(x) {
  if_else(str_starts(as.character(x), "\\."), NA_character_, as.character(x))
}

# ── Clean all columns ─────────────────────────────────────────────────────────

gss_clean <- gss_raw |>
  mutate(across(everything(), clean_col)) |>
  mutate(
    year  = as.integer(year),
    id_   = as.integer(id_),
    age_num = case_when(
      age == "89 or older"                ~ 89L,
      str_starts(as.character(age), "\\.") ~ NA_integer_,
      TRUE                                 ~ as.integer(age)
    ),
    educ_num = case_when(
      educ == "No formal schooling"        ~ 0L,
      educ == "1st grade"                  ~ 1L,
      educ == "2nd grade"                  ~ 2L,
      educ == "3rd grade"                  ~ 3L,
      educ == "4th grade"                  ~ 4L,
      educ == "5th grade"                  ~ 5L,
      educ == "6th grade"                  ~ 6L,
      educ == "7th grade"                  ~ 7L,
      educ == "8th grade"                  ~ 8L,
      educ == "9th grade"                  ~ 9L,
      educ == "10th grade"                 ~ 10L,
      educ == "11th grade"                 ~ 11L,
      educ == "12th grade"                 ~ 12L,
      educ == "1 year of college"          ~ 13L,
      educ == "2 years of college"         ~ 14L,
      educ == "3 years of college"         ~ 15L,
      educ == "4 years of college"         ~ 16L,
      educ == "5 years of college"         ~ 17L,
      educ == "6 years of college"         ~ 18L,
      educ == "7 years of college"         ~ 19L,
      educ == "8 or more years of college" ~ 20L,
      TRUE                                 ~ NA_integer_
    )
  )

# ── Diagnostic: check binary vars are only ever YES/NO before recoding ───────
 
diagnostic_vars <- c("abdefect", "abnomore", "abhlth", "abpoor",
                      "abrape", "absingle", "abany",
                      "letdie1", "suicide1",
                      "polhitok", "polmurdr", "polescap", "polattak")
 
unexpected_values <- gss_clean |>
  select(all_of(diagnostic_vars)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value") |>
  filter(!is.na(value), !value %in% c("YES", "NO")) |>
  count(variable, value, sort = TRUE)
 
if (nrow(unexpected_values) == 0) {
  cat("✓ No unexpected values - every non-NA response is YES or NO.\n")
} else {
  cat("⚠ Found values outside YES/NO - these are silently becoming NA in the recode:\n")
  print(unexpected_values, n = 30)
}

# ── Recode outcome variables to numeric proportions ───────────────────────────
# For binary YES/NO variables: recode to 1 (yes) / 0 (no)
# For ordered variables: keep as ordered factor or numeric scale
# This makes computing yearly proportions straightforward

# Binary YES/NO outcomes
binary_vars <- c("abdefect", "abnomore", "abhlth", "abpoor", 
                 "abrape", "absingle", "abany",
                 "letdie1", "suicide1",
                 "polhitok", "polmurdr", "polescap", "polattak")

gss_clean <- gss_clean |>
  mutate(across(all_of(binary_vars), ~ case_when(
    . == "YES" ~ 1L,
    . == "NO"  ~ 0L,
    TRUE       ~ NA_integer_
  )))

# homosex: 1=Always wrong, 2=Almost always wrong, 3=Sometimes wrong, 
#          4=Not wrong at all, 5=Other
# Recode to numeric 1-4, drop "OTHER" as uninterpretable
gss_clean <- gss_clean |>
  mutate(homosex_num = case_when(
    homosex == "ALWAYS WRONG"      ~ 1L,
    homosex == "ALMST ALWAYS WRG"  ~ 2L,
    homosex == "SOMETIMES WRONG"   ~ 3L,
    homosex == "NOT WRONG AT ALL"  ~ 4L,
    TRUE                           ~ NA_integer_
  ))

# fefam, fechld, fepresch, fepol: 
# 1=Strongly agree, 2=Agree, 3=Disagree, 4=Strongly disagree
# Note: for fefam and fepresch, agreement = traditional view
# Recode to numeric 1-4
likert_vars <- c("fefam", "fechld", "fepresch")

gss_clean <- gss_clean |>
  mutate(across(all_of(likert_vars), ~ case_when(
    . == "STRONGLY AGREE"    ~ 1L,
    . == "AGREE"             ~ 2L,
    . == "DISAGREE"          ~ 3L,
    . == "STRONGLY DISAGREE" ~ 4L,
    TRUE                     ~ NA_integer_
  ), .names = "{.col}_num"))

# fepol: agree/disagree only (men better suited for politics)
gss_clean <- gss_clean |>
  mutate(fepol_num = case_when(
    fepol == "AGREE"    ~ 1L,
    fepol == "DISAGREE" ~ 0L,
    TRUE                ~ NA_integer_
  ))

# grass: should marijuana be legal
gss_clean <- gss_clean |>
  mutate(grass_num = case_when(
    grass == "LEGAL"       ~ 1L,
    grass == "NOT LEGAL"   ~ 0L,
    TRUE                   ~ NA_integer_
  ))

# cappun: favor/oppose death penalty
gss_clean <- gss_clean |>
  mutate(cappun_num = case_when(
    cappun == "FAVOR"  ~ 1L,
    cappun == "OPPOSE" ~ 0L,
    TRUE               ~ NA_integer_
  ))

# ── Compute yearly proportions for each outcome variable ──────────────────────
# Will compare against LLM outputs
# For binary vars: proportion answering YES (1)
# For ordered vars: mean score on numeric scale

outcome_binary <- c("abdefect", "abnomore", "abhlth", "abpoor",
                    "abrape", "absingle", "abany",
                    "letdie1", "suicide1",
                    "polhitok", "polmurdr", "polescap", "polattak",
                    "fepol_num", "grass_num", "cappun_num")

outcome_scale  <- c("homosex_num", "fefam_num", "fechld_num", 
                    "fepresch_num")

# Binary: proportion = mean of 0/1 column
yearly_binary <- gss_clean |>
  select(year, all_of(outcome_binary)) |>
  group_by(year) |>
  summarise(across(everything(), 
                   ~ mean(., na.rm = TRUE),
                   .names = "prop_{.col}"),
            .groups = "drop")

# Scale: mean score
yearly_scale <- gss_clean |>
  select(year, all_of(outcome_scale)) |>
  group_by(year) |>
  summarise(across(everything(),
                   ~ mean(., na.rm = TRUE),
                   .names = "mean_{.col}"),
            .groups = "drop")

# Join into one yearly summary table
yearly_summary <- yearly_binary |>
  left_join(yearly_scale, by = "year")

# ── Save outputs ──────────────────────────────────────────────────────────────

write_csv(gss_clean,       "data_processed/gss_clean.csv")
write_csv(yearly_summary,  "data_processed/gss_yearly_summary.csv")

cat("\n✓ Clean individual-level data saved to data_processed/gss_clean.csv\n")
cat("✓ Yearly summary saved to data_processed/gss_yearly_summary.csv\n")
cat("\nRows in clean file:", nrow(gss_clean), "\n")
cat("Waves in summary:", nrow(yearly_summary), "\n")

# ── Quick sanity check ────────────────────────────────────────────────────────
# Print first few rows of yearly summary for key variables

cat("\n── Sanity check: abany proportion by year ──\n")
yearly_summary |>
  select(year, prop_abany) |>
  filter(!is.na(prop_abany)) |>
  print(n = 35)