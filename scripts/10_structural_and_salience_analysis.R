# ==============================================================================
# 10_structural_and_salience_analysis.R
#
# Purpose:
#   (A) Response determinism diagnostics (point-estimator vs. distributional
#       behavior), including a temporal-invariance check across the full
#       year span (not just across repeated runs within a year).
#   (B) Mixed-effects regression testing whether salience moderates temporal
#       decay: accuracy_gap ~ distance_from_present * salience, with explicit
#       handling of four confounds (item heterogeneity, the suicide wording
#       change, the 2021 GSS mode-of-administration switch, and training-data
#       contamination risk). Each confound is called out at the point in the
#       code where it's addressed - three are handled directly in the model,
#       one (contamination) is only partially mitigated via a sensitivity
#       check, not fully resolved.
#   (C) Salience-controlled bivariate analysis (raw / partial / LOESS-
#       detrended correlation) - kept as a simpler cross-check against the
#       regression in (B).
#
# Inputs:
#   data_processed/condition_a_pilot_comparison.csv
#   data_processed/condition_a_pilot_responses_clean.csv
#   data_processed/salience_final.csv
#
# Output:
#   data_processed/diagnostics_determinism.csv
#   data_processed/diagnostics_temporal_invariance.csv
#   data_processed/regression_primary_summary.csv
#   data_processed/regression_contamination_sensitivity.csv
#   data_processed/diagnostics_salience.csv
#
# No plots are produced here.
# ==============================================================================

library(tidyverse)
library(lme4)
library(lmerTest)   
library(broom.mixed)

# ------------------------------------------------------------------------
# 0. Load & clean
# ------------------------------------------------------------------------

comp <- read_csv(
  "data_processed/condition_a_pilot_comparison.csv",
  show_col_types = FALSE
) |>
  filter(!str_detect(model, "(?i)olmo"))

resp <- read_csv(
  "data_processed/condition_a_pilot_responses_clean.csv",
  show_col_types = FALSE,
  col_types = cols(
    response_raw_1 = col_character(),
    response_raw_2 = col_character(),
    error_message  = col_character(),
    .default       = col_guess()
  )
) |>
  filter(!str_detect(model, "(?i)olmo"))

salience <- read_csv("data_processed/salience_final.csv", show_col_types = FALSE) |>
  mutate(
    issue = recode(issue, "suicide_permissibility" = "suicide"),
    year  = as.numeric(year)   # force numeric to match comp$year at the join below
  ) |>
  group_by(issue, year) |>
  summarise(
    nyt_normalized = sum(nyt_normalized, na.rm = TRUE),
    gtrends_normalized = mean(gtrends_normalized, na.rm = TRUE),
    .groups = "drop"
  )

# --- GSS sample size + mode-of-administration, from raw GSS.xlsx ---------
# Addresses two of the "GSS reliability" gaps flagged earlier: effective
# sample size varies a lot across item-years due to ballot rotation (every
# item in this battery rotates across ballots a/b/c/d, not just some of
# them), and the mode-switch flag was previously too simple (see below).
gss_raw <- readxl::read_excel("data_raw/GSS.xlsx", sheet = "Data")

target_vars <- c("abany","abdefect","abhlth","abnomore","abpoor","abrape","absingle",
                  "fechld","fefam","fepol","fepresch",
                  "polattak","polescap","polhitok","polmurdr",
                  "letdie1","suicide1")

is_valid_response <- function(x) !is.na(x) & !str_starts(as.character(x), "\\.")

gss_n_valid <- map_dfr(target_vars, function(v) {
  gss_raw |>
    group_by(year) |>
    summarise(
      variable = v,
      # renamed to gss_sample_n - "n_valid" is already taken in comp/reg_df,
      # where it means something different (count of valid LLM runs out of 5,
      # not GSS respondent sample size). Same underlying dplyr silent-rename
      # issue as the earlier column-naming problems - keeping distinct names
      # avoids it happening again downstream.
      gss_sample_n = sum(is_valid_response(.data[[v]])),
      total_n  = n(),
      .groups = "drop"
    )
}) |>
  mutate(
    year           = as.numeric(year),
    valid_fraction = gss_sample_n / total_n
  )

# GSS mode-of-administration regime, from NORC's own documentation:
#   1972-2018: face-to-face (baseline)
#   2021:      100% web self-administered (most divergent single break)
#   2022,2024: multi-mode (face-to-face + web + phone) -- NOT a return to
#              the pre-2021 baseline, a third distinct protocol
# face_to_face is set as the reference level (first factor level).
gss_mode_lookup <- tibble(year = as.numeric(1972:2026)) |>
  mutate(gss_mode_regime = case_when(
    year == 2021 ~ "web_2021",
    year %in% c(2022, 2024) ~ "multimode_bridge",
    TRUE ~ "face_to_face"
  )) |>
  mutate(gss_mode_regime = factor(gss_mode_regime,
                                    levels = c("face_to_face", "web_2021", "multimode_bridge")))

# Scale normalization: scale4 items (fechld, fefam, fepresch) run 1-4;
# binary items run 0-1. Divide scale4 gaps by 3 so everything is comparable
# on a 0-1 footing before any cross-issue comparison.
comp <- comp |>
  mutate(
    accuracy_gap_norm = if_else(response_type == "scale4", accuracy_gap / 3, accuracy_gap),
    sim_val_norm      = if_else(response_type == "scale4", (simulated_value - 1) / 3, simulated_value),
    gss_val_norm      = if_else(response_type == "scale4", (gss_value - 1) / 3, gss_value)
  )

# ==============================================================================
# (A) RESPONSE DETERMINISM DIAGNOSTICS
# ==============================================================================

# --- A1. Within-year determinism: Shannon entropy of the 5 repeated runs ----
# H = 0  -> fully deterministic (all runs picked the same category)
# H = 1  -> maximally spread across available categories (normalized by log(K))
clean_resp <- resp |>
  filter(response_category %in% c("clean", "clean_after_reprompt")) |>
  mutate(final_response = as.numeric(final_response))

normalized_entropy <- function(x, n_options) {
  tab <- table(x)
  p <- tab / sum(tab)
  h <- -sum(p * log(p))
  h_max <- log(n_options)
  if (h_max == 0) return(0)
  h / h_max
}

entropy_by_cell <- clean_resp |>
  filter(!is.na(final_response)) |>
  group_by(model, issue, variable, year, n_options) |>
  summarise(n_runs = n(), entropy = normalized_entropy(final_response, first(n_options)), .groups = "drop")

diagnostics_determinism <- entropy_by_cell |>
  group_by(model) |>
  summarise(
    mean_entropy = mean(entropy, na.rm = TRUE),
    pct_zero_entropy = mean(entropy == 0, na.rm = TRUE) * 100,
    n_cells = n(),
    .groups = "drop"
  ) |>
  arrange(mean_entropy)

cat("\n=== A1. RESPONSE DETERMINISM (within-year, across 5 runs) ===\n")
print(diagnostics_determinism, n = Inf)

# --- A2. Temporal invariance: does the model give the SAME answer across  ---
# --- the entire year span, not just within a single year? -------------------
diagnostics_temporal_invariance <- comp |>
  group_by(model, issue, variable) |>
  summarise(sd_across_years = sd(simulated_value, na.rm = TRUE), .groups = "drop") |>
  mutate(is_temporally_constant = sd_across_years < 1e-9)

cat("\n=== A2. % OF ITEMS THAT NEVER CHANGE ACROSS THE FULL YEAR SPAN, BY MODEL ===\n")
diagnostics_temporal_invariance |>
  group_by(model) |>
  summarise(pct_constant = mean(is_temporally_constant) * 100, n_items = n()) |>
  arrange(desc(pct_constant)) |>
  print(n = Inf)

cat("\n=== A2b. SAME, BY ISSUE ===\n")
diagnostics_temporal_invariance |>
  group_by(issue) |>
  summarise(pct_constant = mean(is_temporally_constant) * 100, n_item_model_pairs = n()) |>
  arrange(desc(pct_constant)) |>
  print(n = Inf)

cat("\n=== A2c. SAME, BY VARIABLE (which specific items are most 'frozen') ===\n")
diagnostics_temporal_invariance |>
  group_by(variable) |>
  summarise(pct_constant = mean(is_temporally_constant) * 100, n_models = n()) |>
  arrange(desc(pct_constant)) |>
  print(n = Inf)

# ==============================================================================
# (B) MIXED-EFFECTS REGRESSION: does salience moderate temporal decay?
#
#     Model:  accuracy_gap_norm ~ distance_from_present * salience
#                                 + mode_switch_2021
#                                 + wording_change_flag
#                                 + (1 | model) + (1 | variable)
#
#     Each piece below exists to address one of the four concerns raised
#     about this design. They are different problems with different fixes.
# ==============================================================================

reg_df <- comp |>
  left_join(salience, by = c("issue", "year")) |>
  mutate(
    # --- CONCERN: none yet, just the core IVs ---
    # distance_from_present is the main "how far back are we asking the model
    # to reach" variable. 2026 is the current data-collection year; change
    # this constant if pipeline is rerun later.
    distance_from_present = 2026 - year,

    # ------------------------------------------------------------------
    # CONCERN 1 (partially addressed here, fully addressed by random effects
    # below): item-level heterogeneity. Some items are near-universal-
    # consensus (abrape) and some are contested (abnomore). If salience
    # happens to correlate with WHICH items are asked, an interaction term
    # without item controls can pick up "which items are salience-heavy"
    # rather than a genuine within-item moderation effect.
    # This mutate() step doesn't fix it by itself -- the fix is the
    # (1 | variable) random intercept in the model formula below, which
    # gives every item its own baseline and forces the distance/salience/
    # interaction coefficients to be estimated NET of item-specific level
    # differences.
    # ------------------------------------------------------------------

    # ------------------------------------------------------------------
    # CONCERN 2 (fully addressed): the suicide wording change. GSS switched
    # from "euthanasia" framing to "assisted suicide" framing around 1990.
    # A model's accuracy could shift right at that boundary for reasons that
    # have nothing to do with opinion change or salience -- it's a measurement
    # artifact of the question itself changing. We give this exact boundary
    # its own dummy variable, set to 0 for every issue except suicide, so the
    # coefficient can only absorb a level-shift specific to that wording
    # switch and cannot contaminate the other three issues' estimates.
    # ------------------------------------------------------------------
    wording_change_flag = as.integer(issue == "suicide" & year >= 1990),

    # ------------------------------------------------------------------
    # CONCERN 3 (fully addressed, as far as the data allow): GSS survey-
    # reliability changes. The clearest, documented break is the 2021 mode
    # switch (in-person interviews -> web/mail self-administration during
    # COVID), which can shift item distributions independent of any real
    # opinion change. We flag that year explicitly so the model can absorb
    # a level-shift there rather than attribute it to distance or salience.
    # NOTE: this does NOT correct for item rotation or GSS sample-size
    # changes across waves, because that metadata isn't accessible.
    # ------------------------------------------------------------------
    # mode_switch_2021 (binary) is dropped -- replaced by the 3-level
    # gss_mode_regime joined in below, which correctly separates the
    # 2021 web-only break from the 2022/2024 multimode bridge years
    # (previously both were silently treated as "back to normal").
  )
  
  reg_df <- reg_df |>
  left_join(gss_mode_lookup, by = "year") |>
  left_join(gss_n_valid |> select(variable, year, gss_sample_n, valid_fraction), by = c("variable", "year"))

  reg_df <- reg_df |>
  mutate(gss_sample_n_norm = gss_sample_n / mean(gss_sample_n, na.rm = TRUE))

# Center the two continuous predictors before building the interaction --
# this makes the main-effect coefficients interpretable (effect at the
# *average* value of the other variable) and reduces collinearity between
# the main effects and the interaction term.
reg_df <- reg_df |>
  mutate(
    dist_c = distance_from_present - mean(distance_from_present, na.rm = TRUE),
    sal_c  = nyt_normalized - mean(nyt_normalized, na.rm = TRUE)
  )

cat("\n=== B0. Regression dataset ===\n")
cat(sprintf("n = %d rows, %d models, %d items\n",
            nrow(reg_df), n_distinct(reg_df$model), n_distinct(reg_df$variable)))

# --- PRIMARY MODEL -----------------------------------------------------------
# (1 | model)    -- CONCERN handled: general model-quality differences
#                   (some models are just better/worse across the board)
#                   get absorbed into a per-model random intercept instead
#                   of leaking into the distance/salience coefficients.
# (1 | variable) -- CONCERN 1 handled: item-level heterogeneity, as above.
primary_model <- lmer(
  accuracy_gap_norm ~ dist_c * sal_c + gss_mode_regime + wording_change_flag +
    (1 | model) + (1 | variable),
  data = reg_df,
  weights = gss_sample_n_norm
)

cat("\n=== B1. PRIMARY MODEL: accuracy_gap ~ distance * salience (+ controls) ===\n")
print(summary(primary_model))
cat("\n95% CIs:\n")
print(confint(primary_model, method = "Wald"))

# Substantive read-out: what is the slope of the decay curve (effect of one
# more year of distance) at low vs. high salience? This is the actual test
# of the moderation hypothesis -- a flat/negative slope at low salience and
# a positive slope at high salience is the "salience unlocks temporal decay"
# pattern; if the slope doesn't move across salience levels, there's no
# moderation regardless of what the raw correlations showed.
b_dist <- fixef(primary_model)["dist_c"]
b_int  <- fixef(primary_model)["dist_c:sal_c"]
sal_q  <- quantile(reg_df$sal_c, c(0.1, 0.5, 0.9), na.rm = TRUE)

cat("\n=== B2. Implied decay slope (change in gap per additional year of distance) ===\n")
cat(sprintf("At low salience  (10th pct): %.5f per year\n", b_dist + b_int * sal_q[1]))
cat(sprintf("At median salience         : %.5f per year\n", b_dist + b_int * sal_q[2]))
cat(sprintf("At high salience (90th pct): %.5f per year\n", b_dist + b_int * sal_q[3]))

regression_primary_summary <- broom.mixed::tidy(primary_model)
write_csv(regression_primary_summary, "data_processed/regression_primary_summary.csv")

# --- CONCERN 4 (NOT fully resolved -- partial sensitivity check only) -------
# Training-data contamination. A model may get a recent year "right" because
# it memorized the actual reported GSS statistic during training, not
# because it inferred period-appropriate opinion. This is observationally
# very close to "distance from present" itself, since virtually all 10
# models here have training cutoffs within the last ~1-2 years of each
# other (mid-2020s), meaning cutoff date barely varies across the model
# roster and is largely collinear with plain recency. It is ALSO correlated
# with general model capability (newer-cutoff models tend to just be more
# capable models), which the (1 | model) random intercept above already
# partly absorbs -- so there isn't much independent variance left for a
# contamination-specific term to explain, even if real cutoff dates can be
# sourced.
#
# What this CAN do: if the models' cutoffs vary by more than a few months,
# it is possible to test whether accuracy on very recent survey years 
# (where secondary reporting has had less time to accumulate before a 
# given model's cutoff) is worse for models with tighter 
# "maturation time" -- i.e., closer cutoff to the survey's release. That 
# is a real, if noisy, contamination probe. It is NOT a clean 
# identification -- this is a sensitivity check alongside the primary 
# model, not a resolution of the confound.
#
# Real cutoff dates below:
model_training_cutoffs <- tribble(
  ~model,              ~cutoff_year,
  "claude_4_6",         2025.58,
  "cogito_v21_671b",    2024.83,
  "command_r",          2023.08,
  "deepseek_v31",       2025.00,
  "deepseek_v32",       2025.17,
  "deepseek_v3_0324",   2024.50,
  "gemini25_lite",      2025.00,
  "gemini_3_flash",     2025.00,
  "gemma4_31b",         2025.00,
  "glm_4_6",            2025.17,
  "gpt4o_mini",         2023.75,
  "gpt5",               2024.67,
  "gpt5_mini",          2024.33,
  "gpt_oss_120b",       2024.42,
  "grok_4_3",           2025.92,
  "jamba_large",        2025.58,
  "kimi_k26",           2025.25,
  "llama31_8b",         2023.92,
  "llama33_70b",        2023.92,
  "llama4_maverick",    2024.58,
  "mistral_large3",     2023.75,
  "mistral_nemo",       2024.25,
  "mistral_small4",     2024.92,
  "qwen3_235b",         2025.42,
  "qwen3_next_80b",     2025.67
)

if (all(is.na(model_training_cutoffs$cutoff_year))) {
  cat("\n=== B3. Contamination sensitivity check SKIPPED ===\n")
  cat("Fill in model_training_cutoffs above with real cutoff dates to run this.\n")
} else {
  reg_df_contam <- reg_df |>
    left_join(model_training_cutoffs, by = "model") |>
    mutate(maturation_time = cutoff_year - year) |>
    filter(!is.na(maturation_time))

  contamination_model <- lmer(
    accuracy_gap_norm ~ dist_c * sal_c + gss_mode_regime + wording_change_flag +
      maturation_time + (1 | model) + (1 | variable),
    data = reg_df_contam,
    weights = gss_sample_n_norm
)

  cat("\n=== B3. CONTAMINATION SENSITIVITY CHECK (partial mitigation only) ===\n")
  print(summary(contamination_model))
  cat("\nCompare the dist_c and dist_c:sal_c coefficients to the primary model above.\n")
  cat("If they barely move, maturation_time isn't explaining away the salience\n")
  cat("effect -- reassuring, but NOT proof contamination is absent, since\n")
  cat("maturation_time is an imperfect, largely collinear proxy (see comment above).\n")

  write_csv(broom.mixed::tidy(contamination_model), "data_processed/regression_contamination_sensitivity.csv")
}

# ==============================================================================
# (C) SALIENCE-CONTROLLED BIVARIATE ANALYSIS (cross-check against B)
# ==============================================================================

yearly <- reg_df |>
  group_by(issue, year) |>
  summarise(
    mean_gap = mean(accuracy_gap_norm, na.rm = TRUE),
    nyt = first(nyt_normalized),
    .groups = "drop"
  )

partial_corr <- function(x, y, z) {
  resx <- residuals(lm(x ~ z))
  resy <- residuals(lm(y ~ z))
  cor.test(resx, resy)
}

loess_detrend_corr <- function(df) {
  df <- df |> arrange(year)
  trend <- fitted(loess(nyt ~ year, data = df, span = 0.75))
  resid_sal <- df$nyt - trend
  cor.test(resid_sal, df$mean_gap)
}

cat("\n=== C. SALIENCE vs ACCURACY GAP (year-level, n = distinct years per issue) ===\n")
for (iss in unique(yearly$issue)) {
  g <- yearly |> filter(issue == iss)
  raw <- cor.test(g$nyt, g$mean_gap)
  part <- partial_corr(g$nyt, g$mean_gap, g$year)
  lo <- tryCatch(loess_detrend_corr(g), error = function(e) NULL)

  cat(sprintf(
    "\n%s (n=%d years):\n  raw r = %.3f (p = %.4f)\n  partial r (year controlled) = %.3f (p = %.4f)\n",
    iss, nrow(g), raw$estimate, raw$p.value, part$estimate, part$p.value
  ))
  if (!is.null(lo)) {
    cat(sprintf("  LOESS-detrended r = %.3f (p = %.4f)\n", lo$estimate, lo$p.value))
  }
}

diagnostics_salience <- yearly

# ------------------------------------------------------------------------
# Write outputs
# ------------------------------------------------------------------------

write_csv(diagnostics_determinism, "data_processed/diagnostics_determinism.csv")
write_csv(diagnostics_temporal_invariance, "data_processed/diagnostics_temporal_invariance.csv")
write_csv(diagnostics_salience, "data_processed/diagnostics_salience.csv")
saveRDS(primary_model, "data_processed/primary_model.rds")
saveRDS(reg_df, "data_processed/reg_df_final.rds")

cat("\nDone.\n")