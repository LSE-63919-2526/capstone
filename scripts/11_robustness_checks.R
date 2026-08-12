# ==============================================================================
# 11_robustness_checks.R
#
# Purpose: stress-test the findings from 10_structural_and_salience_analysis.R
# before they go in the write-up.
#
# Six checks, grouped:
#
#   PART 1 -- was the event-study design (discussed but not built into the
#             pipeline) actually a good idea?
#     (1) Placebo event test: apply the same pre/post split logic to an
#         issue with NO real-world shock, at the same year thresholds used
#         for abortion (Dobbs, 2022) and police_violence (BLM, 2020). If a
#         "break" of similar size shows up in the placebo, the design can't
#         distinguish a real shock from noise.
#
#   PART 2 -- is the bivariate salience-vs-gap correlation from Section C
#             robust, or was it an artifact of a few points?
#     (2) Spearman (rank) correlation -- raw, by issue.
#     (3) Partial Spearman, controlling for year -- the rank-based analog of
#         the linear partial correlation reported earlier. This is the most
#         important check in the file; read the note before the code.
#     (4) Outlier sensitivity: does dropping the single most extreme
#         salience-year (abortion 2022, salience=100) change the sign or
#         size of the correlation?
#     (5) Alternate salience source: does Google Trends (available 2004+)
#         agree with the NYT-based index, where they overlap?
#
#   PART 3 -- is the regression interaction from Section B driven by one
#             model, or by within-year pseudo-replication understating the
#             standard errors?
#     (6) Leave-one-model-out refits of the primary regression.
#     (7) Cluster-robust standard errors (clustered by year) via clubSandwich,
#         as a check on whether the lmer model's default SEs are overstating
#         confidence given that many rows share the same year's salience value.
#
# Inputs: same three files as script 10.
# Output: data_processed/robustness_placebo.csv
#         data_processed/robustness_spearman.csv
#         data_processed/robustness_loo.csv
#         data_processed/robustness_cluster_robust_se.csv
# ==============================================================================

library(tidyverse)
library(lme4)
library(lmerTest)
library(lmtest)
library(broom.mixed)
library(clubSandwich)

# ------------------------------------------------------------------------
# 0. Rebuild the same data objects as script 10
# ------------------------------------------------------------------------

reg_df <- readRDS("data_processed/reg_df_final.rds")

yearly <- reg_df |>
  group_by(issue, year) |>
  summarise(
    mean_gap = mean(accuracy_gap_norm, na.rm = TRUE),
    nyt = first(nyt_normalized),
    gtrends = first(gtrends_normalized),
    .groups = "drop"
  )

# ==============================================================================
# PART 1: PLACEBO EVENT TEST
# ==============================================================================
# Logic: for each REAL event (issue, threshold year), compare mean accuracy_gap
# in years before vs. at/after that threshold (Welch t-test, unequal variance --
# post-event samples are tiny, so can't assume equal variance).
# Then run the IDENTICAL split on an issue that had no comparable real-world
# shock at that date (gender_roles, whose salience series is smooth/monotonic
# rather than spiky -- see script 10 Section C). If gender_roles shows a
# "break" of similar size and significance at the SAME threshold years used
# for the real events, that's evidence the design can't tell a real shock
# apart from ordinary year-to-year drift with a small post-period sample --
# which is exactly the concern raised about the event-study idea before it
# was dropped from the main pipeline.

pre_post_break <- function(df, event_year, label) {
  pre <- df$mean_gap[df$year < event_year]
  post <- df$mean_gap[df$year >= event_year]
  if (length(pre) < 3 || length(post) < 2) {
    return(tibble(label = label, event_year = event_year, n_pre = length(pre),
                   n_post = length(post), mean_pre = NA, mean_post = NA,
                   diff = NA, t = NA, p = NA))
  }
  tt <- t.test(post, pre)
  tibble(label = label, event_year = event_year, n_pre = length(pre), n_post = length(post),
         mean_pre = mean(pre), mean_post = mean(post), diff = mean(post) - mean(pre),
         t = unname(tt$statistic), p = tt$p.value)
}

placebo_results <- bind_rows(
  pre_post_break(yearly |> filter(issue == "abortion"), 2022,
                 "abortion @ Dobbs (2022, REAL)"),
  pre_post_break(yearly |> filter(issue == "police_violence"), 2020,
                 "police_violence @ BLM (2020, REAL)"),
  pre_post_break(yearly |> filter(issue == "gender_roles"), 2022,
                 "gender_roles @ 2022 (PLACEBO, matches abortion threshold)"),
  pre_post_break(yearly |> filter(issue == "gender_roles"), 2020,
                 "gender_roles @ 2020 (PLACEBO, matches police_violence threshold)"),
  pre_post_break(yearly |> filter(issue == "abortion"), 2000,
                 "abortion @ 2000 (PLACEBO, arbitrary/no known shock)"),
  pre_post_break(yearly |> filter(issue == "police_violence"), 2000,
                 "police_violence @ 2000 (PLACEBO, arbitrary/no known shock)")
)

cat("\n=== 1. PLACEBO EVENT TEST ===\n")
cat("If placebo rows show breaks as large/significant as the real-event rows,\n")
cat("the event-study design is not distinguishing shocks from noise.\n\n")
print(placebo_results, n = Inf, width = Inf)

write_csv(placebo_results, "data_processed/robustness_placebo.csv")

# ==============================================================================
# PART 2: BIVARIATE CORRELATION ROBUSTNESS
# ==============================================================================

# --- (2) Raw Spearman (rank) correlation, by issue ---------------------------
# Spearman only cares about rank order, not the actual magnitude of an
# extreme value -- so it's naturally resistant to a single outlier point
# (e.g. abortion's 2022 salience spike to 100) in a way Pearson isn't.
spearman_raw <- yearly |>
  group_by(issue) |>
  summarise(
    spearman_r = cor(nyt, mean_gap, method = "spearman"),
    p = cor.test(nyt, mean_gap, method = "spearman")$p.value,
    n = n(),
    .groups = "drop"
  )

cat("\n=== 2. RAW SPEARMAN CORRELATION (salience vs. gap, by issue) ===\n")
print(spearman_raw, n = Inf)

# --- (3) Partial Spearman, controlling for year -----------------------------
# THIS IS THE IMPORTANT ONE. Earlier, a linear partial correlation
# (controlling for year via OLS residuals) found abortion and police_violence
# survived the year control (partial r = -0.47 and +0.41). That test assumes
# a roughly linear relationship and is sensitive to influential points - the
# 2022 abortion spike, in particular, has a lot of leverage on a linear fit
# with only 34 data points. Rank-transforming both variables before doing the
# same partial-correlation logic removes that sensitivity.
partial_spearman <- function(x, y, z) {
  rx <- rank(x); ry <- rank(y); rz <- rank(z)
  resx <- residuals(lm(rx ~ rz))
  resy <- residuals(lm(ry ~ rz))
  cor.test(resx, resy)
}

cat("\n=== 3. PARTIAL SPEARMAN (year-controlled) -- compare to script 10's linear partial r ===\n")
for (iss in unique(yearly$issue)) {
  g <- yearly |> filter(issue == iss)
  pc <- partial_spearman(g$nyt, g$mean_gap, g$year)
  cat(sprintf("%s: partial spearman r = %.3f (p = %.4f), n = %d\n",
              iss, pc$estimate, pc$p.value, nrow(g)))
}
cat("\nIf these are all small and non-significant while script 10's linear partial\n")
cat("correlations were large and significant, the earlier issue-level findings for\n")
cat("abortion/police_violence should be treated as fragile, not confirmed --\n")
cat("the POOLED regression in script 10 Section B is a separate, more stable test\n")
cat("(see Part 3 below) and isn't undermined by this even if the simple bivariate\n")
cat("correlations are.\n")

# --- (4) Outlier sensitivity: drop the single most extreme salience-year ---
cat("\n=== 4. OUTLIER CHECK: abortion with vs. without the 2022 salience spike ===\n")
ab <- yearly |> filter(issue == "abortion")
ab_no_outlier <- ab |> filter(year != 2022)
r_with <- cor.test(ab$nyt, ab$mean_gap)
r_without <- cor.test(ab_no_outlier$nyt, ab_no_outlier$mean_gap)
cat(sprintf("WITH 2022:    r = %.3f (p = %.4f)\n", r_with$estimate, r_with$p.value))
cat(sprintf("WITHOUT 2022: r = %.3f (p = %.4f)\n", r_without$estimate, r_without$p.value))
cat("A sign flip or large magnitude change here means the raw correlation is\n")
cat("substantially driven by one data point -- a 34-point series can't support\n")
cat("much more than that from a single influential year.\n")

# --- (5) Alternate salience source: Google Trends (2004+) ------------------
cat("\n=== 5. ALTERNATE SALIENCE SOURCE: Google Trends vs. NYT-based index ===\n")
cat("Coverage is much sparser (2004+ only, and gtrends_normalized has its own\n")
cat("missingness) -- treat any null result here as underpowered, not disconfirming.\n\n")
for (iss in unique(yearly$issue)) {
  g <- yearly |> filter(issue == iss) |> drop_na(gtrends)
  if (nrow(g) < 5) {
    cat(sprintf("%s: insufficient Google Trends coverage (n = %d) -- skipped\n", iss, nrow(g)))
    next
  }
  rt <- cor.test(g$gtrends, g$mean_gap)
  cat(sprintf("%s: gtrends r = %.3f (p = %.4f), n = %d\n", iss, rt$estimate, rt$p.value, nrow(g)))
}

spearman_summary <- yearly |>
  group_by(issue) |>
  summarise(
    spearman_raw = cor(nyt, mean_gap, method = "spearman"),
    .groups = "drop"
  )
write_csv(spearman_summary, "data_processed/robustness_spearman.csv")

# ==============================================================================
# PART 3: REGRESSION ROBUSTNESS (script 10, Section B)
# ==============================================================================

# --- (6) Leave-one-model-out: does excluding any single model change the ---
# --- sign or significance of the distance x salience interaction? ----------
loo_results <- list()
for (m in unique(reg_df$model)) {
  sub <- reg_df |> filter(model != m)
  fit <- lmer(
    accuracy_gap_norm ~ dist_c * sal_c + gss_mode_regime + wording_change_flag +
      (1 | model) + (1 | variable),
    data = sub
  )
  co <- summary(fit)$coefficients
  loo_results[[m]] <- tibble(
    excluded_model = m,
    interaction_coef = co["dist_c:sal_c", "Estimate"],
    interaction_p = co["dist_c:sal_c", "Pr(>|t|)"]
  )
}
loo_df <- bind_rows(loo_results)

cat("\n=== 6. LEAVE-ONE-MODEL-OUT: distance x salience interaction stability ===\n")
print(loo_df, n = Inf)
cat(sprintf("\nRange of interaction coefficient across exclusions: [%.6f, %.6f]\n",
            min(loo_df$interaction_coef), max(loo_df$interaction_coef)))
cat("If this range is small relative to the coefficient itself and every\n")
cat("exclusion stays significant, no single model is driving the finding.\n")

write_csv(loo_df, "data_processed/robustness_loo.csv")

# --- (7) Cluster-robust SEs (by year), via clubSandwich ---------------------
# Many rows share the exact same year's salience value (every model, every
# item, in a given year, sees the same nyt_normalized number) - 
# pseudo-replication issue raised earlier. lmer's default SEs don't account
# for this kind of clustering on a predictor that repeats within year.
# clubSandwich computes cluster-robust ("sandwich") standard errors that are
# valid even when errors are correlated within a cluster (here: within year).
# If the interaction survives with cluster-robust SEs, the earlier model-
# based inference wasn't materially overstating confidence; if it doesn't,
# the naive SEs were too optimistic.
if (!requireNamespace("clubSandwich", quietly = TRUE)) {
  cat("\n=== 7. CLUSTER-ROBUST SE CHECK SKIPPED (install 'clubSandwich' to run) ===\n")
} else {
library(clubSandwich)

# NOTE: vcovCR.lmerMod (clubSandwich's method for lmer objects) requires
# NESTED random effects. Our (1|model) and (1|variable) are CROSSED -- every
# model answers every item -- so it can't be applied directly to a mixed
# model here. We refit as two-way fixed effects (dummies for model and item
# instead of random intercepts) and cluster by year on that instead;
# clubSandwich handles plain lm objects with crossed structure without
# issue. This also fixes two bugs in the previous refit: it referenced the
# retired `mode_switch_2021` flag instead of the current `gss_mode_regime`
# factor, and it was unweighted, so it didn't match the primary model.
fe_model <- lm(
  accuracy_gap_norm ~ dist_c * sal_c + gss_mode_regime + wording_change_flag +
    factor(model) + factor(variable),
  data = reg_df,
  weights = gss_sample_n_norm
)
cr_test <- coef_test(fe_model, vcov = "CR2", cluster = reg_df$year, test = "Satterthwaite")

cat("\n=== 7. CLUSTER-ROBUST SE (clustered by year) vs. MODEL-BASED SE ===\n")
print(cr_test[cr_test$Coef %in% c("dist_c", "sal_c", "dist_c:sal_c"), ])
cat("\nCompare the 'dist_c:sal_c' row's SE and p-value here to the primary\n")
cat("model's output in script 10 -- an inflated but still-significant SE\n")
cat("means the finding survives properly accounting for within-year clustering.\n")

write_csv(as.data.frame(cr_test), "data_processed/robustness_cluster_robust_se.csv")
}

cat("\nDone.\n")
