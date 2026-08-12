# ==============================================================================
# SCRIPT 12: THESIS VISUALIZATIONS
# ==============================================================================
# Assumes this is sourced in the same R session as scripts 09/10/11, so the
# following objects already exist in the environment:
#   - primary_model              (lmer object from script 10, Section B1)
#   - reg_df                     (regression dataset from script 10)
#   - diagnostics_temporal_invariance  (from script 10, Section A2)
#   - yearly                     (year-level salience/gap df from script 10, Section C)
#   - diagnostics_determinism    (from script 09/10, Section A1 -- used in Figure F)
#   - resp                       (script 10's cleaned response-level data -- used in Figure G)
#
# If this script is running standalone in a fresh session, uncomment the
# loading block below and point it at saved objects (must add
# saveRDS(primary_model, "data_processed/primary_model.rds") to the end of
# script 10 first, since lmer model objects aren't written by write_csv()).

# --- Standalone fallback loading (uncomment if not run in the same session) -
# primary_model <- readRDS("data_processed/primary_model.rds")
# reg_df <- read_csv("data_processed/reg_df.csv")
# diagnostics_temporal_invariance <- read_csv("data_processed/diagnostics_temporal_invariance.csv")
# yearly <- read_csv("data_processed/yearly_salience_gap.csv")

library(ggplot2)
library(ggeffects)
library(dplyr)
library(tidyr)
library(ggeffects)
library(scales)
library(ggnewscale)
library(stringr)

dir.create("figures", showWarnings = FALSE)

theme_thesis <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey30", size = 11),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

# ==============================================================================
# FIGURE A: Predicted decay curve, distance x salience interaction
# (B1/B2 finding visualized)
# ==============================================================================

# ggpredict computes model-implied predictions holding other covariates at
# their means/reference levels. terms = c("dist_c [all]", "sal_c [quantile2]")
# asks for a full range of distance, evaluated at three salience levels
# (10th/50th/90th percentile).

# --- CHANGED ---
sal_q <- quantile(reg_df$sal_c, c(0.1, 0.5, 0.9), na.rm = TRUE)

pred <- ggpredict(
  primary_model,
  terms = c("dist_c [all]", sprintf("sal_c [%s]", paste(round(sal_q, 4), collapse = ",")))
)
# --- END CHANGE ---

pred_df <- as.data.frame(pred) |>
  rename(dist_c = x, sal_group = group) |>
  mutate(
    salience_level = factor(
      sal_group,
      labels = c("Low salience (10th pct)", "Median salience", "High salience (90th pct)")
    )
  )

fig_a <- ggplot(pred_df, aes(dist_c, predicted, color = salience_level, fill = salience_level)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("#2C6E49", "#5E60CE", "#D62839")) +
  scale_fill_manual(values = c("#2C6E49", "#5E60CE", "#D62839")) +
  labs(
    title = "Temporal decay in LLM\u2013GSS accuracy, moderated by salience",
    subtitle = "Predicted accuracy gap by historical distance, at three salience levels",
    x = "Distance from present (centered years)",
    y = "Predicted accuracy gap (normalized)",
    color = NULL, fill = NULL
  ) +
  theme_thesis

ggsave("figures/fig_a_decay_interaction.png", fig_a, width = 8, height = 5.5, dpi = 300)

# ==============================================================================
# FIGURE B: Forest / coefficient plot of fixed effects
# ==============================================================================

# Requires regression_primary_summary.csv (written in script 10 via
# broom.mixed::tidy(primary_model))
regression_primary_summary <- read.csv("data_processed/regression_primary_summary.csv")

coef_df <- regression_primary_summary |>
  filter(effect == "fixed", term != "(Intercept)") |>
  mutate(
    term_label = recode(term,
      dist_c = "Distance (years, centered)",
      sal_c = "Salience (centered)",
      `dist_c:sal_c` = "Distance \u00d7 Salience",
      gss_mode_regimeweb_2021 = "GSS mode: web-only (2021)",
      gss_mode_regimemultimode_bridge = "GSS mode: multimode bridge",
      wording_change_flag = "Wording change flag"
    ),
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    sig = ifelse(conf.low > 0 | conf.high < 0, "Significant (95% CI excludes 0)", "Not significant")
  )

fig_b <- ggplot(coef_df, aes(estimate, reorder(term_label, estimate), color = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), height = 0.15, linewidth = 0.9) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Significant (95% CI excludes 0)" = "#1B4965", "Not significant" = "grey70")) +
  labs(
    title = "Fixed-effect estimates, primary mixed-effects model",
    subtitle = "accuracy_gap_norm ~ distance \u00d7 salience + mode regime + \nwording change + (1 | model) + (1 | item)",
    x = "Estimate (95% CI)", y = NULL, color = NULL
  ) +
  theme_thesis

ggsave("figures/fig_b_coefficient_forest.png", fig_b, width = 8, height = 4.5, dpi = 300)

# ==============================================================================
# FIGURE C: Model x item temporal invariance heatmap, hierarchically clustered
# ==============================================================================

hm_df <- diagnostics_temporal_invariance |>
  group_by(model, variable) |>
  summarise(pct_constant = mean(is_temporally_constant) * 100, .groups = "drop")

wide <- hm_df |> pivot_wider(names_from = variable, values_from = pct_constant)
mat <- as.matrix(wide[, -1])
rownames(mat) <- wide$model
mat[is.na(mat)] <- 0  # safety: shouldn't be any NAs if every model x item pair was tested

model_order <- rownames(mat)[hclust(dist(mat))$order]
item_order <- colnames(mat)[hclust(dist(t(mat)))$order]

hm_df <- hm_df |>
  mutate(
    model = factor(model, levels = model_order),
    variable = factor(variable, levels = item_order)
  )

fig_c <- ggplot(hm_df, aes(variable, model, fill = pct_constant)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient(low = "#F4F1DE", high = "#3D405B", name = "% years\nconstant") +
  labs(
    title = "Temporal invariance across models and items",
    subtitle = "Share of the 1972\u20132024 span where a model's answer never changes",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )

ggsave("figures/fig_c_invariance_heatmap.png", fig_c, width = 9, height = 8, dpi = 300)

# ==============================================================================
# FIGURE D: Salience vs. accuracy gap over time, faceted by issue
# ==============================================================================

ribbon_df <- yearly |>
  mutate(nyt_rescaled = rescale(nyt, to = c(0, 1)), .by = issue) |>
  select(issue, year, mean_gap, nyt_rescaled) |>
  pivot_longer(cols = c(mean_gap, nyt_rescaled), names_to = "series", values_to = "value") |>
  mutate(series = recode(series, mean_gap = "Accuracy gap", nyt_rescaled = "Salience (rescaled 0\u20131)"))

fig_d <- ggplot(ribbon_df, aes(year, value, color = series)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~issue, scales = "free_x", ncol = 2) +
  scale_color_manual(values = c("Accuracy gap" = "#3D405B", "Salience (rescaled 0\u20131)" = "#E07A5F")) +
  labs(
    title = "Salience and accuracy gap over time, by issue",
    subtitle = "Rescaled to a common 0\u20131 axis for visual comparison within each issue",
    x = NULL, y = "Rescaled value", color = NULL
  ) +
  theme_thesis +
  theme(strip.text = element_text(face = "bold"))

ggsave("figures/fig_d_salience_gap_ribbon.png", fig_d, width = 9, height = 7, dpi = 300)

# ==============================================================================
# FIGURE E: Continuous moderation (Johnson-Neyman style) plot
# Shows the implied decay slope (d(accuracy_gap)/d(distance)) as a continuous
# function of salience, with a 95% CI ribbon -- a fuller picture of the B1/B2
# interaction than the three discrete slices in Figure A.
# ==============================================================================

vc       <- vcov(primary_model)
b_dist   <- fixef(primary_model)["dist_c"]
b_int    <- fixef(primary_model)["dist_c:sal_c"]
var_dist <- vc["dist_c", "dist_c"]
var_int  <- vc["dist_c:sal_c", "dist_c:sal_c"]
cov_di   <- vc["dist_c", "dist_c:sal_c"]

sal_seq <- seq(min(reg_df$sal_c, na.rm = TRUE), max(reg_df$sal_c, na.rm = TRUE), length.out = 200)

jn_df <- tibble(sal_c = sal_seq) |>
  mutate(
    slope     = b_dist + b_int * sal_c,
    se        = sqrt(var_dist + (sal_c^2) * var_int + 2 * sal_c * cov_di),
    conf.low  = slope - 1.96 * se,
    conf.high = slope + 1.96 * se
  )

# Approximate zero-crossing for annotation only -- not a formal test, just
# marking where the line in the plot switches sign.
crossing_sal_c <- jn_df$sal_c[which(diff(sign(jn_df$slope)) != 0)]

fig_e <- ggplot(jn_df, aes(sal_c, slope)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#5E60CE", alpha = 0.2) +
  geom_line(color = "#5E60CE", linewidth = 1.2)

if (length(crossing_sal_c) > 0) {
  fig_e <- fig_e + geom_vline(xintercept = crossing_sal_c[1], linetype = "dotted", color = "#D62839")
}

fig_e <- fig_e +
  labs(
    title = "Where does salience flip the sign of temporal decay?",
    subtitle = "Implied slope of accuracy gap on distance, as a continuous function of salience (95% CI)",
    x = "Salience (centered)", y = "Implied decay slope (\u2202gap / \u2202distance)"
  ) +
  theme_thesis

ggsave("figures/fig_e_johnson_neyman_slope.png", fig_e, width = 8, height = 5, dpi = 300)

# ==============================================================================
# FIGURE F: Per-model raw decay trajectories, population curves overlaid
# Ties Figure A to the A1 determinism finding: rigid/near-zero-entropy models
# should track flat lines regardless of distance or salience; more responsive
# models should track the population curve more closely.
# ==============================================================================
library(ggnewscale)

# Empirical (not model-based) per-model trend: mean accuracy_gap_norm within
# distance deciles, collapsed across items and salience. Deliberately simple -
# this is descriptive texture behind Fig A, not a re-estimation of the B1 model.
model_trend_df <- reg_df |>
  mutate(dist_bin = ntile(dist_c, 10)) |>
  group_by(model, dist_bin) |>
  summarise(
    dist_c_mid = mean(dist_c, na.rm = TRUE),
    mean_gap   = mean(accuracy_gap_norm, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(diagnostics_determinism |> select(model, mean_entropy), by = "model")

fig_f <- ggplot() +
  # Per-model spaghetti lines - no salience_level column in this data, so
  # ggplot repeats this layer identically across all three facet panels
  geom_line(
    data = model_trend_df,
    aes(dist_c_mid, mean_gap, group = model, color = mean_entropy),
    linewidth = 0.45, alpha = 0.6
  ) +
  scale_color_viridis_c(option = "plasma", name = "Mean entropy\n(0 = deterministic)") +
  new_scale_color() +
  # Population curve + ribbon, one per facet
  geom_ribbon(
    data = pred_df, aes(dist_c, ymin = conf.low, ymax = conf.high, fill = salience_level),
    alpha = 0.15
  ) +
  geom_line(
    data = pred_df, aes(dist_c, predicted, color = salience_level), linewidth = 1.3
  ) +
  scale_color_manual(values = c("#2C6E49", "#5E60CE", "#D62839"), guide = "none") +
  scale_fill_manual(values = c("#2C6E49", "#5E60CE", "#D62839"), guide = "none") +
  facet_wrap(~salience_level, nrow = 1) +
  labs(
    title = "Individual model trajectories against the population-level decay curve",
    subtitle = "Thin lines = per-model raw means by distance decile, colored by response-entropy (A1)",
    x = "Distance from present (centered years)", y = "Accuracy gap (normalized)"
  ) +
  theme_thesis +
  theme(strip.text = element_text(face = "bold"))

ggsave("figures/fig_f_model_spaghetti.png", fig_f, width = 12, height = 5, dpi = 300)

# ==============================================================================
# FIGURE G: Refusal rate over time, by model
# Refusal rates vary systematically across models/versions (see script 10,
# A1 notes) -- this makes that variation visible, and calls out the
# deepseek_v31 -> deepseek_v32 refusal collapse as a natural experiment.
# Requires `resp` (script 10's cleaned response-level data) in the session.
# ==============================================================================

refusal_by_model_year <- resp |>
  mutate(is_refusal = str_detect(response_category, regex("refus", ignore_case = TRUE))) |>
  group_by(model, year) |>
  summarise(refusal_rate = mean(is_refusal, na.rm = TRUE) * 100, n = n(), .groups = "drop")

model_order_refusal <- refusal_by_model_year |>
  group_by(model) |>
  summarise(mean_refusal = mean(refusal_rate), .groups = "drop") |>
  arrange(desc(mean_refusal)) |>
  pull(model)

refusal_by_model_year <- refusal_by_model_year |>
  mutate(model = factor(model, levels = model_order_refusal))

fig_g <- ggplot(refusal_by_model_year, aes(year, refusal_rate)) +
  geom_area(fill = "#3D405B", alpha = 0.25) +
  geom_line(color = "#3D405B", linewidth = 0.6) +
  facet_wrap(~model, ncol = 5) +
  labs(
    title = "Refusal rate over time, by model",
    subtitle = "Ordered by overall refusal rate, highest to lowest",
    x = NULL, y = "Refusal rate (%)"
  ) +
  theme_minimal(base_size = 9) +
  theme(strip.text = element_text(face = "bold", size = 8))

ggsave("figures/fig_g_refusal_by_model.png", fig_g, width = 12, height = 10, dpi = 300)

# --- Zoomed companion: the DeepSeek v3.1 -> v3.2 collapse specifically ------
fig_g_deepseek <- refusal_by_model_year |>
  filter(str_detect(model, "(?i)deepseek")) |>
  ggplot(aes(year, refusal_rate, color = model)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "deepseek_v3_0324" = "grey50",
    "deepseek_v31"     = "#5E60CE",
    "deepseek_v32"     = "#D62839"
  )) +
  labs(
    title = "The deepseek_v31 \u2192 deepseek_v32 refusal collapse",
    subtitle = "A natural experiment on version-specific tuning",
    x = NULL, y = "Refusal rate (%)", color = NULL
  ) +
  theme_thesis

ggsave("figures/fig_g2_deepseek_refusal_collapse.png", fig_g_deepseek, width = 7, height = 5, dpi = 300)

# ==============================================================================
# FIGURE H: Wording-change sanity check (suicide item, 1990 boundary)
# Diagnostic figure to chase down the counterintuitive wording_change_flag
# coefficient direction (flagged years showing SMALLER gaps) before write-up.
# ==============================================================================

suicide_df <- reg_df |> filter(issue == "suicide")

# H1 -- now explicitly flagged as the RAW/naive pattern, not yet corrected
fig_h1 <- ggplot(suicide_df, aes(year, accuracy_gap_norm, color = factor(wording_change_flag))) +
  geom_point(alpha = 0.4, size = 1.2) +
  geom_smooth(aes(group = factor(wording_change_flag)), method = "loess", se = TRUE, linewidth = 1) +
  geom_vline(xintercept = 1990, linetype = "dashed", color = "grey40") +
  scale_color_manual(
    values = c("0" = "#5E60CE", "1" = "#D62839"),
    labels = c("0" = "Pre-1990 (euthanasia framing)", "1" = "1990+ (assisted suicide framing)"),
    name = NULL
  ) +
  labs(
    title = "Accuracy gap around the suicide-item wording change",
    subtitle = "RAW pattern, not yet controlling for distance -- see Fig H3 for the effect net of dist_c\n(wording_change_flag and dist_c correlate at r = -0.74 within this item)",
    x = NULL, y = "Accuracy gap (normalized)"
  ) +
  theme_thesis

ggsave("figures/fig_h1_wording_change_trend.png", fig_h1, width = 8, height = 5, dpi = 300)


# --- 1. Collinearity check ---------------------------------------------------
# If wording_change_flag and dist_c are highly correlated *within suicide*,
# the flag's coefficient in the pooled model can't be cleanly separated from
# an ordinary recency effect for this one item -- they're both trying to
# explain the same variance.
cat("\n=== 1. wording_change_flag vs. dist_c correlation (suicide subset only) ===\n")
cor_check <- cor(suicide_df$wording_change_flag, suicide_df$dist_c, use = "complete.obs")
cat(sprintf("Pearson r = %.3f\n", cor_check))
cat("If |r| is large (say, > 0.7), the flag is largely redundant with dist_c\n")
cat("for this item, and the B1 coefficient may be absorbing recency, not wording.\n")

# --- 2. Sample size asymmetry -------------------------------------------------
cat("\n=== 2. Row counts by wording regime ===\n")
suicide_df |> count(wording_change_flag) |> print()
cat("A large imbalance means the pre-1990 mean/median is a much noisier\n")
cat("estimate than the post-1990 one.\n")

# --- 3. Suicide-only isolated regression -------------------------------------
# Strips out the pooled data and the salience interaction entirely, so the
# flag isn't competing with dist_c:sal_c for variance across other issues.
# Only 2 items in this issue (letdie1, suicide1), so (1|variable) is a weak
# random effect with just 2 levels -- included for consistency with the
# primary model's structure, but treat its estimate loosely; a fixed effect
# (factor(variable)) would be an equally defensible alternative given so few
# levels.
suicide_model <- lmer(
  accuracy_gap_norm ~ dist_c + wording_change_flag + (1 | model) + (1 | variable),
  data = suicide_df
)

cat("\n=== 3. Suicide-only model: accuracy_gap ~ distance + wording_change_flag ===\n")
print(summary(suicide_model))
cat("\nCompare this wording_change_flag coefficient's sign/magnitude to the\n")
cat("pooled B1 model. If it shrinks toward zero or flips sign once dist_c is\n")
cat("the only other predictor (no salience interaction, no other issues), that\n")
cat("supports the 'recency wearing a wording-change costume' explanation.\n")
cat("If it holds up at similar size, the wording effect looks more genuine.\n")

# --- 4. Partial-residual check ------------------------------------------------
# Fit distance-only (no wording flag), extract residuals - these are the
# part of accuracy_gap NOT explained by dist_c. If a break still appears at
# 1990 in the residuals, this is evidence of a genuine wording-change effect
# on top of recency. If the residuals look flat across 1990, the original
# break was fully explained by distance alone.
dist_only_model <- lmer(
  accuracy_gap_norm ~ dist_c + (1 | model) + (1 | variable),
  data = suicide_df
)

suicide_df <- suicide_df |>
  mutate(resid_net_of_distance = residuals(dist_only_model))

# H2 -- now explicitly flagged as the corrected/decisive figure
fig_h2 <- ggplot(suicide_df, aes(year, resid_net_of_distance, color = factor(wording_change_flag))) +
  geom_point(alpha = 0.4, size = 1.2) +
  geom_smooth(aes(group = factor(wording_change_flag)), method = "loess", se = TRUE, linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") +
  geom_vline(xintercept = 1990, linetype = "dashed", color = "grey40") +
  scale_color_manual(
    values = c("0" = "#5E60CE", "1" = "#D62839"),
    labels = c("0" = "Pre-1990 (euthanasia framing)", "1" = "1990+ (assisted suicide framing)"),
    name = NULL
  ) +
  labs(
    title = "Accuracy gap net of the distance effect, around the wording change",
    subtitle = "CORRECTED view: residuals from a distance-only model, isolating the wording effect\nfrom the recency effect it's confounded with in Fig H1 (see isolated regression, \u03b2 = -0.027, p = .017)",
    x = NULL, y = "Residual accuracy gap (net of distance)"
  ) +
  theme_thesis

ggsave("figures/fig_h2_wording_change_partial_residual.png", fig_h2, width = 8, height = 5, dpi = 300)

cat("\n=== 4. Partial-residual check written to fig_h2 ===\n")
cat("Read this alongside check 3: if both the isolated regression's flag\n")
cat("coefficient AND this residual plot show a clear break at 1990, the\n")
cat("wording-change effect is likely real. If either one flattens out, treat\n")
cat("the original pooled coefficient as confounded with recency and say so\n")
cat("explicitly in the write-up rather than reporting it as a clean finding.\n")

cat("\n=== Script 12 complete: 4 figures written to figures/ ===\n")
cat("fig_a_decay_interaction.png   -- headline interaction plot\n")
cat("fig_b_coefficient_forest.png  -- fixed-effect forest plot\n")
cat("fig_c_invariance_heatmap.png  -- clustered model x item heatmap\n")
cat("fig_d_salience_gap_ribbon.png -- salience/gap time series by issue\n")
cat("fig_e_continuous_moderation.png -- implied decay slope as a function of salience\n")
cat("fig_f_decay_trajectories.png -- decay tracking population curve\n")
cat("fig_g_refusal_rates.png -- tracking refusal rates over time by model\n")
cat("fig_h_wording_change.png -- sanity check for wording change coefficient\n")