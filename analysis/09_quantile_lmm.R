# =============================================================
# 09_quantile_lmm.R
# Distributional yield response, profitability, and NPV via
# quantile regression on within-field yield responses
#
# CONCEPTUAL STORY:
#   Lime response is not a single number — it varies enormously
#   across fields due to differences in soil chemistry, crop
#   variety, management, and micro-topography. The LMM CI band
#   (e.g. [0.63, 0.78] t/ha for maize T2) describes uncertainty
#   in the MEAN across all farms; it is narrow precisely because
#   the mean is well-estimated. It tells us nothing about whether
#   a SPECIFIC farm will gain 0.1 or 2.0 t/ha.
#
#   This variation propagates directly into profitability and NPV:
#   - Optimistic farms (Q80) are clearly profitable at low lime rates
#   - Pessimistic farms (Q20) may lose money even at 1 t/ha
#   - The typical farm (Q50) sits in between
#
#   Understanding this distribution matters for:
#   (a) Extension targeting: focus lime promotion on high-response
#       fields, not blanket recommendations
#   (b) Risk communication: credit-constrained farmers who cannot
#       absorb a bad outcome need to know the downside
#   (c) Policy design: if Q20 farms are consistently unprofitable,
#       risk-sharing instruments (credit, insurance) are needed
#
# METHOD — Quantile regression on within-field yield responses:
#   1. Compute yield_resp_obs = yield_tha − field_control_mean
#      This "within-field differencing" removes the field random
#      intercept (Koenker 2004 within-estimator / Canay 2011).
#   2. Fit rq(yield_resp_obs ~ lime_tha + I(lime_tha^2), tau = τ)
#      for τ ∈ {0.20, 0.50, 0.80}.
#      Q20 = bottom quintile of farms ("pessimistic")
#      Q50 = median farm ("typical")
#      Q80 = top quintile of farms ("optimistic")
#   3. Compute year-1 profit and 4-year NPV for each quantile.
#
# Outputs:
#   outputs/quantile_response_curves.csv
#   outputs/quantile_response_summary.csv
#   outputs/quantile_profit_npv_curves.csv
#   outputs/fig_quantile_response.png
#   outputs/fig_quantile_profit.png
#   outputs/fig_quantile_npv.png
# =============================================================

pacman::p_load(dplyr, readr, tidyr, purrr, ggplot2, quantreg, extrafont)
source("R/helpers_econ.R")   # pv_factor()

extrafont::loadfonts(quiet = TRUE)
FONT  <- "Times New Roman"
CROPS <- c("Maize", "Beans", "Soybean", "Wheat", "Fababean")
TAUS  <- c(0.20, 0.50, 0.80)

# Three-scenario palette: pessimistic | typical | optimistic
tau_colors <- c(
  "Q20" = "#C0392B",   # red   — bottom quintile (pessimistic)
  "Q50" = "#1A3A5C",   # navy  — median (typical)
  "Q80" = "#27AE60"    # green — top quintile (optimistic)
)

# NPV parameters (consistent with 04_profitability.R)
NPV_T     <- 4
NPV_R     <- 0.10
NPV_DECAY <- 0.25
PV        <- pv_factor(NPV_T, NPV_R, NPV_DECAY)
cat(sprintf("PV factor = %.4f\n", PV))

theme_academic <- function(base_size = 12) {
  theme_bw(base_size = base_size, base_family = FONT) +
    theme(
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(color = "grey88", linewidth = 0.35),
      strip.background  = element_rect(fill = "grey95", color = "grey60"),
      strip.text        = element_text(face = "bold", size = base_size - 1),
      axis.title        = element_text(size = base_size),
      axis.text         = element_text(size = base_size - 1),
      legend.background = element_blank(),
      legend.key        = element_blank(),
      plot.title        = element_text(face = "bold", size = base_size + 1),
      plot.subtitle     = element_text(size = base_size - 1, color = "grey40"),
      plot.caption      = element_text(size = base_size - 2, color = "grey50",
                                       hjust = 0)
    )
}

# -----------------------------------------------------------------
# 0. Load data and compute within-field yield responses
# -----------------------------------------------------------------
df     <- read_csv("data/data_y1_rain.csv", show_col_types = FALSE)
prices <- read_csv("outputs/crop_prices.csv", show_col_types = FALSE)

# Within-field differencing: removes field random intercept,
# isolates the observed treatment response per field observation.
df_resp <- df |>
  group_by(admin2_gadm, fid, crop) |>
  mutate(
    yield_ctrl     = mean(yield_tha[lime_tha == 0], na.rm = TRUE),
    yield_resp_obs = yield_tha - yield_ctrl
  ) |>
  ungroup() |>
  filter(!is.na(yield_ctrl), !is.na(yield_resp_obs))

lime_grid <- seq(0, 7, by = 0.1)

# -----------------------------------------------------------------
# 1. Quantile regression per crop (Q20 / Q50 / Q80)
# -----------------------------------------------------------------
results <- list()

for (cr in CROPS) {
  cat("\nFitting Q20/Q50/Q80 for:", cr, "\n")

  dat <- df_resp |>
    filter(crop == cr) |>
    select(yield_resp_obs, lime_tha) |>
    drop_na()

  cat("  n =", nrow(dat), " | treated obs:", sum(dat$lime_tha > 0), "\n")

  fit <- rq(
    yield_resp_obs ~ lime_tha + I(lime_tha^2),
    tau    = TAUS,
    data   = dat,
    method = "fn"
  )

  coefs <- coef(fit)
  cat("  Linear / quadratic lime coefficients per quantile:\n")
  colnames(coefs) <- paste0("Q", TAUS * 100)
  print(round(rbind(coefs["lime_tha", ], coefs["I(lime_tha^2)", ]), 4))

  newdat   <- data.frame(lime_tha = lime_grid)
  pred_mat <- predict(fit, newdata = newdat)
  # Re-centre so every quantile curve passes through zero at lime = 0
  pred0    <- pred_mat[lime_grid == 0, , drop = TRUE]
  resp_mat <- sweep(pred_mat, 2, pred0, "-")

  for (i in seq_along(TAUS)) {
    results[[paste(cr, TAUS[i])]] <- tibble(
      crop       = cr,
      tau        = TAUS[i],
      tau_lab    = paste0("Q", TAUS[i] * 100),
      lime_tha   = lime_grid,
      yield_resp = resp_mat[, i]
    )
  }
}

qr_df <- bind_rows(results) |>
  mutate(
    crop    = factor(crop, levels = CROPS),
    tau_lab = factor(tau_lab, levels = paste0("Q", TAUS * 100))
  )

write_csv(qr_df, "outputs/quantile_response_curves.csv")
cat("\nSaved: quantile_response_curves.csv\n")

# -----------------------------------------------------------------
# 2. Summary table: yield responses at trial treatment levels
# -----------------------------------------------------------------
qr_summary <- qr_df |>
  filter(lime_tha %in% c(1, 2.5, 7)) |>
  select(crop, lime_tha, tau_lab, yield_resp) |>
  mutate(yield_resp = round(yield_resp, 2)) |>
  pivot_wider(names_from = tau_lab, values_from = yield_resp) |>
  arrange(crop, lime_tha)

cat("\n--- Quantile yield responses (t/ha) at treatment levels ---\n")
print(qr_summary)
write_csv(qr_summary, "outputs/quantile_response_summary.csv")

# -----------------------------------------------------------------
# 3. Figure A: Distributional yield response fan
#    Shaded band = Q20–Q80 range (60% of farms)
#    Lines: Q20 (pessimistic), Q50 (typical), Q80 (optimistic)
# -----------------------------------------------------------------
ribbon_df <- qr_df |>
  filter(tau %in% c(0.20, 0.80)) |>
  pivot_wider(id_cols = c(crop, lime_tha),
              names_from = tau_lab, values_from = yield_resp)

p_qr <- ggplot() +
  # Q20–Q80 shaded band (60% of farms lie within)
  geom_ribbon(
    data = ribbon_df,
    aes(x = lime_tha, ymin = Q20, ymax = Q80, group = crop),
    fill = "grey80", alpha = 0.5, color = NA
  ) +
  # Field observations
  geom_jitter(
    data = df_resp |>
      filter(lime_tha > 0) |>
      mutate(crop = factor(crop, levels = CROPS)),
    aes(x = lime_tha, y = yield_resp_obs),
    color = "grey30", alpha = 0.15, size = 0.6, width = 0.12
  ) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey40", linewidth = 0.35) +
  # Quantile curves
  geom_line(
    data = qr_df,
    aes(x = lime_tha, y = yield_resp,
        color = tau_lab, linewidth = tau_lab, linetype = tau_lab)
  ) +
  facet_wrap(~crop, scales = "free_y", ncol = 3) +
  scale_color_manual(
    values = tau_colors,
    labels = c("Q20 — pessimistic (bottom quintile)",
               "Q50 — typical (median)",
               "Q80 — optimistic (top quintile)"),
    name = NULL
  ) +
  scale_linewidth_manual(
    values = c(Q20 = 0.8, Q50 = 1.5, Q80 = 0.8),
    guide  = "none"
  ) +
  scale_linetype_manual(
    values = c(Q20 = "dashed", Q50 = "solid", Q80 = "dashed"),
    guide  = "none"
  ) +
  scale_x_continuous(breaks = c(0, 1, 2.5, 7)) +
  labs(
    x        = "Lime rate (t/ha)",
    y        = "Yield response vs control (t/ha)",
    title    = "Distribution of yield responses to lime",
    subtitle = paste0(
      "Quantile regression on within-field yield responses.\n",
      "Shaded band = 60% of farms (Q20\u2013Q80). Grey dots = observed field responses."
    ),
    caption  = paste0(
      "Q50 \u2248 LMM conditional mean. The spread between Q20 and Q80 ",
      "reflects genuine farm-level heterogeneity in lime responsiveness,\n",
      "not statistical uncertainty in the mean. ",
      "LMM 95% CI is far narrower (e.g. [0.63, 0.78] t/ha for maize T2)."
    )
  ) +
  theme_academic(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 10)
  )

ggsave("outputs/fig_quantile_response.png",
       p_qr, width = 12, height = 7, dpi = 300)
cat("Saved: fig_quantile_response.png\n")

# -----------------------------------------------------------------
# 4. Economic calculations: year-1 profit and 4-year NPV per quantile
# -----------------------------------------------------------------
qr_econ <- qr_df |>
  filter(lime_tha > 0) |>
  left_join(prices |> select(crop, crop_price, lime_price), by = "crop") |>
  mutate(
    profit_yr1 = yield_resp * crop_price - lime_tha * lime_price,
    npv        = yield_resp * crop_price * PV - lime_tha * lime_price
  )

write_csv(
  qr_econ |> select(crop, tau, tau_lab, lime_tha,
                    yield_resp, profit_yr1, npv),
  "outputs/quantile_profit_npv_curves.csv"
)

# -----------------------------------------------------------------
# 5. Figure B: Distributional year-1 profit
# -----------------------------------------------------------------
ribbon_profit <- qr_econ |>
  filter(tau %in% c(0.20, 0.80)) |>
  pivot_wider(id_cols = c(crop, lime_tha),
              names_from = tau_lab, values_from = profit_yr1)

p_profit <- ggplot() +
  geom_ribbon(
    data = ribbon_profit,
    aes(x = lime_tha, ymin = Q20, ymax = Q80, group = crop),
    fill = "grey80", alpha = 0.5, color = NA
  ) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey40", linewidth = 0.35) +
  geom_line(
    data = qr_econ,
    aes(x = lime_tha, y = profit_yr1,
        color = tau_lab, linewidth = tau_lab, linetype = tau_lab)
  ) +
  facet_wrap(~crop, scales = "free_y", ncol = 3) +
  scale_color_manual(
    values = tau_colors,
    labels = c("Q20 — pessimistic", "Q50 — typical", "Q80 — optimistic"),
    name   = NULL
  ) +
  scale_linewidth_manual(values = c(Q20 = 0.8, Q50 = 1.5, Q80 = 0.8), guide = "none") +
  scale_linetype_manual(values = c(Q20 = "dashed", Q50 = "solid", Q80 = "dashed"), guide = "none") +
  scale_x_continuous(breaks = c(1, 2.5, 7)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    x        = "Lime rate (t/ha)",
    y        = "Year-1 profit (USD/ha)",
    title    = "Distribution of year-1 profitability by crop and lime rate",
    subtitle = paste0(
      "Profit = yield response \u00d7 farmgate crop price \u2212 lime rate \u00d7 lime price.\n",
      "Shaded band = Q20\u2013Q80 range of farm outcomes."
    ),
    caption  = paste0(
      "Farmgate prices (USD/t): Maize 243, Beans 451, Soybean 571, Wheat 447, Fababean 595. ",
      "Lime = USD 111/t."
    )
  ) +
  theme_academic(base_size = 12) +
  theme(legend.position = "bottom", legend.text = element_text(size = 10))

ggsave("outputs/fig_quantile_profit.png",
       p_profit, width = 12, height = 7, dpi = 300)
cat("Saved: fig_quantile_profit.png\n")

# -----------------------------------------------------------------
# 6. Figure C: Distributional 4-year NPV
#    This is the key decision-relevant figure: even farms with
#    negative year-1 profit may have positive NPV once multi-year
#    residual benefits are accounted for.
# -----------------------------------------------------------------
ribbon_npv <- qr_econ |>
  filter(tau %in% c(0.20, 0.80)) |>
  pivot_wider(id_cols = c(crop, lime_tha),
              names_from = tau_lab, values_from = npv)

p_npv <- ggplot() +
  geom_ribbon(
    data = ribbon_npv,
    aes(x = lime_tha, ymin = Q20, ymax = Q80, group = crop),
    fill = "grey80", alpha = 0.5, color = NA
  ) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey40", linewidth = 0.35) +
  geom_line(
    data = qr_econ,
    aes(x = lime_tha, y = npv,
        color = tau_lab, linewidth = tau_lab, linetype = tau_lab)
  ) +
  facet_wrap(~crop, scales = "free_y", ncol = 3) +
  scale_color_manual(
    values = tau_colors,
    labels = c("Q20 — pessimistic", "Q50 — typical", "Q80 — optimistic"),
    name   = NULL
  ) +
  scale_linewidth_manual(values = c(Q20 = 0.8, Q50 = 1.5, Q80 = 0.8), guide = "none") +
  scale_linetype_manual(values = c(Q20 = "dashed", Q50 = "solid", Q80 = "dashed"), guide = "none") +
  scale_x_continuous(breaks = c(1, 2.5, 7)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    x        = "Lime rate (t/ha)",
    y        = "4-year NPV (USD/ha)",
    title    = "Distribution of 4-year NPV by crop and lime rate",
    subtitle = sprintf(
      "NPV = yield response \u00d7 farmgate price \u00d7 PV \u2212 lime cost.  PV = %.2f  (r = %.0f%%, decay = %.0f%%/yr, T = %d yrs).\nShaded band = Q20\u2013Q80 range of farm outcomes.",
      PV, NPV_R * 100, NPV_DECAY * 100, NPV_T
    ),
    caption  = paste0(
      "The NPV figure is the correct decision criterion for liming: even farms whose year-1 profit\n",
      "is negative (Q20 for some crops) may have positive NPV once residual lime benefits are included."
    )
  ) +
  theme_academic(base_size = 12) +
  theme(legend.position = "bottom", legend.text = element_text(size = 10))

ggsave("outputs/fig_quantile_npv.png",
       p_npv, width = 12, height = 7, dpi = 300)
cat("Saved: fig_quantile_npv.png\n")

# -----------------------------------------------------------------
# 7. Key numbers for paper text
# -----------------------------------------------------------------
cat("\n====================================================\n")
cat("KEY NUMBERS AT T2 (1 t/ha lime)\n")
cat("====================================================\n")

cat("\n--- Yield response (t/ha) ---\n")
qr_summary |> filter(lime_tha == 1) |> print()

cat("\n--- Year-1 profit (USD/ha) ---\n")
qr_econ |>
  filter(lime_tha == 1) |>
  select(crop, tau_lab, profit_yr1) |>
  mutate(profit_yr1 = round(profit_yr1)) |>
  pivot_wider(names_from = tau_lab, values_from = profit_yr1) |>
  print()

cat("\n--- 4-year NPV (USD/ha) ---\n")
qr_econ |>
  filter(lime_tha == 1) |>
  select(crop, tau_lab, npv) |>
  mutate(npv = round(npv)) |>
  pivot_wider(names_from = tau_lab, values_from = npv) |>
  print()

cat("\n--- At which lime rate does Q20 NPV turn positive? ---\n")
qr_econ |>
  filter(tau == 0.20) |>
  group_by(crop) |>
  summarise(
    first_positive_L = {
      pos <- lime_tha[npv > 0]
      if (length(pos) == 0) NA_real_ else min(pos)
    },
    .groups = "drop"
  ) |>
  print()
