# =============================================================
# 08_additional_analysis.R
# Additional economic analyses:
#
#  A. Benefit-cost ratio (BCR) and Return on Investment (ROI)
#  B. Internal Rate of Return (IRR) per crop × lime rate
#  C. pH-stratified profitability (targeting criterion)
#  D. Stochastic dominance of lime rates (CDF of field profits)
#  E. Lime price ceiling: max lime price that keeps NPV > 0
#
# Outputs:
#   outputs/bcr_roi.csv
#   outputs/irr_by_crop.csv
#   outputs/ph_stratified_profit.csv
#   outputs/fig_bcr_roi.png
#   outputs/fig_irr.png
#   outputs/fig_ph_stratified.png
#   outputs/fig_stochastic_dominance.png
#   outputs/fig_lime_price_ceiling.png
# =============================================================

pacman::p_load(dplyr, readr, tidyr, purrr, ggplot2, extrafont)
source("R/helpers_econ.R")

extrafont::loadfonts(quiet = TRUE)
FONT  <- "Times New Roman"
CROPS <- c("Maize", "Beans", "Soybean", "Wheat", "Fababean")

crop_colors <- c(
  Maize    = "#1A3A5C",
  Beans    = "#B5451B",
  Soybean  = "#2E7D32",
  Wheat    = "#F9A825",
  Fababean = "#6A1B9A"
)

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
# 0. Load inputs
# -----------------------------------------------------------------
curves     <- read_csv("outputs/response_curves.csv",
                       show_col_types = FALSE)
prices     <- read_csv("outputs/crop_prices.csv",
                       show_col_types = FALSE)
profit_yr1 <- read_csv("outputs/profitability_yr1.csv",
                       show_col_types = FALSE)
df         <- read_csv("data/data_y1_rain.csv",
                       show_col_types = FALSE)

NPV_T <- 4; NPV_R <- 0.10; NPV_DECAY <- 0.25
PV    <- pv_factor(NPV_T, NPV_R, NPV_DECAY)

lime_labels <- c("1" = "T2 (1 t/ha)", "2.5" = "T3 (2.5 t/ha)", "7" = "T4 (7 t/ha)")

# =================================================================
# A. Benefit-Cost Ratio and Return on Investment
# =================================================================
npv_curves <- read_csv("outputs/npv_by_crop.csv",
                       show_col_types = FALSE)

bcr_roi <- npv_curves |>
  filter(lime_tha > 0) |>
  mutate(
    total_benefit = yield_resp    * crop_price * PV,
    total_cost    = lime_tha      * lime_price,
    bcr           = total_benefit / total_cost,
    bcr_lo        = (yield_resp_lo * crop_price * PV) / total_cost,
    bcr_hi        = (yield_resp_hi * crop_price * PV) / total_cost,
    roi           = 100 * (total_benefit - total_cost) / total_cost,
    crop          = factor(crop, levels = CROPS)
  )

write_csv(bcr_roi, "outputs/bcr_roi.csv")

# Figure A: BCR by lime rate and crop
p_bcr <- bcr_roi |>
  filter(lime_tha %in% c(1, 2.5, 7)) |>
  mutate(lime_lab = factor(lime_labels[as.character(lime_tha)],
                           levels = lime_labels)) |>
  ggplot(aes(x = lime_lab, y = bcr, color = crop, group = crop, shape = crop)) +
  geom_hline(yintercept = 1, linetype = "dashed",
             color = "grey40", linewidth = 0.5) +
  annotate("text", x = "T2 (1 t/ha)", y = 1.05, label = "BCR = 1 (break-even)",
           hjust = 0.5, size = 3.2, color = "grey40", family = FONT) +
  geom_errorbar(aes(ymin = bcr_lo, ymax = bcr_hi),
                width = 0.15, linewidth = 0.6, alpha = 0.7) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  geom_point(size = 3.5) +
  scale_color_manual(values = crop_colors, name = "Crop") +
  scale_shape_manual(values = c(16, 17, 15, 18, 8), name = "Crop") +
  scale_y_continuous(breaks = seq(0, 10, 1)) +
  labs(
    x       = "Lime rate",
    y       = "Benefit-cost ratio (BCR)",
    title   = "Benefit-cost ratio of liming by crop and lime rate",
    subtitle = sprintf(
      "NPV parameters: r = %.0f%%, decay = %.0f%%/yr, T = %d years",
      NPV_R * 100, NPV_DECAY * 100, NPV_T
    ),
    caption = "BCR > 1 indicates net positive return. Error bars = 95% CI."
  ) +
  theme_academic()

ggsave("outputs/fig_bcr_roi.png",
       p_bcr, width = 8, height = 5, dpi = 300)
cat("Saved: fig_bcr_roi.png\n")

# =================================================================
# B. Internal Rate of Return (IRR)
# =================================================================
# IRR = discount rate r* such that NPV(r*) = 0
# NPV(r) = yield_resp × crop_price × pv_factor(T,r,decay) - L × lime_price
# Solve numerically for each crop × lime rate

compute_irr <- function(yield_resp, crop_price, lime_tha, lime_price,
                        T = 4, decay = 0.25) {
  if (yield_resp <= 0 || lime_tha == 0) return(NA_real_)
  npv_fn <- function(r) {
    yield_resp * crop_price * pv_factor(T, r, decay) - lime_tha * lime_price
  }
  if (npv_fn(0) < 0) return(NA_real_)
  # If still profitable at r=500%, report as >500 (exceptionally high IRR)
  if (npv_fn(5.0) > 0) return(500)
  tryCatch(
    uniroot(npv_fn, c(0.001, 5.0))$root * 100,
    error = function(e) NA_real_
  )
}

irr_df <- curves |>
  filter(lime_tha %in% c(1, 2.5, 7)) |>
  left_join(prices |> select(crop, crop_price, lime_price), by = "crop") |>
  mutate(
    irr     = pmap_dbl(list(yield_resp,    crop_price, lime_tha, lime_price), compute_irr),
    irr_lo  = pmap_dbl(list(yield_resp_lo, crop_price, lime_tha, lime_price), compute_irr),
    irr_hi  = pmap_dbl(list(yield_resp_hi, crop_price, lime_tha, lime_price), compute_irr),
    lime_lab = factor(lime_labels[as.character(lime_tha)], levels = lime_labels),
    crop     = factor(crop, levels = CROPS)
  )

write_csv(irr_df, "outputs/irr_by_crop.csv")

p_irr <- irr_df |>
  ggplot(aes(x = lime_lab, y = irr, color = crop, group = crop, shape = crop)) +
  geom_hline(yintercept = 10, linetype = "dashed",
             color = "grey40", linewidth = 0.5) +
  annotate("text", x = "T2 (1 t/ha)", y = 12, label = "Hurdle rate (10%)",
           hjust = 0.5, size = 3.2, color = "grey40", family = FONT) +
  geom_errorbar(aes(ymin = irr_lo, ymax = irr_hi),
                width = 0.15, linewidth = 0.6, alpha = 0.7,
                na.rm = TRUE) +
  geom_line(linewidth = 0.8, alpha = 0.8, na.rm = TRUE) +
  geom_point(size = 3.5, na.rm = TRUE) +
  scale_color_manual(values = crop_colors, name = "Crop") +
  scale_shape_manual(values = c(16, 17, 15, 18, 8), name = "Crop") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    x       = "Lime rate",
    y       = "Internal rate of return (%)",
    title   = "Internal rate of return by crop and lime rate",
    caption = paste0(
      "IRR = discount rate at which NPV = 0 (benefit decay = 25%/yr, T = 4 yrs).\n",
      "NA values indicate the investment does not recoup its cost under any discount rate.\n",
      "Dashed line = assumed opportunity cost of capital (10%)."
    )
  ) +
  theme_academic()

ggsave("outputs/fig_irr.png",
       p_irr, width = 8, height = 5, dpi = 300)
cat("Saved: fig_irr.png\n")

# =================================================================
# C. pH-Stratified Profitability (targeting criterion)
# =================================================================
ph_profit <- profit_yr1 |>
  filter(!is.na(p_h_BP), lime_tha %in% c(1, 2.5, 7)) |>
  group_by(crop) |>
  mutate(
    ph_quartile = cut(p_h_BP,
                      breaks   = quantile(p_h_BP, probs = c(0, .25, .5, .75, 1),
                                          na.rm = TRUE),
                      labels   = c("Q1\n(most acid)", "Q2", "Q3", "Q4\n(least acid)"),
                      include.lowest = TRUE)
  ) |>
  ungroup() |>
  filter(!is.na(ph_quartile)) |>
  group_by(crop, lime_tha, ph_quartile) |>
  summarise(
    n              = n(),
    mean_profit    = mean(profit_yr1, na.rm = TRUE),
    se_profit      = sd(profit_yr1, na.rm = TRUE) / sqrt(n()),
    pct_profitable = mean(profit_yr1 > 0, na.rm = TRUE) * 100,
    mean_ph        = mean(p_h_BP, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    lime_lab = factor(lime_labels[as.character(lime_tha)], levels = lime_labels),
    crop     = factor(crop, levels = CROPS)
  )

write_csv(ph_profit, "outputs/ph_stratified_profit.csv")

p_ph <- ph_profit |>
  ggplot(aes(x = ph_quartile, y = mean_profit,
             fill = lime_lab, group = lime_lab)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65,
           alpha = 0.9, color = "white", linewidth = 0.2) +
  geom_errorbar(
    aes(ymin = mean_profit - 1.96 * se_profit,
        ymax = mean_profit + 1.96 * se_profit),
    position = position_dodge(width = 0.75), width = 0.25,
    linewidth = 0.5
  ) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey30") +
  facet_wrap(~crop, scales = "free_y", ncol = 3) +
  scale_fill_manual(
    values = c("T2 (1 t/ha)"   = "#1A3A5C",
               "T3 (2.5 t/ha)" = "#5B8DB8",
               "T4 (7 t/ha)"   = "#C8D8E8"),
    name = "Lime rate"
  ) +
  labs(
    x       = "Soil pH quartile (baseline)",
    y       = "Mean year-1 profit (USD/ha)",
    title   = "Year-1 profitability by soil pH quartile and lime rate",
    caption = paste0(
      "Soil pH quartiles computed within each crop's data. ",
      "Error bars = 95% CI. Q1 = lowest pH (most acidic)."
    )
  ) +
  theme_academic() +
  theme(legend.position = "bottom")

ggsave("outputs/fig_ph_stratified.png",
       p_ph, width = 12, height = 7, dpi = 300)
cat("Saved: fig_ph_stratified.png\n")

# =================================================================
# D. Stochastic Dominance: CDF of field profits by lime rate
# =================================================================
cdf_data <- profit_yr1 |>
  filter(lime_tha %in% c(1, 2.5, 7)) |>
  mutate(
    lime_lab = factor(lime_labels[as.character(lime_tha)], levels = lime_labels),
    crop     = factor(crop, levels = CROPS)
  ) |>
  group_by(crop, lime_lab) |>
  arrange(profit_yr1) |>
  mutate(cdf = seq_along(profit_yr1) / n()) |>
  ungroup()

p_cdf <- cdf_data |>
  ggplot(aes(x = profit_yr1, y = cdf, color = lime_lab, linetype = lime_lab)) +
  geom_vline(xintercept = 0, linetype = "dotted",
             color = "grey40", linewidth = 0.4) +
  geom_line(linewidth = 0.85) +
  facet_wrap(~crop, scales = "free_x", ncol = 3) +
  scale_color_manual(
    values   = c("T2 (1 t/ha)" = "#1A3A5C",
                 "T3 (2.5 t/ha)" = "#5B8DB8",
                 "T4 (7 t/ha)"   = "#B5451B"),
    name = "Lime rate"
  ) +
  scale_linetype_manual(
    values = c("T2 (1 t/ha)" = "solid",
               "T3 (2.5 t/ha)" = "dashed",
               "T4 (7 t/ha)"   = "dotdash"),
    name = "Lime rate"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x       = "Year-1 profit (USD/ha)",
    y       = "Cumulative probability",
    title   = "Empirical CDF of year-1 profit by lime rate",
    caption = paste0(
      "First-order stochastic dominance: curve A dominates B if A lies entirely to ",
      "the right of B.\nVertical dotted line at zero profit."
    )
  ) +
  theme_academic() +
  theme(legend.position = "bottom")

ggsave("outputs/fig_stochastic_dominance.png",
       p_cdf, width = 12, height = 7, dpi = 300)
cat("Saved: fig_stochastic_dominance.png\n")

# =================================================================
# E. Lime price ceiling: max lime price that keeps NPV > 0
# =================================================================
# Solve: yield_resp × crop_price × PV - L × lime_price_max = 0
# => lime_price_max = yield_resp × crop_price × PV / L

lime_ceiling <- curves |>
  filter(lime_tha > 0) |>
  left_join(prices |> select(crop, crop_price, lime_price), by = "crop") |>
  mutate(
    lime_price_max    = (yield_resp    * crop_price * PV) / lime_tha,
    lime_price_max_lo = (yield_resp_lo * crop_price * PV) / lime_tha,
    lime_price_max_hi = (yield_resp_hi * crop_price * PV) / lime_tha,
    pct_above_current = 100 * (lime_price_max - lime_price) / lime_price,
    crop = factor(crop, levels = CROPS)
  )

write_csv(lime_ceiling, "outputs/lime_price_ceiling.csv")

# Highlight actual treatment levels
highlight_rates <- lime_ceiling |>
  filter(lime_tha %in% c(1, 2.5, 7)) |>
  mutate(lime_lab = lime_labels[as.character(lime_tha)])

p_ceil <- lime_ceiling |>
  ggplot(aes(x = lime_tha, y = lime_price_max, color = crop, fill = crop)) +
  geom_ribbon(aes(ymin = lime_price_max_lo, ymax = lime_price_max_hi),
              alpha = 0.12, color = NA) +
  geom_line(linewidth = 1.0) +
  # current lime price as reference
  geom_hline(
    data = prices |> mutate(crop = factor(crop, levels = CROPS)),
    aes(yintercept = lime_price, color = crop),
    linetype = "dashed", linewidth = 0.5
  ) +
  geom_point(data = highlight_rates,
             aes(x = lime_tha, y = lime_price_max),
             size = 3, shape = 21, fill = "white", stroke = 1.2) +
  facet_wrap(~crop, scales = "free_y", ncol = 3) +
  scale_x_continuous(breaks = c(1, 2.5, 4, 7)) +
  scale_color_manual(values = crop_colors) +
  scale_fill_manual(values  = crop_colors) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    x       = "Lime rate (t/ha)",
    y       = "Maximum lime price (USD/t)",
    title   = "Lime price ceiling: maximum price at which liming remains profitable",
    subtitle = sprintf(
      "NPV framework: r = %.0f%%, decay = %.0f%%/yr, T = %d yrs | Dashed = current lime price",
      NPV_R * 100, NPV_DECAY * 100, NPV_T
    ),
    caption = paste0(
      "Ceiling = yield_resp × crop_price × PV_factor / lime_rate. ",
      "Band = 95% CI. Open circles = observed treatment levels."
    )
  ) +
  theme_academic() +
  theme(legend.position = "none")

ggsave("outputs/fig_lime_price_ceiling.png",
       p_ceil, width = 12, height = 7, dpi = 300)
cat("Saved: fig_lime_price_ceiling.png\n")

# -----------------------------------------------------------------
# Print BCR and IRR summary tables
# -----------------------------------------------------------------
cat("\n--- BCR at treatment levels ---\n")
print(
  bcr_roi |>
    filter(lime_tha %in% c(1, 2.5, 7)) |>
    mutate(lime_lab = lime_labels[as.character(lime_tha)]) |>
    select(crop, lime_lab, bcr, bcr_lo, bcr_hi) |>
    mutate(across(where(is.numeric), \(x) round(x, 2)))
)

cat("\n--- IRR at treatment levels ---\n")
print(
  irr_df |>
    select(crop, lime_lab, irr, irr_lo, irr_hi) |>
    mutate(across(where(is.numeric), \(x) round(x, 1)))
)
