# =============================================================
# 04_profitability.R
# Year-1 profitability and NPV per crop
#
# Prices: base_crop_prices.csv (weighted mean per crop)
#         base_lime_price.csv  (weighted mean per crop)
# NPV:    pv_factor(T=4, r=0.10, decay=0.25) from R/helpers_econ.R
#
# Outputs:
#   outputs/crop_prices.csv
#   outputs/profitability_yr1.csv   (field-level)
#   outputs/npv_by_crop.csv         (model-predicted curves)
#   outputs/optimal_lime_rate.csv
#   outputs/fig_profit_yr1_field.png
#   outputs/fig_npv_curves.png
# =============================================================

pacman::p_load(dplyr, readr, tidyr, purrr, ggplot2, ggthemes, extrafont)
source("R/helpers_econ.R")

extrafont::loadfonts(quiet = TRUE)
my_font <- "Muli"

bar_colors <- c("#0E3065", "#FFBE00", "#FC3400", "#00640D", "#8F2D56")

# -----------------------------------------------------------------
# 0. Load inputs
# -----------------------------------------------------------------
df <- read_csv("data/data_y1_rain.csv", show_col_types = FALSE)

curves <- read_csv("outputs/response_curves.csv",
                   show_col_types = FALSE)

base_crop_prices <- read_csv("data/prices/base_crop_prices.csv", show_col_types = FALSE)
base_lime_price  <- read_csv("data/prices/base_lime_price.csv",  show_col_types = FALSE)

# NPV parameters
NPV_T     <- 4      # years
NPV_R     <- 0.10   # discount rate
NPV_DECAY <- 0.25   # annual benefit decay
PV        <- pv_factor(NPV_T, NPV_R, NPV_DECAY)
cat(sprintf("pv_factor(T=%d, r=%.0f%%, decay=%.0f%%) = %.4f\n",
            NPV_T, NPV_R * 100, NPV_DECAY * 100, PV))

# -----------------------------------------------------------------
# 1. Aggregate prices to crop level (sample-size-weighted mean)
# -----------------------------------------------------------------

# Field counts per site × crop (from raw data at lime=0 = one row per field)
field_counts <- df |>
  filter(lime_tha == 0) |>
  count(country, admin2_gadm, crop, name = "n_fields")

crop_prices <- base_crop_prices |>
  left_join(field_counts, by = c("country", "admin2_gadm", "crop")) |>
  filter(!is.na(n_fields)) |>
  group_by(crop) |>
  summarise(
    crop_price    = weighted.mean(crop_price_base, w = n_fields),
    n_fields_total = sum(n_fields),
    .groups = "drop"
  )

lime_prices <- base_lime_price |>
  left_join(
    field_counts |> group_by(country) |> summarise(n = sum(n_fields)),
    by = "country"
  ) |>
  # one lime price per crop: weight by number of fields in each country
  # (lime price doesn't vary by crop, so this gives a crop-independent value)
  summarise(lime_price = weighted.mean(lime_price_base, w = n)) |>
  # repeat for each crop
  cross_join(crop_prices |> select(crop)) |>
  select(crop, lime_price)

# -----------------------------------------------------------------
# Farmgate price adjustment
# WFP/FAO market prices are retail/wholesale prices. Smallholder
# farmgate prices are systematically lower due to marketing margins
# (transport, storage, trader margins).
# Farmgate-to-market ratios from the East Africa literature:
#   Maize:    0.65 (35% margin) — Jayne et al. (2010); World Bank (2012)
#   Beans:    0.70 (30% margin) — Howard et al. (2003); CIAT bean studies
#   Soybean:  0.72 (28% margin) — USAID (2010) Ethiopia soybean value chain
#   Wheat:    0.68 (32% margin) — Minten et al. (2020) Ethiopia grain markets
#   Fababean: 0.75 (25% margin) — better farmgate price due to export demand
#     (Legesse et al., 2012; FAO Ethiopia pulse market)
# -----------------------------------------------------------------
farmgate_factors <- tibble(
  crop     = c("Maize", "Beans", "Soybean", "Wheat", "Fababean"),
  fg_ratio = c(0.65,    0.70,    0.72,      0.68,    0.75)
)

# Join into one price table per crop
price_tbl <- crop_prices |>
  left_join(lime_prices, by = "crop") |>
  left_join(farmgate_factors, by = "crop") |>
  mutate(
    crop_price_retail = crop_price,
    crop_price        = crop_price * fg_ratio,          # farmgate price
    price_ratio_obs   = crop_price / lime_price
  ) |>
  select(-fg_ratio)

write_csv(price_tbl, "outputs/crop_prices.csv")
cat("\n--- Crop-level prices (USD/t): retail → farmgate ---\n")
print(price_tbl |> select(crop, crop_price_retail, crop_price, lime_price, price_ratio_obs))

# -----------------------------------------------------------------
# 2. Field-level year-1 profitability (observed yield responses)
# -----------------------------------------------------------------
# Yield response at field level: yield - control mean within field
df_resp <- df |>
  group_by(country, admin2_gadm, fid, crop) |>
  mutate(
    yield_T1      = mean(yield_tha[lime_tha == 0], na.rm = TRUE),
    yield_resp_obs = yield_tha - yield_T1
  ) |>
  ungroup() |>
  filter(lime_tha > 0, !is.na(yield_resp_obs))

# Join prices and compute field profit
profit_yr1 <- df_resp |>
  left_join(price_tbl |> select(crop, crop_price, lime_price), by = "crop") |>
  mutate(
    profit_yr1 = yield_resp_obs * crop_price - lime_tha * lime_price,
    profitable = profit_yr1 > 0
  )

write_csv(profit_yr1, "outputs/profitability_yr1.csv")

# Share profitable by crop × lime rate
prof_share <- profit_yr1 |>
  group_by(crop, lime_tha) |>
  summarise(
    n               = n(),
    pct_profitable  = mean(profitable, na.rm = TRUE) * 100,
    mean_profit     = mean(profit_yr1, na.rm = TRUE),
    .groups = "drop"
  )
cat("\n--- % fields profitable by crop × lime rate (year 1) ---\n")
print(prof_share)

# -----------------------------------------------------------------
# 3. Model-predicted profitability + NPV curves
# -----------------------------------------------------------------
npv_curves <- curves |>
  left_join(price_tbl |> select(crop, crop_price, lime_price), by = "crop") |>
  mutate(
    profit_yr1     = yield_resp     * crop_price - lime_tha * lime_price,
    profit_yr1_lo  = yield_resp_lo  * crop_price - lime_tha * lime_price,
    profit_yr1_hi  = yield_resp_hi  * crop_price - lime_tha * lime_price,
    npv            = yield_resp     * crop_price * PV - lime_tha * lime_price,
    npv_lo         = yield_resp_lo  * crop_price * PV - lime_tha * lime_price,
    npv_hi         = yield_resp_hi  * crop_price * PV - lime_tha * lime_price
  )

# Optimal lime rate per crop (argmax NPV, excluding lime=0)
optimal <- npv_curves |>
  filter(lime_tha > 0) |>
  group_by(crop) |>
  slice_max(npv, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(crop, opt_lime_tha = lime_tha, npv_opt = npv,
         npv_opt_lo = npv_lo, npv_opt_hi = npv_hi,
         profit_yr1_opt = profit_yr1)

write_csv(npv_curves, "outputs/npv_by_crop.csv")
write_csv(optimal,    "outputs/optimal_lime_rate.csv")

cat("\n--- Optimal lime rates (argmax NPV) ---\n")
print(optimal)

# -----------------------------------------------------------------
# 4. Figure: field-level year-1 profit boxplot
# -----------------------------------------------------------------
lime_labels <- c("1" = "T2\n(1 t/ha)", "2.5" = "T3\n(2.5 t/ha)", "7" = "T4\n(7 t/ha)")

p_field <- profit_yr1 |>
  mutate(lime_lab = factor(lime_labels[as.character(lime_tha)],
                           levels = lime_labels)) |>
  ggplot(aes(x = lime_lab, y = profit_yr1, fill = crop)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_boxplot(
    alpha = 0.85, width = 0.55, staplewidth = 0.3,
    outliers = FALSE, coef = 1.5, linewidth = 0.25
  ) +
  facet_wrap(~crop, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = bar_colors) +
  labs(
    x     = "",
    y     = "Year-1 profit (USD/ha)",
    title = "Field-level year-1 profitability by crop and lime rate"
  ) +
  ggthemes::theme_clean(base_size = 13, base_family = my_font) +
  theme(
    legend.position   = "none",
    strip.background  = element_rect(fill = "#d0e2f0", color = "black", linewidth = 0.4),
    strip.text        = element_text(color = "#0E3065", size = 11),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.2)
  )

ggsave("outputs/fig_profit_yr1_field.png",
       p_field, width = 11, height = 6, dpi = 300)

# -----------------------------------------------------------------
# 5. Figure: model-predicted NPV curves per crop
# -----------------------------------------------------------------
# Mark optimal lime rate per crop
opt_labels <- optimal |>
  mutate(
    label = sprintf("Opt: %.1f t/ha\nNPV: $%.0f", opt_lime_tha, npv_opt)
  )

p_npv <- npv_curves |>
  ggplot(aes(x = lime_tha)) +
  # NPV CI ribbon
  geom_ribbon(aes(ymin = npv_lo, ymax = npv_hi),
              fill = "#00640D", alpha = 0.15) +
  # Year-1 profit CI ribbon
  geom_ribbon(aes(ymin = profit_yr1_lo, ymax = profit_yr1_hi),
              fill = "#0E3065", alpha = 0.12) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  # NPV curve
  geom_line(aes(y = npv, color = "NPV (4-yr)"), linewidth = 1.1) +
  # Year-1 profit curve
  geom_line(aes(y = profit_yr1, color = "Year-1 profit"), linewidth = 1.1,
            linetype = "dashed") +
  # Optimal lime rate vertical line
  geom_vline(
    data = optimal,
    aes(xintercept = opt_lime_tha),
    color = "grey40", linetype = "dotted", linewidth = 0.6
  ) +
  # Optimal rate label
  geom_text(
    data = opt_labels,
    aes(x = opt_lime_tha, y = npv_opt_hi, label = label),
    hjust = -0.1, vjust = 1, size = 3, color = "grey30",
    family = my_font
  ) +
  facet_wrap(~crop, scales = "free_y", ncol = 3) +
  scale_x_continuous(breaks = c(0, 1, 2.5, 7)) +
  scale_color_manual(
    values = c("NPV (4-yr)" = "#00640D", "Year-1 profit" = "#0E3065")
  ) +
  labs(
    x       = "Lime rate (t/ha)",
    y       = "Profit (USD/ha)",
    color   = "",
    title   = "Model-predicted profitability and NPV per crop",
    subtitle = sprintf(
      "NPV parameters: discount rate = %.0f%%, benefit decay = %.0f%%/yr, horizon = %d yrs | Bands = 95%% CI",
      NPV_R * 100, NPV_DECAY * 100, NPV_T
    )
  ) +
  ggthemes::theme_clean(base_size = 13, base_family = my_font) +
  theme(
    legend.position   = "bottom",
    strip.background  = element_rect(fill = "#d0e2f0", color = "black", linewidth = 0.4),
    strip.text        = element_text(color = "#0E3065", size = 11),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.2),
    plot.subtitle     = element_text(size = 9, color = "grey40")
  )

ggsave("outputs/fig_npv_curves.png",
       p_npv, width = 12, height = 7, dpi = 300)

cat("\nSaved: outputs/fig_profit_yr1_field.png\n")
cat("Saved: outputs/fig_npv_curves.png\n")
