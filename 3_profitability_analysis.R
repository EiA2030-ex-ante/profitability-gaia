# =========================================================
# Liming Profitability in Tanzania, Rwanda, and Ethiopia
# =========================================================
# Author: Bisrat H
# date: 28/09/2025
# What it does:
# - Reads raw trial data + baseline price tables (mainly assumptions, will update with WFP prices)
# - Computes per-field yield response (vs T1 control) per site × crop
# - Additional revenue, total lime cost, profit gain at field
# - NPV with 25%/yr benefit decay over 4 years and 10% discount rate
# - Sensitivity of profit to crop & lime prices
# Outputs written to "outputs/" as CSVs and PNGs
# =========================================================


# ─────────────────────────────────────────────────────────────
# 0. Load Packages
# ─────────────────────────────────────────────────────────────
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  dplyr,
  readr,
  tidyr,
  purrr,
  stringr,
  fs,
  scales
)


extrafont::loadfonts(q = T) # for Windows

my_font <- "Frutiger"
my_font_2 <- "Muli"

bar_colors <- c(
  "#0E3065",
  "#FFBE00",
  "#FC3400",
  "#00640D",
  "#8F2D56",
  "#490000",
  "#4E5E77",
  
  "#454283"
)


theme_bisrat <- function() {
  ggthemes::theme_pander(base_size = 12, base_family = my_font_2) +
    theme(
      strip.background = element_rect(fill = "#d0e2f0", color = "black", linewidth = 0.5),
      strip.text.x     = element_text(color = "#0E3065", family = my_font_2, size = 10),
      panel.grid       = element_line(color = "grey90", linewidth = 0.2),
      panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5),
      plot.title       = element_text(face = "plain")
    )
}

# ─────────────────────────────────────────────────────────────
# 1. Paths and Parameters
# ─────────────────────────────────────────────────────────────
raw_data_path <- "tmp/data_y1.dta" # raw, observational data
base_crop_price_path <- "tmp/base_crop_prices.csv" # cols: country,admin2_gadm,crop,crop_price_base
base_lime_price_path <- "tmp/base_lime_price.csv" # cols: country,lime_price_base
output_dir <- "outputs/profitability_analysis" # output directory

dir_create(output_dir)

default_crop_price <- 160 # USD/t if baseline file row missing
default_lime_price <- 55 # USD/t if baseline file row missing

discount_rate <- 0.10 # 10% annual discount (editable)
benefit_decay <- 0.25 # 25%/year decay in yield benefit
time_horizon_years <- 4 # 4 years

# Sensitivity grid (multipliers on baseline prices)
crop_multipliers <- seq(0.8, 1.8, by = 0.1) # 60%..180% of baseline
lime_multipliers <- seq(0.8, 1.8, by = 0.1)

# ─────────────────────────────────────────────────────────────
# 2. Helper Functions
# ─────────────────────────────────────────────────────────────
safe_read_csv <- function(path) {
  if (file.exists(path)) {
    readr::read_csv(path, show_col_types = FALSE)
  } else {
    NULL
  }
}

pv_factor <- function(T, r, decay = 0) {
  # decay: proportion (0..1). If 0.25, benefits shrink to 75% each year
  if (T <= 0) {
    return(0)
  }
  t <- 1:T
  sum(((1 - decay)^(t - 1)) / ((1 + r)^t))
}

# Compute field-level yield response vs control (T1) within each country/site/field/crop

#   yield_response = yield_tha - mean(yield_tha of T1 in that field)
compute_yield_response <- function(df_raw) {
  df_raw |>
    group_by(country, admin2_gadm, crop, fid) |>
    mutate(
      yield_T1 = mean(yield_tha[treatment == "T1"], na.rm = TRUE),
      yield_response = yield_tha - yield_T1
    ) |>
    ungroup()
}

# ─────────────────────────────────────────────────────────────
# 3. Load and Prepare Data
# ─────────────────────────────────────────────────────────────
df_raw <- haven::as_factor(haven::read_dta(raw_data_path), only_labelled = T) |>
  select(
    country,
    admin2_gadm,
    crop,
    fid,
    treatment,
    lime_tha,
    yield_tha,
    everything()
  ) |>
  filter(
    !is.na(country),
    !is.na(admin2_gadm),
    !is.na(crop),
    !is.na(fid),
    !is.na(yield_tha)
  ) |>
  mutate(
    country     = as.character(country),
    admin2_gadm = as.character(admin2_gadm),
    crop        = as.character(crop),
    fid         = as.character(fid)
  ) |>
  sjlabelled::var_labels(
    country     = "Country",
    admin2_gadm = "Admin2 GADM",
    crop        = "Crop",
    fid         = "Field ID",
    treatment   = "Lime Treatment",
    lime_tha    = "Lime Application Rate (t/ha)",
    yield_tha   = "Yield (t/ha)"
  )


base_crop_prices <- safe_read_csv(base_crop_price_path)
base_lime_price <- safe_read_csv(base_lime_price_path)


# ─────────────────────────────────────────────────────────────
# 4. Profit and NPV Computation
# ─────────────────────────────────────────────────────────────
df_resp <- compute_yield_response(df_raw)
df_resp_nz <- df_resp |> filter(treatment != "T1")

# Attach baseline crop price (by country, site, crop) and lime price (by country)
df_resp_pr <- df_resp_nz |>
  left_join(base_crop_prices, by = c("country", "admin2_gadm", "crop")) |>
  left_join(base_lime_price, by = "country") |>
  mutate(
    crop_price_base = ifelse(is.na(crop_price_base), default_crop_price, crop_price_base),
    lime_price_base = ifelse(is.na(lime_price_base), default_lime_price, lime_price_base)
  )

disc_factor <- pv_factor(time_horizon_years, discount_rate, benefit_decay)

df_field <- df_resp_pr |>
  mutate(
    # Additional revenue in year 1 (USD/ha): yield_response (t/ha) × crop price (USD/t)
    addl_revenue_y1 = yield_response * crop_price_base,

    # Total lime cost (USD/ha).
    lime_cost = lime_tha * lime_price_base,

    # One-year margin (simple profit in year 1)
    profit_y1 = addl_revenue_y1 - lime_cost,

    # NPV of benefit stream over T years with decay, minus one-time lime cost.
    # Benefit stream is addl_revenue_y1 discounted & decayed by pv_factor.
    npv = (addl_revenue_y1 * disc_factor) - lime_cost
  ) |>
  mutate(
    treatment = factor(treatment,
      levels = c("T2", "T3", "T4"),
      labels = c("1 t/ha", "2.5 t/ha", "7.5 t/ha")
    )
  )

# ----------------------------------------------
# 4.1. Boxplot of Profit by Site (facet by Crop)
# ----------------------------------------------
df_field |>
  mutate(facet_label = paste0(admin2_gadm, " | ", crop)) |>
  ggplot(aes(x = treatment, y = profit_y1, fill = treatment)) +
  geom_boxplot(
    alpha = 0.9,
    width = 0.5,
    staplewidth = 0.2,
    outliers = FALSE,
    coef = 1.5,
    color = "#0E3065",
    linewidth = 0.2
  ) +
  facet_wrap(~facet_label, scales = "free_y") +
  scale_y_continuous(labels = dollar_format()) +
  scale_fill_manual(values = bar_colors) +
  labs(
    title = "Field-level Profit by Site and Crop",
    x = "Site",
    y = "Profit (USD/ha)"
  ) +
  ggthemes::theme_pander(base_size = 14, base_family = my_font_2) +
  theme(
    strip.background = element_rect(fill = "#d0e2f0", color = "black", linewidth = 0.5),
    strip.text.x = element_text(color = "#0E3065", face = "plain", family = my_font_2, size = 12),
    panel.grid = element_line(color = "grey90", linewidth = 0.2),
    legend.position = "none",
    plot.title = element_text(face = "plain"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

ggsave(file.path(output_dir, "box_profit_by_site_crop.png"), width = 14, height = 14, dpi = 400)


# ----------------------------------------------
# 4.2. Boxplot of NPV by Site (facet by Crop)
# ----------------------------------------------
df_field |>
  mutate(facet_label = paste0(admin2_gadm, " | ", crop)) |>
  ggplot(aes(x = treatment, y = npv, fill = treatment)) +
  geom_boxplot(
    alpha = 0.9,
    width = 0.5,
    staplewidth = 0.2,
    outliers = FALSE,
    coef = 1.5,
    color = "#0E3065",
    linewidth = 0.2
  ) +
  facet_wrap(~facet_label, scales = "free_y") +
  scale_y_continuous(labels = dollar_format()) +
  scale_fill_manual(values = bar_colors) +
  labs(
    title = "Field-level NPV by Site and Crop",
    subtitle = paste0("Discount rate: ", discount_rate * 100, "%; Time horizon: ", time_horizon_years, " years; Benefit decay: ", benefit_decay * 100, "%/year"),
    x = "Site",
    y = "NPV (USD/ha)"
  ) +
  ggthemes::theme_pander(base_size = 14, base_family = my_font_2) +
  theme(
    strip.background = element_rect(fill = "#d0e2f0", color = "black", linewidth = 0.5),
    strip.text.x = element_text(color = "#0E3065", face = "plain", family = my_font_2, size = 12),
    panel.grid = element_line(color = "grey90", linewidth = 0.2),
    legend.position = "none",
    plot.title = element_text(face = "plain"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )


ggsave(file.path(output_dir, "box_npv_by_site_crop.png"), width = 12, height = 16, dpi = 600)


# -----------------------------
# 5. Sensitivity analysis (profit & NPV) vs price multipliers
# -----------------------------
# For each site×crop×field row, recompute profit_y1 & NPV across a grid of
# (crop_price_base * crop_mult, lime_price_base * lime_mult), then average.

grid <- expand.grid(crop_mult = crop_multipliers, lime_mult = lime_multipliers)

sensitivity_df <- df_field |>
  select(
    country,
    admin2_gadm,
    treatment,
    crop,
    fid,
    lime_tha,
    yield_response,
    crop_price_base,
    lime_price_base
  ) |>
  mutate(row_id = dplyr::row_number()) |>
  # cross with grid
  tidyr::crossing(grid) |>
  mutate(
    crop_price = crop_price_base * crop_mult,
    lime_price = lime_price_base * lime_mult,
    addl_revenue_y1 = yield_response * crop_price,
    lime_cost = lime_tha * lime_price,
    profit_y1 = addl_revenue_y1 - lime_cost,
    npv = (addl_revenue_y1 * disc_factor) - lime_cost
  )

# Summaries of sensitivity surfaces
sens_site_crop <- sensitivity_df |>
  group_by(country, admin2_gadm, crop, crop_mult, lime_mult) |>
  summarise(
    mean_profit_y1 = mean(profit_y1, na.rm = TRUE),
    mean_npv = mean(npv, na.rm = TRUE),
    .groups = "drop"
  )

sens_overall <- sensitivity_df |>
  group_by(crop_mult, lime_mult) |>
  summarise(
    mean_profit_y1 = mean(profit_y1, na.rm = TRUE),
    mean_npv = mean(npv, na.rm = TRUE),
    .groups = "drop"
  )


# ----------------------------------------------
# 6.1. Sensitivity: % of fields with positive profit
# ----------------------------------------------

#  Summarize probability of profit > 0 by crop
sens_summary_crop <- sensitivity_df |>
  group_by(crop, admin2_gadm, treatment, crop_mult, lime_mult) |>
  summarise(
    pct_positive = mean(profit_y1 > 0, na.rm = TRUE) * 100,
    pct_positive_npv = mean(npv > 0, na.rm = TRUE) * 100,
    .groups = "drop"
  ) |>
  mutate(facet_label = paste0(admin2_gadm, " | ", crop))

#  Heatmap faceted by crop (profit)
ggplot(sens_summary_crop, aes(x = crop_mult, y = lime_mult, fill = pct_positive)) +
  geom_tile(color = "grey90") +
  # geom_text(aes(label = sprintf("%.0f%%", pct_positive)), color = "white", size = 3) +
  facet_wrap(~facet_label, ncol = 4) +
  scale_fill_viridis_c(name = "% fields profitable", option = "C", limits = c(0, 100)) +
  labs(
    title = "Sensitivity of Profitability to Crop–Lime Price Ratios (Year 1)",
    subtitle = "Each panel shows % of fields with positive profit under each price scenario",
    x = "Crop price multiplier",
    y = "Lime price multiplier"
  ) +
  ggthemes::theme_pander(base_size = 14, base_family = my_font_2) +
  theme(
    strip.background = element_rect(fill = "#d0e2f0", color = "black", linewidth = 0.5),
    strip.text.x = element_text(color = "#0E3065", face = "plain", family = my_font_2, size = 12),
    panel.grid = element_line(color = "grey90", linewidth = 0.2),
    legend.position = c(0.8, 0.15),
    plot.title = element_text(face = "plain"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )


ggsave(
  filename = file.path(output_dir, "sensitivity_profitability_heatmap_by_crop_site.png"),
  width = 10,
  height = 14,
  dpi = 600
)

# heatmap faceted by site (NPV)
ggplot(sens_summary_crop, aes(x = crop_mult, y = lime_mult, fill = pct_positive_npv)) +
  geom_tile(color = "grey90") +
  # geom_text(aes(label = sprintf("%.0f%%", pct_positive_npv)), color = "white", size = 3) +
  facet_wrap(~facet_label, ncol = 4) +
  scale_fill_viridis_c(name = "% fields NPV>0", option = "C", limits = c(0, 100)) +
  labs(
    title = "Sensitivity of NPV Profitability to Crop–Lime Price Ratios",
    subtitle = "Each panel shows % of fields with positive NPV under each price scenario",
    x = "Crop price multiplier",
    y = "Lime price multiplier"
  ) +
  ggthemes::theme_pander(base_size = 14, base_family = my_font_2) +
  theme(
    strip.background = element_rect(fill = "#d0e2f0", color = "black", linewidth = 0.5),
    strip.text.x = element_text(color = "#0E3065", face = "plain", family = my_font_2, size = 12),
    panel.grid = element_line(color = "grey90", linewidth = 0.2),
    legend.position = c(0.8, 0.15),
    plot.title = element_text(face = "plain"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )


ggsave(
  filename = file.path(output_dir, "sensitivity_npv_profitability_heatmap_by_crop_site.png"),
  width = 10,
  height = 14,
  dpi = 600
)


# only maize for simplicity
sensitivity_df_maize <- sens_summary_crop |>
  filter(tolower(crop) == "maize") |>
  mutate(facet_label_2 = paste0(admin2_gadm, " | ", treatment))

ggplot(sensitivity_df_maize, aes(x = crop_mult, y = lime_mult, fill = pct_positive_npv)) +
  geom_tile(color = "grey90") +
  # geom_text(aes(label = sprintf("%.0f%%", pct_positive_npv)), color = "white", size = 3) +
  facet_wrap(~facet_label_2, ncol = 3) +
  scale_fill_viridis_c(name = "% fields NPV>0", option = "C", limits = c(0, 100)) +
  labs(
    title = "Sensitivity of NPV Profitability to Crop–Lime Price Ratios (Maize)",
    subtitle = "Each panel shows % of fields with positive NPV under each price scenario",
    x = "Crop price multiplier",
    y = "Lime price multiplier"
  ) +
  ggthemes::theme_pander(base_size = 14, base_family = my_font_2) +
  theme(
    strip.background = element_rect(fill = "#d0e2f0", color = "black", linewidth = 0.5),
    strip.text.x = element_text(color = "#0E3065", face = "plain", family = my_font_2, size = 12),
    panel.grid = element_line(color = "grey90", linewidth = 0.2),
    legend.position = "bottom",
    plot.title = element_text(face = "plain"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )


ggsave(
  filename = file.path(output_dir, "sensitivity_npv_profitability_heatmap_maize.png"),
  width = 10,
  height = 18,
  dpi = 600
)

# first year profit only

ggplot(sensitivity_df_maize, aes(x = crop_mult, y = lime_mult, fill = pct_positive)) +
  geom_tile(color = "grey90") +
  # geom_text(aes(label = sprintf("%.0f%%", pct_positive)), color = "white", size = 3) +
  facet_wrap(~facet_label_2, ncol = 3) +
  scale_fill_viridis_c(name = "% fields profitable", option = "C", limits = c(0, 100)) +
  labs(
    title = "Sensitivity of Profitability (First-year) to Crop–Lime Price Ratios (Maize, Year 1)",
    subtitle = "Each panel shows % of fields with positive profit under each price scenario",
    x = "Crop price multiplier",
    y = "Lime price multiplier"
  ) +
  ggthemes::theme_pander(base_size = 14, base_family = my_font_2) +
  theme(
    strip.background = element_rect(fill = "#d0e2f0", color = "black", linewidth = 0.5),
    strip.text.x = element_text(color = "#0E3065", face = "plain", family = my_font_2, size = 12),
    panel.grid = element_line(color = "grey90", linewidth = 0.2),
    legend.position = "bottom",
    plot.title = element_text(face = "plain"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )


ggsave(
  filename = file.path(output_dir, "sensitivity_profitability_heatmap_maize_year1.png"),
  width = 10,
  height = 18,
  dpi = 600
)

# only beans for simplicity
sensitivity_df_beans <- sens_summary_crop |>
  filter(tolower(crop) == "beans") |>
  mutate(facet_label_2 = paste0(admin2_gadm, " | ", treatment))

ggplot(sensitivity_df_beans, aes(x = crop_mult, y = lime_mult, fill = pct_positive_npv)) +
  geom_tile(color = "grey90") +
  # geom_text(aes(label = sprintf("%.0f%%", pct_positive_npv)),
  #           color = "white", size = 3) +
  facet_wrap(~facet_label_2, ncol = 3) +
  scale_fill_viridis_c(name = "% fields NPV>0", option = "C", limits = c(0, 100)) +
  labs(
    title = "Sensitivity of NPV Profitability to Crop–Lime Price Ratios (Beans)",
    subtitle = "Each panel shows % of fields with positive NPV under each price scenario",
    x = "Crop price multiplier",
    y = "Lime price multiplier"
  ) +
  ggthemes::theme_pander(base_size = 14, base_family = my_font_2) +
  theme(
    strip.background = element_rect(fill = "#d0e2f0", color = "black", linewidth = 0.5),
    strip.text.x = element_text(color = "#0E3065", face = "plain", family = my_font_2, size = 12),
    panel.grid = element_line(color = "grey90", linewidth = 0.2),
    legend.position = "bottom",
    plot.title = element_text(face = "plain"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )


ggsave(
  filename = file.path(output_dir, "sensitivity_npv_profitability_heatmap_beans.png"),
  width = 10,
  height = 18,
  dpi = 600
)

ggplot(sensitivity_df_beans, aes(x = crop_mult, y = lime_mult, fill = pct_positive)) +
  geom_tile(color = "grey90") +
  # geom_text(aes(label = sprintf("%.0f%%", pct_positive)), color = "white", size = 3) +
  facet_wrap(~facet_label_2, ncol = 3) +
  scale_fill_viridis_c(name = "% fields profitable", option = "C", limits = c(0, 100)) +
  labs(
    title = "Sensitivity of Profitability (First-year) to Crop–Lime Price Ratios (Beans, Year 1)",
    subtitle = "Each panel shows % of fields with positive profit under each price scenario",
    x = "Crop price multiplier",
    y = "Lime price multiplier"
  ) +
  ggthemes::theme_pander(base_size = 14, base_family = my_font_2) +
  theme(
    strip.background = element_rect(fill = "#d0e2f0", color = "black", linewidth = 0.5),
    strip.text.x = element_text(color = "#0E3065", face = "plain", family = my_font_2, size = 12),
    panel.grid = element_line(color = "grey90", linewidth = 0.2),
    legend.position = "bottom",
    plot.title = element_text(face = "plain"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )


ggsave(
  filename = file.path(output_dir, "sensitivity_profitability_heatmap_beans_year1.png"),
  width = 10,
  height = 18,
  dpi = 600
)

# ----------------------------------------------
# 6.2. Sensitivity: Profitability vs Lime/Crop Price Ratio
# ----------------------------------------------

ratio_df <- sensitivity_df |>
  mutate(price_ratio = lime_price / crop_price) |>
  group_by(crop, treatment, admin2_gadm, price_ratio) |>
  summarise(
    pct_positive_npv = mean(npv > 0, na.rm = TRUE) * 100,
    pct_positive = mean(profit_y1 > 0, na.rm = TRUE) * 100,
    .groups = "drop"
  ) |>
  mutate(facet_label_3 = paste0(admin2_gadm, " | ", crop))

baseline_ratio_df <- sensitivity_df |>
  filter(lime_mult == 1, crop_mult == 1) |>
  group_by(crop, admin2_gadm) |>
  summarise(baseline_ratio = mean(lime_price / crop_price, na.rm = TRUE), .groups = "drop")


baseline_points <- sensitivity_df |>
  filter(crop_mult == 1, lime_mult == 1) |>
  group_by(crop, treatment, admin2_gadm) |>
  summarise(
    price_ratio = mean(lime_price / crop_price, na.rm = TRUE),
    pct_positive = mean(profit_y1 > 0, na.rm = TRUE) * 100,
    pct_positive_npv = mean(npv > 0, na.rm = TRUE) * 100,
    .groups = "drop"
  ) |>
  mutate(facet_label_3 = paste0(admin2_gadm, " | ", crop))

ggplot(ratio_df, aes(x = price_ratio, y = pct_positive, color = treatment)) +
  # geom_line(size = 1.1) +
  geom_smooth(method = "loess", se = F, span = 0.2, linewidth = 0.6) +
  geom_point(
    data = baseline_points,
    aes(x = price_ratio, y = pct_positive, color = treatment),
    shape = 15,
    size = 3,
    stroke = 0.8,
    inherit.aes = FALSE
  ) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.01)) +
  scale_color_manual(values = bar_colors) +
  facet_wrap(~facet_label_3, scale = "free") +
  labs(
    title = "Smoothed Profitability Threshold by Lime–Crop Price Ratio",
    subtitle = "Share of fields with positive First-Year Profit (Point = baseline prices)",
    x = "Lime/Crop price ratio",
    y = "% of fields with positive First-Year Profit",
  ) +
  ggthemes::theme_pander(base_size = 14, base_family = my_font_2) +
  theme(
    strip.background = element_rect(fill = "#d0e2f0", color = "black", linewidth = 0.5),
    strip.text.x = element_text(color = "#0E3065", face = "plain", family = my_font_2, size = 12),
    panel.grid = element_line(color = "grey90", linewidth = 0.2),
    legend.position = c(0.8, 0.15),
    plot.title = element_text(face = "plain"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )


ggsave(
  filename = file.path(output_dir, "sensitivity_profitability_vs_price_ratio.png"),
  width = 10,
  height = 16,
  dpi = 600
)

ggplot(ratio_df, aes(x = price_ratio, y = pct_positive_npv, color = treatment)) +
  # geom_point(alpha = 0.4, size = 2) +  # raw values
  # geom_line(size = 1.1) +
  geom_smooth(method = "loess", se = F, span = 0.2, linewidth = 0.6) +
  geom_point(
    data = baseline_points,
    aes(x = price_ratio, y = pct_positive_npv, color = treatment),
    shape = 15,
    size = 3,
    stroke = 0.8,
    inherit.aes = FALSE
  ) +
  scale_x_continuous(labels = number_format(accuracy = 0.1)) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_color_manual(values = bar_colors) +
  facet_wrap(~facet_label_3, scale = "free") +
  labs(
    title = "Smoothed Probability of Profitability by Lime–Crop Price Ratio",
    subtitle = "Share of fields with positive NPV (4 years, 25 % benefit decay)(Point = baseline prices)",
    x = "Lime / Crop price ratio",
    y = "% of fields with positive NPV",
    color = "Crop"
  ) +
  ggthemes::theme_pander(base_size = 14, base_family = my_font_2) +
  theme(
    strip.background = element_rect(fill = "#d0e2f0", color = "black", linewidth = 0.5),
    strip.text.x = element_text(color = "#0E3065", face = "plain", family = my_font_2, size = 12),
    panel.grid = element_line(color = "grey90", linewidth = 0.2),
    legend.position = c(0.8, 0.15),
    plot.title = element_text(face = "plain"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

ggsave(
  filename = file.path(output_dir, "sensitivity_npv_profitability_vs_price_ratio.png"),
  width = 10,
  height = 16,
  dpi = 600
)

df_field |>
  group_by(crop, admin2_gadm, treatment) |>
  arrange(profit_y1) |>
  mutate(cumshare = row_number() / n()) |>
  mutate(facet_label = paste0(admin2_gadm, " | ", crop)) |>
  ggplot(aes(x = profit_y1, y = cumshare, color = treatment)) +
  geom_line(linewidth = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#490000") +
  facet_wrap(~facet_label) +
  scale_color_manual(values = bar_colors) +
  labs(
    x = "Profit (USD/ha)", y = "Cumulative Share of Fields",
    title = "Cumulative Profitability Curve by Crop"
  ) +
  ggthemes::theme_pander(base_size = 14, base_family = my_font_2) +
  theme(
    strip.background = element_rect(fill = "#d0e2f0", color = "black", linewidth = 0.5),
    strip.text.x = element_text(color = "#0E3065", face = "plain", family = my_font_2, size = 12),
    panel.grid = element_line(color = "grey90", linewidth = 0.2),
    legend.position = c(0.8, 0.05),
    plot.title = element_text(face = "plain"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )


ggsave(
  filename = file.path(output_dir, "cumulative_profitability_curve.png"),
  width = 10,
  height = 16,
  dpi = 600
)

df_field |>
  group_by(crop, admin2_gadm, treatment) |>
  arrange(npv) |>
  mutate(cumshare = row_number() / n()) |>
  mutate(facet_label = paste0(admin2_gadm, " | ", crop)) |>
  ggplot(aes(x = npv, y = cumshare, color = treatment)) +
  geom_line(linewidth = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#490000") +
  facet_wrap(~facet_label) +
  scale_color_manual(values = bar_colors) +
  labs(
    x = "NPV (USD/ha)", y = "Cumulative Share of Fields",
    title = "Cumulative NPV Profitability Curve by Crop"
  ) +
  ggthemes::theme_pander(base_size = 14, base_family = my_font_2) +
  theme(
    strip.background = element_rect(fill = "#d0e2f0", color = "black", linewidth = 0.5),
    strip.text.x = element_text(color = "#0E3065", face = "plain", family = my_font_2, size = 12),
    panel.grid = element_line(color = "grey90", linewidth = 0.2),
    legend.position = c(0.8, 0.05),
    plot.title = element_text(face = "plain"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )


ggsave(
  filename = file.path(output_dir, "cumulative_npv_profitability_curve.png"),
  width = 10,
  height = 16,
  dpi = 600
)
df_field |>
  mutate(facet_label = paste0(admin2_gadm, " | ", crop)) |>
  ggplot(aes(x = ex_ac_BP, y = npv, color = treatment)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", span = 0.8, linewidth = 0.5) +
  facet_wrap(~facet_label, scale = "free") +
  labs(
    title = "NPV vs Exchangable acidity",
    x = "Exchangable Acidity (cmol/kg)",
    y = "Profit (USD/ha)"
  ) +
  scale_color_manual(values = bar_colors) +
  scale_y_continuous(labels = dollar_format()) +
  ggthemes::theme_pander(base_size = 14, base_family = my_font_2) +
  theme(
    strip.background = element_rect(fill = "#d0e2f0", color = "black", linewidth = 0.5),
    strip.text.x = element_text(color = "#0E3065", face = "plain", family = my_font_2, size = 12),
    panel.grid = element_line(color = "grey90", linewidth = 0.2),
    legend.position = c(0.8, 0.1),
    plot.title = element_text(face = "plain"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

ggsave(
  filename = file.path(output_dir, "profit_vs_ex_acidity.png"),
  width = 10,
  height = 12,
  dpi = 600
)

# ph vs profit
df_field |>
  mutate(facet_label = paste0(admin2_gadm, " | ", crop)) |>
  ggplot(aes(x = p_h_BP, y = npv, color = treatment)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", span = 0.8, linewidth = 0.6) +
  facet_wrap(~facet_label, scale = "free") +
  labs(
    title = "NPV vs Soil pH",
    x = "Soil pH",
    y = "NPV (USD/ha)"
  ) +
  scale_color_manual(values = bar_colors) +
  scale_y_continuous(labels = dollar_format()) +
  ggthemes::theme_pander(base_size = 14, base_family = my_font_2) +
  theme(
    strip.background = element_rect(fill = "#d0e2f0", color = "black", linewidth = 0.5),
    strip.text.x = element_text(color = "#0E3065", face = "plain", family = my_font_2, size = 12),
    panel.grid = element_line(color = "grey90", linewidth = 0.2),
    legend.position = c(0.8, 0.08),
    plot.title = element_text(face = "plain"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

ggsave(
  filename = file.path(output_dir, "profit_vs_ph.png"),
  width = 10,
  height = 16,
  dpi = 600
)


# -----------------------------
# Write outputs
# -----------------------------
write_csv(df_field, file.path(output_dir, "field_profitability.csv"))
write_csv(
  sensitivity_df,
  file.path(output_dir, "sensitivity_field_level.csv")
)

# ─────────────────────────────────────────────────────────────
# 9. BREAK-EVEN ANALYSIS
# ─────────────────────────────────────────────────────────────

# -------------------------------------------------------------
# (A) 1st-Year Baseline Profitability (Who breaks even now?)
# -------------------------------------------------------------
baseline_break_even <- df_field %>%
  mutate(is_profitable = profit_y1 >= 0) %>%
  group_by(country, admin2_gadm, crop, treatment) %>%
  summarise(
    n_fields = n(),
    pct_profitable = mean(is_profitable, na.rm = TRUE) * 100,
    mean_profit = mean(profit_y1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_profitable))

# Save and print summary
write_csv(baseline_break_even, file.path(output_dir, "break_even_baseline_summary.csv"))
print(baseline_break_even, n = 20)


# -------------------------------------------------------------
# (B) Break-even Lime Price (Field-level threshold)
# -------------------------------------------------------------
break_even_field <- df_field %>%
  mutate(
    lime_price_break_even = ifelse(
      lime_tha > 0,
      (yield_response * crop_price_base) / lime_tha,
      NA_real_
    ),
    break_even_gap = lime_price_base - lime_price_break_even
  )

# Summary: average threshold by site × crop
break_even_summary <- break_even_field %>%
  group_by(country, admin2_gadm, crop, treatment) %>%
  summarise(
    mean_break_even_price = mean(lime_price_break_even, na.rm = TRUE),
    median_break_even_price = median(lime_price_break_even, na.rm = TRUE),
    current_lime_price = mean(lime_price_base, na.rm = TRUE),
    pct_profitable_now = mean(profit_y1 > 0, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    lime_price_gap = current_lime_price - mean_break_even_price,
    status = ifelse(lime_price_gap > 0, "Too Expensive", "Profitable")
  )

write_csv(break_even_summary, file.path(output_dir, "break_even_lime_price_summary.csv"))

# -------------------------------------------------------------
# (C) Visualization: Distribution of Break-even Lime Prices
# -------------------------------------------------------------

# if prices are negative (i.e., no lime needed), set to zero for visualization
break_even_field <- break_even_field %>%
  mutate(lime_price_break_even = pmax(lime_price_break_even, 0))|>
  mutate(facet_label = paste0(admin2_gadm, " | ", crop))
g1 <- ggplot(break_even_field, aes(x = lime_price_break_even, fill = treatment)) +
  geom_histogram(bins = 20, alpha = 0.7, position = "identity") +
  # i want to add density curves but not sure how to scale them properly
  scale_fill_manual(values = bar_colors) +
  scale_x_continuous(labels = dollar_format()) +
  facet_wrap(~facet_label, scales = "free_y") +
  labs(
    title = "Distribution of Break-even Lime Prices per Field",
    x = "Break-even Lime Price (USD/t)",
    y = "Number of Fields"
  ) +
  theme_bisrat()
g1
ggsave(file.path(output_dir, "hist_break_even_lime_price.png"), g1, width = 10, height = 7, dpi = 500)


# -------------------------------------------------------------
# (D) Visualization: % of Fields Profitable vs Lime Price
# -------------------------------------------------------------
# Simulate how % profitable changes with different lime prices
lime_price_seq <- seq(0, 200, by = 10)

profit_vs_limeprice <- df_field %>%
  select(country, admin2_gadm,treatment, crop, yield_response, lime_tha, crop_price_base) %>%
  crossing(lime_price = lime_price_seq) %>%
  mutate(
    profit_y1 = (yield_response * crop_price_base) - (lime_tha * lime_price)
  ) %>%
  group_by(admin2_gadm, crop, treatment, lime_price) %>%
  summarise(
    pct_profitable = mean(profit_y1 >= 0, na.rm = TRUE) * 100,
    .groups = "drop"
  ) |>
  mutate(facet_label = paste0(admin2_gadm, " | ", treatment))

g2 <- ggplot(profit_vs_limeprice, aes(x = lime_price, y = pct_profitable, color = crop)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~facet_label, scales = "free_y", ncol=4) +
  scale_color_manual(values = bar_colors) +
  scale_x_continuous(labels = dollar_format()) +
  labs(
    title = "Share of Fields with Positive Profit vs Lime Price",
    subtitle = "By Site × Crop — price thresholds for profitability",
    x = "Lime Price (USD/t)",
    y = "% of Fields Profitable"
  ) +
  theme_bisrat() +
  theme(legend.position = "bottom")

g2
ggsave(file.path(output_dir, "profitability_vs_lime_price_curve.png"), g2, width = 10, height = 14, dpi = 500)


# -------------------------------------------------------------
# (E) Visualization: Site-level Comparison (Mean Break-even Price)
# -------------------------------------------------------------
g3 <- ggplot(break_even_summary, aes(x = reorder(admin2_gadm, mean_break_even_price),
                                     y = mean_break_even_price, fill = crop)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = bar_colors) +
  scale_y_continuous(labels = dollar_format()) +
  facet_wrap(~treatment) +
  coord_flip() +
  labs(
    title = "Average Break-even Lime Price by Site × Crop",
    x = "Site (Admin2)",
    y = "Mean Break-even Lime Price (USD/t)"
  ) +
  theme_bisrat()
g3
ggsave(file.path(output_dir, "break_even_lime_price_by_site_crop.png"), g3, width = 10, height = 8, dpi = 500)

# Calculate percentage change in lime price needed for break-even
break_even_summary <- break_even_summary %>%
  mutate(
    pct_change_needed = ((mean_break_even_price - current_lime_price) / current_lime_price) * 100,
    label_text = sprintf("%.0f%%", pct_change_needed),
    label_y = pct_change_needed / 2,                 # center of each bar
    # tiny bars: nudge labels just outside so they don't vanish
    label_y = ifelse(abs(pct_change_needed) < 3,
                     ifelse(pct_change_needed >= 0, pct_change_needed + 2, pct_change_needed - 2),
                     label_y),
    is_small = abs(pct_change_needed) <=15
  )

# Plot
g_pct <- ggplot(break_even_summary,
                aes(x = reorder(admin2_gadm, pct_change_needed),
                    y = pct_change_needed,
                    fill = crop,
                    group = crop)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  # labels ON the bars (same dodge, y = midpoint)
  geom_text(aes(y = label_y, label = label_text),
            position = position_dodge(width = 0.7),
            size = 2, family = my_font_2, fontface = "plain",
            color = "black") +     # use black for readability across fills
  scale_fill_manual(values = bar_colors) +
  coord_flip(clip = "off") +
  facet_wrap(~treatment) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     breaks = scales::pretty_breaks(n = 6),
                     expand = expansion(mult = c(0.06, 0.10))) +
  labs(
    title = "Percentage Change in Lime Price Needed for Break-even",
    subtitle = "Negative = price must drop; Positive = already below break-even",
    x = "Site (Admin2)",
    y = "Required Change in Lime Price (%)",
    fill = "Crop"
  ) +
  theme_bisrat() +
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        strip.text = element_text(size = 12, color = "#0E3065"))


g_pct
ggsave(file.path(output_dir, "break_even_lime_price_pct_change_by_site_crop.png"),
       g_pct, width = 10, height = 4, dpi = 500)


# ─────────────────────────────────────────────────────────────
# 11. PROFITABILITY QUANTILE THRESHOLDS (e.g. 50%, 80%)
# ─────────────────────────────────────────────────────────────

lime_price_seq <- seq(0, 200, by = 10)

# 1️⃣ Profitability curve at each lime price
profit_curve <- df_field %>%
  select(country, admin2_gadm, crop, treatment,
         yield_response, lime_tha, crop_price_base, lime_price_base) %>%
  crossing(lime_price = lime_price_seq) %>%
  mutate(
    profit_y1 = (yield_response * crop_price_base) - (lime_tha * lime_price),
    profitable = profit_y1 >= 0
  ) %>%
  group_by(country, admin2_gadm, crop, treatment, lime_price) %>%
  summarise(
    pct_profitable = mean(profitable, na.rm = TRUE) * 100,
    current_lime_price = mean(lime_price_base, na.rm = TRUE),
    .groups = "drop"
  )

# 2️⃣ Define profitability targets
target_levels <- c(50, 60, 70, 80)

safe_approx <- function(x, y, target, rule = 2) {
  if (length(na.omit(x)) < 2 || length(na.omit(y)) < 2) return(NA_real_)
  tryCatch(approx(x, y, xout = target, rule = rule)$y, error = function(e) NA_real_)
}

baseline_ratio_df <- df_field %>%
  mutate(price_ratio_base = lime_price_base / crop_price_base) %>%
  group_by(country, admin2_gadm, crop) %>%
  summarise(
    baseline_ratio = mean(price_ratio_base, na.rm = TRUE),
    .groups = "drop"
  )

threshold_quantiles <- profit_curve %>%
  group_by(country, admin2_gadm, crop, treatment) %>%
  summarise(
    price_for_50pct = safe_approx(pct_profitable, lime_price, 50),
    price_for_60pct = safe_approx(pct_profitable, lime_price, 60),
    price_for_70pct = safe_approx(pct_profitable, lime_price, 70),
    price_for_80pct = safe_approx(pct_profitable, lime_price, 80),
    current_lime_price = mean(current_lime_price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    price_change_50 = (price_for_50pct - current_lime_price) / current_lime_price * 100,
    price_change_60 = (price_for_60pct - current_lime_price) / current_lime_price * 100,
    price_change_70 = (price_for_70pct - current_lime_price) / current_lime_price * 100,
    price_change_80 = (price_for_80pct - current_lime_price) / current_lime_price * 100
  )

plot_df <- profit_curve %>%
  #mutate(price_ratio = lime_price / current_lime_price) %>%
  left_join(threshold_quantiles, by = c("country", "admin2_gadm", "crop", "treatment")) %>%
  left_join(baseline_ratio_df, by = c("country", "admin2_gadm", "crop")) |>
  select(-current_lime_price.y) %>%
  rename(current_lime_price = current_lime_price.x)

ggplot(plot_df, aes(x = lime_price / current_lime_price, y = pct_profitable, color = crop)) +
  geom_smooth(se = FALSE, linewidth = 1.2, span = 0.6) +
  
  # baseline price ratio (dotted black)
  geom_vline(aes(xintercept = baseline_ratio),
             color = "black", linetype = "dotted", linewidth = 0.8) +
  
  # thresholds (dashed lines)
  geom_vline(aes(xintercept = price_for_50pct / current_lime_price), color = "#FF6666", linetype = "dashed") +
  geom_vline(aes(xintercept = price_for_60pct / current_lime_price), color = "#FF9933", linetype = "dashed") +
  geom_vline(aes(xintercept = price_for_70pct / current_lime_price), color = "#33CC33", linetype = "dashed") +
  geom_vline(aes(xintercept = price_for_80pct / current_lime_price), color = "#3399FF", linetype = "dashed") +
  
  # text labels for thresholds
  geom_text(
    aes(x = price_for_50pct / current_lime_price, y = 5, label = "50%"),
    color = "#FF6666", angle = 90, vjust = -0.5, size = 3
  ) +
  geom_text(
    aes(x = price_for_60pct / current_lime_price, y = 5, label = "60%"),
    color = "#FF9933", angle = 90, vjust = -0.5, size = 3
  ) +
  geom_text(
    aes(x = price_for_70pct / current_lime_price, y = 5, label = "70%"),
    color = "#33CC33", angle = 90, vjust = -0.5, size = 3
  ) +
  geom_text(
    aes(x = price_for_80pct / current_lime_price, y = 5, label = "80%"),
    color = "#3399FF", angle = 90, vjust = -0.5, size = 3
  ) +
  
  facet_wrap(~ interaction(admin2_gadm, treatment, sep = " | "), ncol = 4) +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 100)) +
  scale_color_manual(values = bar_colors) +
  labs(
    title = "Profitability Thresholds by Lime–Crop Price Ratio",
    subtitle = "Smoothed curves showing % of fields with positive profit; dashed lines = 50–80% thresholds; dotted = current price ratio",
    x = "Lime-to-Crop Price Ratio (relative to baseline)",
    y = "% of fields with positive profit",
    color = "Crop"
  ) +
  ggthemes::theme_pander(base_size = 14, base_family = "Frutiger") +
  theme(
    strip.background = element_rect(fill = "#1871B8", color = "white"),
    strip.text = element_text(color = "white", face = "bold"),
    panel.grid = element_line(color = "grey90", linewidth = 0.2),
    legend.position = c(0.85, 0.05),
    plot.title = element_text(face = "plain")
  )









# 3️⃣ For each site × crop × treatment, find price meeting each target
threshold_quantiles <- profit_curve %>%
  group_by(country, admin2_gadm, crop, treatment) %>%
  summarise(
    price_for_50pct = { tmp <- unique(cbind(pct_profitable, lime_price));
    approx(tmp[,1], tmp[,2], xout = 50, rule = 2)$y },
    price_for_60pct = { tmp <- unique(cbind(pct_profitable, lime_price));
    approx(tmp[,1], tmp[,2], xout = 60, rule = 2)$y },
    price_for_70pct = { tmp <- unique(cbind(pct_profitable, lime_price));
    approx(tmp[,1], tmp[,2], xout = 70, rule = 2)$y },
    price_for_80pct = { tmp <- unique(cbind(pct_profitable, lime_price));
    approx(tmp[,1], tmp[,2], xout = 80, rule = 2)$y },
    current_lime_price = mean(current_lime_price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    price_change_50 = (price_for_50pct - current_lime_price) / current_lime_price * 100,
    price_change_60 = (price_for_60pct - current_lime_price) / current_lime_price * 100,
    price_change_70 = (price_for_70pct - current_lime_price) / current_lime_price * 100,
    price_change_80 = (price_for_80pct - current_lime_price) / current_lime_price * 100
  )
write_csv(threshold_quantiles,
          file.path(output_dir, "profitability_quantile_thresholds.csv"))

# Make long-form dataset from threshold_quantiles
threshold_long <- threshold_quantiles %>%
  select(admin2_gadm, crop, treatment,
         starts_with("price_change_")) %>%
  pivot_longer(
    cols = starts_with("price_change_"),
    names_to = "target",
    values_to = "pct_change"
  ) %>%
  mutate(
    target = str_remove(target, "price_change_") %>% paste0("%"),
    pct_change = round(pct_change, 1)
  )|>
  mutate(facet_label = paste0(admin2_gadm, " | ", treatment))

# Vertical grouped bar chart
g_q_bar <- ggplot(threshold_long,
                  aes(x = target, y = pct_change, fill = crop)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  
  # Add labels on top of bars
  geom_text(aes(label = paste0(ifelse(pct_change > 0, "+", ""), pct_change, "%")),
            position = position_dodge(width = 0.8),
            vjust = ifelse(threshold_long$pct_change < 0, 1.2, -0.4),
            size = 3.2, family = my_font_2) +
  
  facet_wrap(~facet_label, scales = "free_x", ncol=3) +
  scale_fill_manual(values = bar_colors) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     breaks = scales::pretty_breaks(n = 6),
                     expand = expansion(mult = c(0.05, 0.1))) +
  labs(
    title = "Required Lime Price Change for Different Profitability Levels",
    subtitle = "Negative = lime must become cheaper; Positive = already profitable",
    x = "Target Share of Fields Profitable",
    y = "Required Lime Price Change (%)",
    fill = "Crop"
  ) +
  theme_bisrat() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12, color = "#0E3065")
  )


g_q_bar



ggsave(file.path(output_dir, "profitability_threshold_curve_quantile.png"),
       g_q_bar, width = 12, height = 20, dpi = 500)

# Base plot
g_arrow <- ggplot(threshold_long,
                  aes(y = target, xend = pct_change, x = 0, color = crop)) +
  # draw arrows from 0 to required % change
  geom_segment(arrow = arrow(length = unit(0.18, "cm")),
               linewidth = 1.1, show.legend = TRUE, position = position_dodge(width = 0.7)) +
  
  # label at end of arrow
  geom_text(aes(x = pct_change, label = paste0(ifelse(pct_change > 0, "+", ""), pct_change, "%")),
            position = position_dodge(width = 0.7),
            vjust = 0.4,
            size = 3.5,
            family = my_font_2,
            color = "black") +
  
  geom_vline(xintercept = 0, color = "black", linewidth = 0.3) +
  scale_color_manual(values = bar_colors) +
  facet_wrap(~facet_label, ncol = 3) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     breaks = scales::pretty_breaks(n = 6),
                     expand = expansion(mult = c(0.1, 0.15))) +
  labs(
    title = "Required Lime Price Change for Different Profitability Levels",
    subtitle = "Arrows point toward cheaper lime (negative = reduction needed)",
    x = "Required Change in Lime Price (%)",
    y = "Profitability Target (% of Fields Profitable)",
    color = "Crop"
  ) +
  theme_bisrat() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12, color = "#0E3065"),
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 30, 10, 10)
  )
g_arrow