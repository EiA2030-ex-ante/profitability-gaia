# =========================================================
# Liming Profitability
# =========================================================
# Author: Bisrat H
# date: 28/09/2025
# What it does:
# - Reads raw trial data + baseline price tables (mainly assumptions)
# - Computes per-field yield response (vs T1 control) per site × crop
# - Additional revenue, total lime cost, profit gain (field & rollups)
# - NPV with 25%/yr benefit decay over 4 years
# - Sensitivity of profit to crop & lime prices
# Outputs written to "outputs/" as CSVs
# =========================================================

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
  "#454283",
  "#490000",
  "#4E5E77",
  "#8F2D56"
)

# -----------------------------
# Paths (edit if needed)
# -----------------------------
raw_data_path <- "tmp/data_y1.dta" # raw, observational data
base_crop_price_path <- "tmp/base_crop_prices.csv" # cols: country,admin2_gadm,crop,crop_price_base
base_lime_price_path <- "tmp/base_lime_price.csv" # cols: country,lime_price_base
output_dir <- "outputs/profitability_analysis" # output directory

dir_create(output_dir)

# -----------------------------
# Tunable parameters
# -----------------------------
default_crop_price <- 160 # USD/t if baseline file row missing
default_lime_price <- 55 # USD/t if baseline file row missing

discount_rate <- 0.10 # 10% annual discount (editable)
benefit_decay <- 0.25 # 25%/year decay in yield benefit
time_horizon_years <- 4 # 4 years

# Sensitivity grid (multipliers on baseline prices)
crop_multipliers <- seq(0.6, 1.8, by = 0.1) # 60%..180% of baseline
lime_multipliers <- seq(0.6, 1.8, by = 0.1)

# -----------------------------
# Helper functions
# -----------------------------
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
# For a field with multiple treatment rows including T1 (lime_tha=0 often), we set:
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

# -----------------------------
# Load data
# -----------------------------
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
    !is.na(fid)
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

# -----------------------------
# Prepare responses & prices
# -----------------------------
df_resp <- compute_yield_response(df_raw)

# drop the T1 row for the response reporting (its response is 0 by definition),
# but keep it if you want to explicitly see zeros—here we remove T1 for profit calc
df_resp_nz <- df_resp |> filter(treatment != "T1")

# Attach baseline crop price (by country, site, crop) and lime price (by country)
df_resp_pr <- df_resp_nz |>
  left_join(base_crop_prices, by = c("country", "admin2_gadm", "crop")) |>
  left_join(base_lime_price, by = "country") |>
  mutate(
    crop_price_base = ifelse(is.na(crop_price_base), default_crop_price, crop_price_base),
    lime_price_base = ifelse(is.na(lime_price_base), default_lime_price, lime_price_base)
  )

# -----------------------------
# Profit & NPV calculations
# -----------------------------
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
# 1. Boxplot of Profit by Site (facet by Crop)
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
  facet_wrap( ~ facet_label, scales = "free_y") +
  scale_y_continuous(labels = dollar_format()) +
  scale_fill_manual(values = bar_colors) +
  labs(
    title = "Field-level Profit by Site and Crop",
    x = "Site",
    y = "Profit (USD/ha)"
  ) +
  ggthemes::theme_pander(base_size = 14, base_family = my_font_2) +
  theme(
    strip.background = element_rect(fill = "#d0e2f0", color = "white"),
    strip.text.x = element_text(color = "#0E3065", face = "bold", family = my_font_2, size = 12),
    panel.grid = element_line(color = "grey90", linewidth = 0.2),
    legend.position = "none",
    plot.title = element_text(face = "plain")
  )

ggsave(file.path(output_dir, "box_profit_by_site_crop.png"), width = 12, height = 16, dpi = 400)


# ----------------------------------------------
# 2. Boxplot of NPV by Site (facet by Crop)
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
  facet_wrap( ~ facet_label, scales = "free_y") +
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
    strip.background = element_rect(fill = "#d0e2f0", color = "white"),
    strip.text.x = element_text(color = "#0E3065", face = "bold", family = my_font_2, size = 12),
    panel.grid = element_line(color = "grey90", linewidth = 0.2),
    legend.position = "none",
    plot.title = element_text(face = "plain")
  )

ggsave(file.path(output_dir, "box_npv_by_site_crop.png"), width = 12, height = 16, dpi = 600)


# -----------------------------
# Sensitivity analysis (profit & NPV) vs price multipliers
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
# 3. Sensitivity: % of fields with positive profit
# ----------------------------------------------

#  Summarize probability of profit > 0 by crop
sens_summary_crop <- sensitivity_df %>%
  group_by(crop, admin2_gadm,treatment, crop_mult, lime_mult) %>%
  summarise(
    pct_positive = mean(profit_y1 > 0, na.rm = TRUE) * 100,
    pct_positive_npv = mean(npv > 0, na.rm = TRUE) * 100,
    .groups = "drop"
  )%>%
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
    strip.background = element_rect(fill = "#d0e2f0", color = "white"),
    strip.text.x  = element_text(color = "#0E3065", face = "bold", family = my_font_2, size = 12),
    legend.position = c(0.8, 0.15),
    plot.title = element_text(face = "plain")
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
    strip.background = element_rect(fill = "#d0e2f0", color = "white"),
    strip.text.x = element_text(color = "#0E3065", face = "bold", family = my_font_2, size = 12),
    legend.position = c(0.8, 0.15),
    plot.title = element_text(face = "plain")
  )


ggsave(
  filename = file.path(output_dir, "sensitivity_npv_profitability_heatmap_by_crop_site.png"),
  width = 10,
  height = 14,
  dpi = 600
)


# only maize for simplicity
sensitivity_df_maize <- sens_summary_crop %>%
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
    strip.background = element_rect(fill = "#d0e2f0", color = "white"),
    strip.text.x = element_text(color = "#0E3065", face = "bold", family = my_font_2, size = 12),
    legend.position = "bottom",
    plot.title = element_text(face = "plain")
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
    strip.background = element_rect(fill = "#d0e2f0", color = "white"),
    strip.text.x = element_text(color = "#0E3065", face = "bold", family = my_font_2, size = 12),
    legend.position = "bottom",
    plot.title = element_text(face = "plain")
  )

ggsave(
  filename = file.path(output_dir, "sensitivity_profitability_heatmap_maize_year1.png"),
  width = 10,
  height = 18,
  dpi = 600
)

# only beans for simplicity
sensitivity_df_beans <- sens_summary_crop %>%
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
    strip.background = element_rect(fill = "#d0e2f0", color = "white"),
    strip.text.x = element_text(color = "#0E3065", face = "bold", family = my_font_2, size = 12),
    legend.position = "bottom",
    plot.title = element_text(face = "plain")
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
    strip.background = element_rect(fill = "#d0e2f0", color = "white"),
    strip.text.x = element_text(color = "#0E3065", face = "bold", family = my_font_2, size = 12),
    legend.position = "bottom",
    plot.title = element_text(face = "plain")
  )

ggsave(
  filename = file.path(output_dir, "sensitivity_profitability_heatmap_beans_year1.png"),
  width = 10,
  height = 18,
  dpi = 600
)

# ----------------------------------------------
# 4. Sensitivity: Profitability vs Lime/Crop Price Ratio
# ----------------------------------------------

ratio_df <- sensitivity_df %>%
  mutate(price_ratio = lime_price / crop_price) %>%
  group_by(crop, treatment, admin2_gadm, price_ratio) %>%
  summarise(
    pct_positive_npv = mean(npv > 0, na.rm = TRUE) * 100,
    pct_positive = mean(profit_y1 > 0, na.rm = TRUE) * 100,
    .groups = "drop"
  ) |>
  mutate(facet_label_3 = paste0(admin2_gadm, " | ", crop))

baseline_ratio_df <- sensitivity_df %>%
  filter(lime_mult == 1, crop_mult == 1) %>%
  group_by(crop, admin2_gadm) %>%
  summarise(baseline_ratio = mean(lime_price / crop_price, na.rm = TRUE), .groups = "drop")


baseline_points <- sensitivity_df %>%
  filter(crop_mult == 1, lime_mult == 1) %>%
  group_by(crop, treatment, admin2_gadm) %>%
  summarise(
    price_ratio = mean(lime_price / crop_price, na.rm = TRUE),
    pct_positive = mean(profit_y1 > 0, na.rm = TRUE) * 100,
    pct_positive_npv = mean(npv > 0, na.rm = TRUE) * 100,
    .groups = "drop"
  )|>
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
    strip.background = element_rect(fill = "#d0e2f0", color = "white"),
    strip.text.x  = element_text(color = "#0E3065", family = my_font_2, size = 12),
    panel.grid = element_line(color = "grey90", linewidth = 0.2),
    legend.position = c(0.85, 0.05),
    plot.title = element_text(face = "plain")
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
    strip.background = element_rect(fill = "#d0e2f0", color = "white"),
    strip.text.x  = element_text(color = "#0E3065", family = my_font_2, size = 12),
    panel.grid = element_line(color = "grey90", linewidth = 0.2),
    legend.position = c(0.85, 0.05),
    plot.title = element_text(face = "plain")
  )

ggsave(
  filename = file.path(output_dir, "sensitivity_npv_profitability_vs_price_ratio.png"),
  width = 10,
  height = 16,
  dpi = 600
)

df_field %>%
  group_by(crop, admin2_gadm, treatment) %>%
  arrange(profit_y1) %>%
  mutate(cumshare = row_number() / n()) %>%
  mutate(facet_label = paste0(admin2_gadm, " | ", crop)) %>%
  ggplot(aes(x = profit_y1, y = cumshare, color = treatment)) +
  geom_line(size = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#490000") +
  facet_wrap(~ facet_label) +
  scale_color_manual(values = bar_colors) +
  labs(
    x = "Profit (USD/ha)", y = "Cumulative Share of Fields",
    title = "Cumulative Profitability Curve by Crop"
  ) +
  ggthemes::theme_pander(base_size = 14, base_family = my_font_2) +
  theme(
    strip.background = element_rect(fill = "#d0e2f0", color = "white"),
    strip.text.x  = element_text(color = "#0E3065", family = my_font_2, size = 12),
    panel.grid = element_line(color = "grey90", linewidth = 0.2),
    legend.position = c(0.85, 0.05),
    plot.title = element_text(face = "plain")
  )

ggsave(
  filename = file.path(output_dir, "cumulative_profitability_curve.png"),
  width = 10,
  height = 16,
  dpi = 600
)

df_field %>%
  group_by(crop, admin2_gadm, treatment) %>%
  arrange(npv) %>%
  mutate(cumshare = row_number() / n()) %>%
  mutate(facet_label = paste0(admin2_gadm, " | ", crop)) %>%
  ggplot(aes(x = npv, y = cumshare, color = treatment)) +
  geom_line(size = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#490000") +
  facet_wrap(~ facet_label) +
  scale_color_manual(values = bar_colors) +
  labs(
    x = "NPV (USD/ha)", y = "Cumulative Share of Fields",
    title = "Cumulative NPV Profitability Curve by Crop"
  ) +
  ggthemes::theme_pander(base_size = 14, base_family = my_font_2) +
  theme(
    strip.background = element_rect(fill = "#d0e2f0", color = "white"),
    strip.text.x  = element_text(color = "#0E3065", family = my_font_2, size = 12),
    panel.grid = element_line(color = "grey90", linewidth = 0.2),
    legend.position = c(0.85, 0.05),
    plot.title = element_text(face = "plain")
  )

ggsave(
  filename = file.path(output_dir, "cumulative_npv_profitability_curve.png"),
  width = 10,
  height = 16,
  dpi = 600
)
df_field|>
  mutate(facet_label = paste0(admin2_gadm, " | ", crop)) |>
ggplot(aes(x = ex_ac_BP, y = npv, color = treatment)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", span = 0.5, linewidth = 0.5) +
  facet_wrap( ~ facet_label, scale = "free") +
  labs(
    title = "NPV vs Exchangable acidity",
    x = "Exchangable Acidity (cmol/kg)",
    y = "Profit (USD/ha)"
  ) +
  scale_color_manual(values = bar_colors) +
  scale_y_continuous(labels = dollar_format()) +
  ggthemes::theme_pander(base_size = 14, base_family = my_font_2) +
  theme(
    strip.background = element_rect(fill = "#d0e2f0", color = "white"),
    strip.text.x  = element_text(color = "#0E3065", family = my_font_2, size = 12),
    panel.grid = element_line(color = "grey90", linewidth = 0.2),
    legend.position = c(0.85, 0.05),
    plot.title = element_text(face = "plain")
  )
ggsave(
  filename = file.path(output_dir, "profit_vs_ex_acidity.png"),
  width = 10,
  height = 16,
  dpi = 600
)

# ph vs profit
df_field|>
  mutate(facet_label = paste0(admin2_gadm, " | ", crop)) |>
ggplot(aes(x = p_h_BP, y = npv, color = treatment)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", span = 0.5, linewidth = 0.6) +
  facet_wrap( ~ facet_label, scale = "free") +
    labs(
    title = "NPV vs Soil pH",
    x = "Soil pH",
    y = "NPV (USD/ha)"
  ) +
  scale_color_manual(values = bar_colors) +
  scale_y_continuous(labels = dollar_format()) +
  ggthemes::theme_pander(base_size = 14, base_family = my_font_2) +
  theme(
    strip.background = element_rect(fill = "#d0e2f0", color = "white"),
    strip.text.x  = element_text(color = "#0E3065", family = my_font_2, size = 12),
    panel.grid = element_line(color = "grey90", linewidth = 0.2),
    legend.position = c(0.85, 0.05),
    plot.title = element_text(face = "plain")
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