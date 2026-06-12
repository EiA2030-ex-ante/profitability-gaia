# R/globals.R
suppressPackageStartupMessages({
  library(leaflet)
  library(plotly)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(purrr)
  library(scales)
  library(stringr)
  library(ggplot2)
#  library(ggthemes)
  library(viridis)
  library(extrafont)
  library(bslib)
})

# ---------- Load precomputed model outputs ----------
bundle <- readRDS("data/yield_models_bundle.rds")
all_models_df <- bundle$predictions
message("Loaded columns: ", paste(names(all_models_df), collapse = ", "))
perf_summary <- bundle$metrics

# ---- Normalize column names ----
all_models_df <- all_models_df %>%
  rename_with(~ gsub("emmean", "fit", .x)) %>%
  rename_with(~ gsub("lwr|lower.CL", "lower.CL", .x)) %>%
  rename_with(~ gsub("upr|upper.CL", "upper.CL", .x)) %>%
  mutate(
    yield_tha = coalesce(fit, yield_tha),
    country = factor(country),
    admin2_gadm = factor(admin2_gadm),
    crop = factor(crop),
    model = factor(model, levels = c("ols", "lmm", "rf", "xgb"))
  )

catalog <- all_models_df %>%
  distinct(country, admin2_gadm, crop) %>%
  arrange(country, admin2_gadm, crop)

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

extrafont::loadfonts(quiet = TRUE)
my_font_2 <- "Muli"
my_font <- "Frutiger"

# ==========================================================
# load price data
# ==========================================================
safe_read_csv <- function(path) {
  if (file.exists(path)) {
    readr::read_csv(path, show_col_types = FALSE)
  } else {
    NULL
  }
}
base_crop_prices <- safe_read_csv("data/base_crop_prices.csv")
base_lime_price <- safe_read_csv("data/base_lime_price.csv")

# Fallbacks if files are missing:
if (is.null(base_crop_prices)) {
  message("Using fallback crop price baseline (160 USD/t) for all site×crop.")
  base_crop_prices <- all_models_df %>%
    distinct(country, admin2_gadm, crop) %>%
    mutate(crop_price_base = 160)
}

if (is.null(base_lime_price)) {
  message("Using fallback lime price baseline (55 USD/t) by country.")
  base_lime_price <- all_models_df %>%
    distinct(country) %>%
    mutate(lime_price_base = 55)
}

# ==========================================================
# For data section
# ==========================================================
df <- readr::read_csv("data/data_y1.csv") |>
  dplyr::select(-1) |>
  distinct() |>
  mutate(
    crop = as.factor(crop),
    country = as.factor(country),
    treatment = as.factor(treatment),
    lime_factor = factor(lime_tha)
  ) |>
  tidyr::drop_na(yield_tha, lime_tha, fid, crop, country) |>
  mutate(treatment2 = paste0(treatment, " \n(", lime_tha, " t/ha)"))

counts <- df %>%
  distinct() %>%
  count(country, admin2_gadm, crop, treatment)

crop_totals <- counts %>%
  filter(treatment == "T1") %>%
  group_by(crop) %>%
  summarise(total_crop = sum(n), .groups = "drop")
site_totals <- counts %>%
  filter(treatment == "T1") %>%
  group_by(admin2_gadm) %>%
  summarise(total_site = sum(n), .groups = "drop")

counts_labs <- counts %>%
  filter(treatment == "T1") %>%
  left_join(crop_totals, by = "crop") %>%
  left_join(site_totals, by = "admin2_gadm") %>%
  mutate(
    crop_lab = paste0(crop, " \n(N=", total_crop, ")"),
    site_lab = paste0(admin2_gadm, " \n(N=", total_site, ")")
  )

yield_summary <- df %>%
  group_by(crop, treatment, treatment2, admin2_gadm) %>%
  summarise(
    n = n(),
    mean_yield = mean(yield_tha, na.rm = TRUE),
    sd_yield = sd(yield_tha, na.rm = TRUE),
    se = sd_yield / sqrt(n),
    ci95 = 1.96 * se,
    .groups = "drop"
  )

yield_resp <- df %>%
  group_by(country, admin2_gadm, fid, crop) %>%
  mutate(
    yield_T1 = mean(yield_tha[treatment == "T1"], na.rm = TRUE),
    yield_response = yield_tha - yield_T1
  ) %>%
  ungroup() %>%
  filter(treatment != "T1") %>%
  mutate(treatment2 = paste0(treatment, " \n(", lime_tha, " t/ha)"))

resp_summary <- yield_resp %>%
  group_by(country, admin2_gadm, crop, treatment, treatment2) %>%
  summarise(
    n = n(),
    mean_resp = mean(yield_response, na.rm = TRUE),
    sd_resp = sd(yield_response, na.rm = TRUE),
    se = sd_resp / sqrt(n),
    ci95 = 1.96 * se,
    .groups = "drop"
  )

map_1 <- df |>
  dplyr::select(fid, country, admin2_gadm, lat, lng, crop) |>
  distinct() |>
  filter(!is.na(lat)) |>
  group_by(fid, country, admin2_gadm, lat, lng) |>
  summarise(
    n_crops = n(),
    crops = paste(unique(crop), collapse = " | ")
  ) |>
  ungroup() |>
  distinct()

admin2_gadm_vals <- unique(map_1$admin2_gadm)
admin2_gadm_vals <- admin2_gadm_vals[!is.na(admin2_gadm_vals)]
cols <- hcl.colors(length(unique(admin2_gadm_vals)), "Zissou 1")
pal <- colorFactor(palette = cols, domain = map_1$admin2_gadm)

respify <- function(dat) {
  if (nrow(dat) == 0 ||
    !"yield_tha" %in% names(dat) || !"lime_tha" %in% names(dat)) {
    return(
      tibble(
        lime_tha = numeric(),
        yield_tha = numeric(),
        yield_resp = numeric(),
        lower_resp = numeric(),
        upper_resp = numeric()
      )
    )
  }
  if (any(dat$lime_tha == 0, na.rm = TRUE)) {
    y0 <- dat$yield_tha[which.min(abs(dat$lime_tha - 0))]
  } else {
    o <- dat[order(dat$lime_tha), ]
    y0 <- approx(
      x = o$lime_tha,
      y = o$yield_tha,
      xout = 0,
      rule = 2
    )$y
  }
  dat %>%
    mutate(
      yield_resp = yield_tha - y0,
      lower_resp = if ("lower.CL" %in% names(dat)) {
        lower.CL - y0
      } else {
        NA_real_
      },
      upper_resp = if ("upper.CL" %in% names(dat)) {
        upper.CL - y0
      } else {
        NA_real_
      }
    )
}

all_models_df <- all_models_df %>%
  group_by(country, admin2_gadm, crop, model) %>%
  group_modify(~ respify(.x)) %>%
  ungroup()

my_theme_light <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  base_font = font_google("Poppins"),
  heading_font = font_google("Poppins"),
  primary = "#1ABC9C",
  secondary = "#F39C12",
  success = "#1871B8",
  font_scale = 1.05
)

my_theme_dark <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  base_font = font_google("Poppins"),
  heading_font = font_google("Poppins"),
  bg = "#2C3E50",
  fg = "#ECF0F1",
  primary = "#1ABC9C",
  secondary = "#F39C12",
  success = "#1871B8",
  font_scale = 1.05
)