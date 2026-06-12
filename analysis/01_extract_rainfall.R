# =============================================================
# 01_extract_rainfall.R
# Extract 2022 annual rainfall for each field GPS point
# from data/annual_Rainfall_1981_2024.tif
# Output: data/data_y1_rain.csv
# =============================================================

pacman::p_load(terra, dplyr, readr)

# -----------------------------------------------------------------
# 1. Load data
# -----------------------------------------------------------------
df <- read_csv("data/data_y1.csv", show_col_types = FALSE) |>
  select(-1)   # drop row-index column

# -----------------------------------------------------------------
# 2. Load raster and extract 2022 layer
# -----------------------------------------------------------------
r_stack <- rast("data/raw/annual_Rainfall_1981_2024.tif")

# Confirm layer exists
stopifnot("2022_sum_rainfall_2022" %in% names(r_stack))
r_2022 <- r_stack[["2022_sum_rainfall_2022"]]

# -----------------------------------------------------------------
# 3. Build unique GPS points (one row per fid — take first non-NA coordinate)
# -----------------------------------------------------------------
pts_df <- df |>
  filter(!is.na(lat), !is.na(lng)) |>
  group_by(fid) |>
  slice(1) |>
  ungroup() |>
  select(fid, lat, lng)

pts_vect <- vect(pts_df, geom = c("lng", "lat"), crs = "EPSG:4326")

# -----------------------------------------------------------------
# 4. Extract rainfall values
# -----------------------------------------------------------------
rain_vals <- terra::extract(r_2022, pts_vect, ID = FALSE) |>
  rename(rainfall_mm = 1)

fid_rain <- bind_cols(pts_df, rain_vals) |>
  select(fid, rainfall_mm)

# Diagnostic: report coverage
n_missing <- sum(is.na(fid_rain$rainfall_mm))
cat(sprintf(
  "Rainfall extracted for %d fields. Missing: %d (%.1f%%)\n",
  nrow(fid_rain),
  n_missing,
  100 * n_missing / nrow(fid_rain)
))
cat(sprintf(
  "Rainfall range: %.0f – %.0f mm  |  mean: %.0f mm\n",
  min(fid_rain$rainfall_mm, na.rm = TRUE),
  max(fid_rain$rainfall_mm, na.rm = TRUE),
  mean(fid_rain$rainfall_mm, na.rm = TRUE)
))

# -----------------------------------------------------------------
# 5. Join to full dataset and save
# -----------------------------------------------------------------
df_rain <- df |>
  left_join(fid_rain, by = "fid")

write_csv(df_rain, "data/data_y1_rain.csv")
cat("Saved: data/data_y1_rain.csv\n")
