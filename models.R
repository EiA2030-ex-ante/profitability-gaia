# =========================================================
# Precompute yield response models (OLS, LMM, RF, XGB)
# =========================================================
library(dplyr)
library(lme4)
library(randomForest)
library(emmeans)
library(xgboost)
library(purrr)
library(readr)
library(yardstick)
library(tidyr)

# =========================================================
# 1. Load data
# =========================================================
df <- read_csv("tmp/data_y1.csv") |>
  select(-1) |>
  filter(!is.na(yield_tha)) |>
  mutate(
    fid = as.factor(fid),
    admin2_gadm = as.factor(admin2_gadm),
    country = as.factor(country),
    crop = as.factor(crop)
  ) |>
  drop_na()

# =========================================================
# 2. Identify soil variables
# =========================================================
# Use your actual soil variables here
soil_vars <- c("p_h_BP", "soc_BP", "clay_BP", "cec_BP", "ecec_BP", "psi_BP") # adjust if needed
soil_vars <- intersect(soil_vars, names(df)) # ensure they exist

lime_grid <- tibble(lime_tha = seq(0, 7, 1))

# =========================================================
# 3. Helper functions
# =========================================================

# Function to compute yield response relative to control (lime=0)
respify <- function(out) {
  y0 <- out$fit[which.min(abs(out$lime_tha - 0))]
  out |>
    mutate(
      yield_tha = fit,
      yield_resp = fit - y0,
      lower_resp = if ("lwr" %in% names(out)) lwr - y0 else NA,
      upper_resp = if ("upr" %in% names(out)) upr - y0 else NA
    )
}

# Function to compute R² and RMSE
calc_metrics <- function(obs, pred) {
  tibble(
    RMSE = sqrt(mean((pred - obs)^2, na.rm = TRUE)),
    R2 = 1 - sum((pred - obs)^2, na.rm = TRUE) / sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
  )
}

# =========================================================
# 4. Fit models for each crop × site × country
# =========================================================
results <- list()
metrics <- list()

grouped <- df |>
  group_by(country, admin2_gadm, crop) |>
  group_split()

for (dat in grouped) {
  #dat <- grouped[[1]]
  key <- paste(dat$country[1], dat$admin2_gadm[1], dat$crop[1], sep = "_")
  cat("Processing:", key, "\n")

  if (nrow(dat) < 10) next

  X_soil <- dat[, soil_vars, drop = FALSE]
  lime <- dat$lime_tha
  y <- dat$yield_tha

  # ------------------------
  # OLS (quadratic response)
  # ------------------------
  mod_ols <- lm(yield_tha ~ lime_tha + I(lime_tha^2), data = dat)
  pred_ols <- predict(mod_ols, newdata = lime_grid, interval = "confidence")
  df_ols <- cbind(lime_grid, as.data.frame(pred_ols)) |>
    respify() |>
    mutate(model = "ols")

  met_ols <- calc_metrics(dat$yield_tha, predict(mod_ols, dat))

  # ------------------------
  # Mixed effects model
  # ------------------------
  mod_lmm <- try(lmer(yield_tha ~ lime_tha + I(lime_tha^2) + (1 | fid), data = dat, REML = TRUE), silent = TRUE)
  
  if (inherits(mod_lmm, "try-error")) {
    df_lmm <- NULL
    met_lmm <- tibble(RMSE = NA, R2 = NA)
  } else {
    # Compute estimated marginal means (EMMs) at each lime rate
    emm <- emmeans(mod_lmm, ~ lime_tha, at = list(lime_tha = lime_grid$lime_tha))
    emm_df <- as.data.frame(emm)
    
    df_lmm <- emm_df %>%
      rename(fit = emmean, lwr = lower.CL, upr = upper.CL) %>%
      select(lime_tha, fit, lwr, upr) %>%
      respify() %>%
      mutate(model = "lmm")
    
    # Evaluate model performance (on original data)
    met_lmm <- calc_metrics(dat$yield_tha, predict(mod_lmm, dat, re.form = NA))
  }
  # ------------------------
  # Random Forest (with soil)
  # ------------------------
  df_rfdata <- dat %>%
    select(yield_tha, lime_tha, all_of(soil_vars)) %>%
    na.omit()
  
  if (nrow(df_rfdata) > 10) {
    # Fit model
    mod_rf <- randomForest(yield_tha ~ ., data = df_rfdata, ntree = 500)
    
    # Create prediction grid using mean soil values
    soil_means <- df_rfdata %>%
      summarise(across(all_of(soil_vars), mean, na.rm = TRUE))
    
    newdat_rf <- cbind(lime_grid, soil_means[rep(1, nrow(lime_grid)), , drop = FALSE])
    
    pred_rf <- predict(mod_rf, newdata = newdat_rf)
    
    df_rf <- lime_grid %>%
      mutate(fit = pred_rf, lwr = NA_real_, upr = NA_real_) %>%
      respify() %>%
      mutate(model = "rf")
    
    met_rf <- calc_metrics(df_rfdata$yield_tha, predict(mod_rf, df_rfdata))
  } else {
    df_rf <- NULL
    met_rf <- tibble(RMSE = NA, R2 = NA)
  }

  # ------------------------
  # XGBoost (with soil)
  # ------------------------
  df_xgbdata <- dat %>%
    select(yield_tha, lime_tha, all_of(soil_vars)) %>%
    na.omit()
  
  if (nrow(df_xgbdata) > 10) {
    # Prepare matrices
    X <- as.matrix(df_xgbdata %>% select(-yield_tha))
    y <- df_xgbdata$yield_tha
    
    # Train model
    mod_xgb <- xgboost(
      data = X,
      label = y,
      nrounds = 300,
      eta = 0.08,
      max_depth = 6,
      subsample = 0.8,
      colsample_bytree = 0.8,
      objective = "reg:squarederror",
      verbose = 0
    )
    
    # Create prediction grid with mean soil properties
    soil_means <- df_xgbdata %>%
      summarise(across(all_of(soil_vars), mean, na.rm = TRUE))
    
    newdat_xgb <- cbind(lime_grid, soil_means[rep(1, nrow(lime_grid)), , drop = FALSE])
    X_pred <- as.matrix(newdat_xgb)
    
    # Predict
    pred_xgb <- predict(mod_xgb, newdata = X_pred)
    
    # Build response data
    df_xgb <- lime_grid %>%
      mutate(fit = pred_xgb, lwr = NA_real_, upr = NA_real_) %>%
      respify() %>%
      mutate(model = "xgb")
    
    # Compute metrics
    y_pred_train <- predict(mod_xgb, X)
    met_xgb <- calc_metrics(y, y_pred_train)
  } else {
    df_xgb <- NULL
    met_xgb <- tibble(RMSE = NA, R2 = NA)
  }
  # ------------------------
  # Combine results
  # ------------------------
  combined <- bind_rows(df_ols, df_lmm, df_rf, df_xgb) |>
    mutate(
      country = dat$country[1],
      admin2_gadm = dat$admin2_gadm[1],
      crop = dat$crop[1]
    )

  results[[key]] <- combined

  metrics[[key]] <- tibble(
    country = dat$country[1],
    admin2_gadm = dat$admin2_gadm[1],
    crop = dat$crop[1],
    model = c("ols", "lmm", "rf", "xgb"),
    RMSE = c(met_ols$RMSE, met_lmm$RMSE, met_rf$RMSE, met_xgb$RMSE),
    R2 = c(met_ols$R2, met_lmm$R2, met_rf$R2, met_xgb$R2)
  )
}

# =========================================================
# 5. Save results
# =========================================================
yield_predictions_all_models <- bind_rows(results)
model_performance_summary <- bind_rows(metrics)

write_csv(yield_predictions_all_models, "tmp/yield_predictions_all_models.csv")
write_csv(model_performance_summary, "tmp/model_performance_summary.csv")
saveRDS(
  list(
    predictions = yield_predictions_all_models,
    metrics = model_performance_summary
  ),
  "tmp/yield_models_bundle.rds"
)