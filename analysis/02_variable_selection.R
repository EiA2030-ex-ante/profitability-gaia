# =============================================================
# 02_variable_selection.R
# Three-stage covariate screening per crop:
#   Stage 1 — Near-Zero Variance (NZV)
#   Stage 2 — LASSO-CV  (glmnet, lambda.1se, 10-fold)
#   Stage 3 — VIF check (threshold = 5, iterative drop)
#
# Lock-in: p_h_BP and ex_ac_BP are always retained.
#
# Outputs:
#   outputs/variable_selection_summary.csv
#   data/selected_vars.rds   (named list per crop)
# =============================================================

pacman::p_load(dplyr, readr, tidyr, glmnet, car, purrr)

# -----------------------------------------------------------------
# 0. Load data
# -----------------------------------------------------------------
df <- read_csv("data/data_y1_rain.csv", show_col_types = FALSE)

SOIL_VARS  <- c("p_h_BP", "ex_ac_BP", "soc_BP", "clay_BP", "cec_BP",
                "ecec_BP", "psi_BP", "sand_BP", "silt_BP", "tn_BP")
RAIN_VAR   <- "rainfall_mm"
LOCK_IN    <- c("p_h_BP", "ex_ac_BP")
CANDIDATES <- c(SOIL_VARS, RAIN_VAR)

CROPS      <- sort(unique(df$crop))
VIF_THRESH <- 5

# -----------------------------------------------------------------
# Helper: iterative VIF drop (never drops locked-in vars)
# -----------------------------------------------------------------
drop_high_vif <- function(vars, dat, thresh = VIF_THRESH) {
  remaining <- vars
  repeat {
    if (length(remaining) < 2) break
    f   <- reformulate(c("lime_tha", "I(lime_tha^2)", remaining), response = "yield_tha")
    mod <- lm(f, data = dat)
    # vif() needs >= 2 terms; single covariate can't be collinear
    if (length(remaining) == 1) break
    v   <- tryCatch(car::vif(mod), error = function(e) NULL)
    if (is.null(v)) break
    # keep only the soil/rain terms from vif output (drop lime terms)
    v_cov <- v[names(v) %in% remaining]
    if (max(v_cov, na.rm = TRUE) <= thresh) break
    # find worst offender that is NOT locked in
    worst <- names(which.max(v_cov))
    if (worst %in% LOCK_IN) {
      # skip locked-in, drop next worst among non-locked
      v_cov_free <- v_cov[!names(v_cov) %in% LOCK_IN]
      if (length(v_cov_free) == 0 || max(v_cov_free) <= thresh) break
      worst <- names(which.max(v_cov_free))
    }
    remaining <- remaining[remaining != worst]
  }
  remaining
}

# -----------------------------------------------------------------
# Main loop over crops
# -----------------------------------------------------------------
summary_rows <- list()
selected_vars <- list()

for (cr in CROPS) {
  cat("\n====", cr, "====\n")

  dat <- df |>
    filter(crop == cr) |>
    select(yield_tha, lime_tha, all_of(CANDIDATES)) |>
    drop_na()

  cat(sprintf("  N (complete cases): %d\n", nrow(dat)))

  # ── Stage 1: NZV filter ────────────────────────────────────────
  nzv_pass <- c()
  nzv_drop <- c()

  for (v in CANDIDATES) {
    vals   <- dat[[v]]
    cv     <- sd(vals, na.rm = TRUE) / abs(mean(vals, na.rm = TRUE) + 1e-9)
    pct_mode <- max(table(vals)) / length(vals)
    if (cv < 0.01 || pct_mode > 0.90) {
      nzv_drop <- c(nzv_drop, v)
    } else {
      nzv_pass <- c(nzv_pass, v)
    }
  }
  # Always keep lock-in vars even if they fail NZV
  nzv_pass <- union(nzv_pass, LOCK_IN)
  nzv_drop <- setdiff(nzv_drop, LOCK_IN)

  cat(sprintf("  Stage 1 (NZV) — dropped: %s\n",
              if (length(nzv_drop)) paste(nzv_drop, collapse = ", ") else "none"))

  # ── Stage 2: LASSO-CV ──────────────────────────────────────────
  # Build design matrix (standardised) from NZV survivors
  lasso_candidates <- nzv_pass
  fm <- reformulate(
    c("lime_tha", "I(lime_tha^2)", lasso_candidates),
    response = "yield_tha"
  )
  X_raw <- model.matrix(fm, data = dat)[, -1]   # drop intercept
  X_sc  <- scale(X_raw)
  y     <- dat$yield_tha

  set.seed(42)
  cv_fit <- cv.glmnet(X_sc, y, alpha = 1, nfolds = 10)

  coef_mat   <- coef(cv_fit, s = "lambda.1se")
  coef_df    <- data.frame(
    variable = rownames(coef_mat),
    coef     = as.numeric(coef_mat)
  ) |> filter(variable != "(Intercept)")

  # Variables selected by LASSO (non-zero coef), restricted to our candidates
  lasso_selected <- coef_df |>
    filter(variable %in% lasso_candidates, coef != 0) |>
    pull(variable)

  # Apply lock-in rule
  lasso_final <- union(lasso_selected, LOCK_IN)
  lasso_final <- intersect(lasso_final, lasso_candidates)  # can't add vars not in data

  lasso_dropped <- setdiff(lasso_candidates, lasso_final)

  cat(sprintf("  Stage 2 (LASSO) — selected: %s\n",
              paste(lasso_final, collapse = ", ")))
  cat(sprintf("  Stage 2 (LASSO) — dropped: %s\n",
              if (length(lasso_dropped)) paste(lasso_dropped, collapse = ", ") else "none"))

  # ── Stage 3: VIF check on LASSO survivors ─────────────────────
  if (length(lasso_final) >= 2) {
    vif_final <- drop_high_vif(lasso_final, dat)
  } else {
    vif_final <- lasso_final
  }

  # Final VIF values for reporting
  if (length(vif_final) >= 2) {
    f_vif   <- reformulate(c("lime_tha", "I(lime_tha^2)", vif_final), response = "yield_tha")
    vif_mod <- lm(f_vif, data = dat)
    vif_vals <- tryCatch(car::vif(vif_mod), error = function(e) rep(NA, length(vif_final)))
    vif_report <- vif_vals[names(vif_vals) %in% vif_final]
  } else {
    vif_report <- setNames(NA_real_, vif_final)
  }

  vif_dropped <- setdiff(lasso_final, vif_final)
  cat(sprintf("  Stage 3 (VIF) — dropped: %s\n",
              if (length(vif_dropped)) paste(vif_dropped, collapse = ", ") else "none"))
  cat(sprintf("  Final retained: %s\n", paste(sort(vif_final), collapse = ", ")))

  # ── Assemble per-variable summary rows ────────────────────────
  for (v in CANDIDATES) {
    lasso_coef_v <- coef_df |> filter(variable == v) |> pull(coef)
    lasso_coef_v <- if (length(lasso_coef_v) == 0) NA_real_ else lasso_coef_v

    drop_reason <- case_when(
      v %in% nzv_drop      ~ "NZV",
      v %in% lasso_dropped ~ "LASSO",
      v %in% vif_dropped   ~ "VIF",
      TRUE                  ~ NA_character_
    )

    summary_rows[[paste(cr, v, sep = "_")]] <- tibble(
      crop            = cr,
      variable        = v,
      nzv_pass        = !(v %in% nzv_drop),
      lasso_selected  = v %in% lasso_selected,
      lasso_coef      = lasso_coef_v,
      vif_value       = if (v %in% names(vif_report)) vif_report[[v]] else NA_real_,
      final_retained  = v %in% vif_final,
      drop_reason     = drop_reason
    )
  }

  selected_vars[[cr]] <- vif_final
}

# -----------------------------------------------------------------
# Save outputs
# -----------------------------------------------------------------
summary_df <- bind_rows(summary_rows) |>
  mutate(across(where(is.numeric), \(x) round(x, 4)))

write_csv(summary_df, "outputs/variable_selection_summary.csv")
saveRDS(selected_vars, "data/selected_vars.rds")

cat("\n--- Variable selection complete ---\n")
cat("Saved: outputs/variable_selection_summary.csv\n")
cat("Saved: data/selected_vars.rds\n\n")

# Quick summary print
cat("Selected variables per crop:\n")
iwalk(selected_vars, \(vars, cr) cat(sprintf("  %-10s: %s\n", cr, paste(vars, collapse = ", "))))
