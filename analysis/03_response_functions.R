# =============================================================
# 03_response_functions.R
# Fit LMM per crop and extract yield response curves with CI
#
# Model: yield_tha ~ lime_tha + I(lime_tha^2) + [selected vars]
#                  + (1 | admin2_gadm / fid)
#
# CI method: Wald-based, propagated through yield_resp = pred(L) - pred(0)
#
# Outputs:
#   outputs/response_curves.csv
#   outputs/model_summary.csv
#   outputs/fig_response_curves.png
# =============================================================

pacman::p_load(dplyr, readr, tidyr, purrr, lme4, ggplot2, ggthemes, extrafont)


extrafont::loadfonts(quiet = TRUE)
my_font <- "Muli"

# -----------------------------------------------------------------
# 0. Load inputs
# -----------------------------------------------------------------
df <- read_csv("data/data_y1_rain.csv", show_col_types = FALSE) |>
  mutate(
    fid = as.factor(fid),
    admin2_gadm = as.factor(admin2_gadm),
    crop = as.factor(crop)
  )

selected_vars <- readRDS("data/selected_vars.rds")
CROPS <- names(selected_vars)
LIME_GRID <- seq(0, 7, by = 0.5)

# -----------------------------------------------------------------
# Helper: Wald CI for fixed-effect predictions from an LMM
# Returns a data frame with columns: lime_tha, fit, lwr, upr
# -----------------------------------------------------------------
wald_pred <- function(mod, newdat) {
  fit <- predict(mod, newdata = newdat, re.form = NA)
  X_new <- model.matrix(formula(mod, fixed.only = TRUE)[-2], data = newdat)
  V <- as.matrix(vcov(mod))
  se <- sqrt(pmax(rowSums((X_new %*% V) * X_new), 0))
  tibble(
    lime_tha = newdat$lime_tha,
    fit      = fit,
    lwr      = fit - 1.96 * se,
    upr      = fit + 1.96 * se,
    se_fit   = se
  )
}

# -----------------------------------------------------------------
# Helper: propagate CI through yield_resp(L) = pred(L) - pred(0)
# Uses full covariance matrix so Var(resp) = Var(L) + Var(0) - 2*Cov(L,0)
# -----------------------------------------------------------------
resp_ci <- function(mod, newdat) {
  X_new <- model.matrix(formula(mod, fixed.only = TRUE)[-2], data = newdat)
  V <- as.matrix(vcov(mod))
  fit <- predict(mod, newdata = newdat, re.form = NA)
  cov_mat <- X_new %*% V %*% t(X_new)

  idx0 <- which(newdat$lime_tha == 0)
  yield_resp <- fit - fit[idx0]
  var_resp <- diag(cov_mat) + cov_mat[idx0, idx0] - 2 * cov_mat[, idx0]
  se_resp <- sqrt(pmax(var_resp, 0))

  tibble(
    lime_tha       = newdat$lime_tha,
    yield_resp     = yield_resp,
    yield_resp_lo  = yield_resp - 1.96 * se_resp,
    yield_resp_hi  = yield_resp + 1.96 * se_resp
  )
}

# -----------------------------------------------------------------
# Main loop
# -----------------------------------------------------------------
curves_list <- list()
summary_list <- list()

for (cr in CROPS) {
  cat("\n====", cr, "====\n")

  vars <- selected_vars[[cr]]
  dat <- df |>
    filter(crop == cr) |>
    select(yield_tha, lime_tha, admin2_gadm, fid, all_of(vars)) |>
    drop_na()

  cat(sprintf(
    "  N = %d | sites = %d | fields = %d\n",
    nrow(dat), n_distinct(dat$admin2_gadm), n_distinct(dat$fid)
  ))

  # ── Build formula ────────────────────────────────────────────
  n_sites <- n_distinct(dat$admin2_gadm)
  re_term <- if (n_sites > 1) "(1 | admin2_gadm / fid)" else "(1 | fid)"
  f_lmm <- as.formula(paste(
    "yield_tha ~ lime_tha + I(lime_tha^2) +",
    paste(vars, collapse = " + "), "+", re_term
  ))
  cat("  Formula:", deparse(f_lmm), "\n")

  # ── Fit LMM ─────────────────────────────────────────────────
  mod <- tryCatch(
    lmer(f_lmm,
      data = dat, REML = TRUE,
      control = lmerControl(optimizer = "bobyqa")
    ),
    error = function(e) {
      message("  ERROR: ", e$message)
      NULL
    }
  )
  if (is.null(mod)) next

  # Check for convergence warnings
  if (!is.null(mod@optinfo$conv$lme4$messages)) {
    cat("  Warning: convergence issue —", mod@optinfo$conv$lme4$messages, "\n")
  }

  # ── Build prediction grid at covariate means ─────────────────
  covar_means <- dat |>
    summarise(across(all_of(vars), \(x) mean(x, na.rm = TRUE)))

  newdat <- bind_cols(
    tibble(lime_tha = LIME_GRID),
    covar_means[rep(1, length(LIME_GRID)), , drop = FALSE]
  )

  # ── Extract yield response curve with CI ────────────────────
  curve <- tryCatch(
    resp_ci(mod, newdat) |> mutate(crop = cr),
    error = function(e) {
      message("  CI error: ", e$message)
      NULL
    }
  )
  if (is.null(curve)) next
  curves_list[[cr]] <- curve

  # ── Model summary (fixed effects) ───────────────────────────
  fe <- as.data.frame(coef(summary(mod))) |>
    tibble::rownames_to_column("term") |>
    mutate(crop = cr) |>
    rename(estimate = Estimate, std_error = `Std. Error`, t_value = `t value`)

  summary_list[[cr]] <- fe

  cat(sprintf(
    "  Response at T4 (7 t/ha): %.3f t/ha [%.3f, %.3f]\n",
    curve$yield_resp[curve$lime_tha == 7],
    curve$yield_resp_lo[curve$lime_tha == 7],
    curve$yield_resp_hi[curve$lime_tha == 7]
  ))
}

# -----------------------------------------------------------------
# Save CSVs
# -----------------------------------------------------------------
curves_df <- bind_rows(curves_list) |>
  mutate(across(where(is.numeric), \(x) round(x, 4)))

summary_df <- bind_rows(summary_list) |>
  mutate(across(where(is.numeric), \(x) round(x, 4))) |>
  select(crop, term, estimate, std_error, t_value)

write_csv(curves_df, "outputs/response_curves.csv")
write_csv(summary_df, "outputs/model_summary.csv")
cat("\nSaved: outputs/response_curves.csv\n")
cat("Saved: outputs/model_summary.csv\n")

# -----------------------------------------------------------------
# Figure: yield response curves per crop with CI ribbon
# -----------------------------------------------------------------
# Observed field-level responses (for background points)
obs_resp <- df |>
  group_by(crop, admin2_gadm, fid) |>
  mutate(
    yield_T1 = mean(yield_tha[lime_tha == 0], na.rm = TRUE),
    yield_resp_obs = yield_tha - yield_T1
  ) |>
  ungroup() |>
  filter(lime_tha > 0, !is.na(yield_resp_obs))

# Observed treatment means ± SE per crop
obs_means <- obs_resp |>
  group_by(crop, lime_tha) |>
  summarise(
    mean_resp = mean(yield_resp_obs, na.rm = TRUE),
    se        = sd(yield_resp_obs, na.rm = TRUE) / sqrt(n()),
    .groups   = "drop"
  )

p <- curves_df |>
  ggplot(aes(x = lime_tha, y = yield_resp)) +
  # CI ribbon
  geom_ribbon(
    aes(ymin = yield_resp_lo, ymax = yield_resp_hi),
    fill = "#1871B8", alpha = 0.15
  ) +
  # Observed field points (jittered)
  geom_jitter(
    data = obs_resp,
    aes(x = lime_tha, y = yield_resp_obs),
    width = 0.08, alpha = 0.15, size = 0.8, color = "grey50"
  ) +
  # Observed treatment means ± SE
  geom_pointrange(
    data = obs_means,
    aes(
      x = lime_tha, y = mean_resp,
      ymin = mean_resp - 1.96 * se,
      ymax = mean_resp + 1.96 * se
    ),
    color = "#FC3400", size = 0.4, linewidth = 0.6
  ) +
  # Model curve
  geom_line(color = "#0E3065", linewidth = 1.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  facet_wrap(~crop, scales = "free_y", ncol = 3) +
  scale_x_continuous(breaks = c(0, 1, 2.5, 7)) +
  labs(
    x = "Lime rate (t/ha)",
    y = "Yield response vs control (t/ha)",
    title = "Crop-level yield response to lime",
    subtitle = "LMM: soil + rainfall covariates at means | Random: site / field\nBlue band = 95% CI (Wald) | Red = observed treatment means ± 95% CI | Grey = field observations"
  ) +
  ggthemes::theme_clean(base_size = 13, base_family = my_font) +
  theme(
    strip.background = element_rect(fill = "#d0e2f0", color = "black", linewidth = 0.4),
    strip.text = element_text(color = "#0E3065", size = 11),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.2),
    plot.subtitle = element_text(size = 9, color = "grey40")
  )

ggsave(
  "outputs/fig_response_curves.png",
  p,
  width = 12, height = 7, dpi = 300
)
cat("Saved: outputs/fig_response_curves.png\n")
