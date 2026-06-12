# =============================================================
# 07_model_performance.R
# Model diagnostics and performance summary
#
# - Marginal & conditional R², RMSE, ICC per crop
# - Variance decomposition (site / field / residual)
# - Fixed-effect coefficient forest plot
# - Residual vs fitted diagnostic plot
#
# Outputs:
#   outputs/model_performance.csv
#   outputs/variance_decomposition.csv
#   outputs/fig_model_performance.png
#   outputs/fig_variance_decomp.png
#   outputs/fig_coef_plot.png
#   outputs/fig_residuals.png
# =============================================================

pacman::p_load(dplyr, readr, tidyr, purrr, lme4, ggplot2, extrafont)

extrafont::loadfonts(quiet = TRUE)
FONT  <- "Times New Roman"
CROPS <- c("Maize", "Beans", "Soybean", "Wheat", "Fababean")

# Consistent crop colour palette (print-safe)
crop_colors <- c(
  Maize    = "#1A3A5C",
  Beans    = "#B5451B",
  Soybean  = "#2E7D32",
  Wheat    = "#F9A825",
  Fababean = "#6A1B9A"
)

# Shared ggplot theme for academic figures
theme_academic <- function(base_size = 12) {
  theme_bw(base_size = base_size, base_family = FONT) +
    theme(
      panel.grid.minor    = element_blank(),
      panel.grid.major    = element_line(color = "grey88", linewidth = 0.35),
      strip.background    = element_rect(fill = "grey95", color = "grey60"),
      strip.text          = element_text(face = "bold", size = base_size - 1),
      axis.title          = element_text(size = base_size),
      axis.text           = element_text(size = base_size - 1),
      legend.background   = element_blank(),
      legend.key          = element_blank(),
      plot.title          = element_text(face = "bold", size = base_size + 1),
      plot.subtitle       = element_text(size = base_size - 1, color = "grey40"),
      plot.caption        = element_text(size = base_size - 2, color = "grey50",
                                         hjust = 0)
    )
}

# -----------------------------------------------------------------
# 0. Load data and refit models
# -----------------------------------------------------------------
df <- read_csv("data/data_y1_rain.csv", show_col_types = FALSE) |>
  mutate(
    fid         = as.factor(fid),
    admin2_gadm = as.factor(admin2_gadm),
    crop        = as.factor(crop)
  )

sv <- readRDS("data/selected_vars.rds")

models   <- list()
perf_lst <- list()
vc_lst   <- list()
coef_lst <- list()
resid_lst <- list()

for (cr in CROPS) {
  vars <- sv[[cr]]
  cols <- c("yield_tha", "lime_tha", "admin2_gadm", "fid", vars)
  dat  <- df[df$crop == cr, cols, drop = FALSE] |> drop_na()

  n_sites <- n_distinct(dat$admin2_gadm)
  re_term <- if (n_sites > 1) "(1 | admin2_gadm / fid)" else "(1 | fid)"
  f_lmm   <- as.formula(paste(
    "yield_tha ~ lime_tha + I(lime_tha^2) +",
    paste(vars, collapse = " + "), "+", re_term
  ))

  m <- lmer(f_lmm, data = dat, REML = TRUE,
            control = lmerControl(optimizer = "bobyqa"))
  models[[cr]] <- m

  # ── Performance metrics ───────────────────────────────────────
  vc        <- as.data.frame(VarCorr(m))
  resid_var <- sigma(m)^2
  total_var <- sum(vc$vcov) + resid_var
  icc       <- (total_var - resid_var) / total_var

  fit_marg  <- predict(m, re.form = NA)
  fit_full  <- predict(m)
  obs       <- dat$yield_tha
  ss_tot    <- sum((obs - mean(obs))^2)

  r2_marg   <- 1 - sum((obs - fit_marg)^2) / ss_tot
  r2_cond   <- 1 - sum((obs - fit_full)^2) / ss_tot
  rmse_marg <- sqrt(mean((obs - fit_marg)^2))
  mae_marg  <- mean(abs(obs - fit_marg))
  n_sites_v <- n_distinct(dat$admin2_gadm)
  n_fields  <- n_distinct(dat$fid)

  perf_lst[[cr]] <- tibble(
    crop      = cr,
    n_obs     = nrow(dat),
    n_sites   = n_sites_v,
    n_fields  = n_fields,
    r2_marg   = round(r2_marg,  3),
    r2_cond   = round(r2_cond,  3),
    rmse      = round(rmse_marg, 3),
    mae       = round(mae_marg,  3),
    icc       = round(icc,       3)
  )

  # ── Variance decomposition ────────────────────────────────────
  vc_df <- vc |>
    mutate(crop = cr, pct_var = round(100 * vcov / total_var, 1)) |>
    bind_rows(tibble(
      grp    = "Residual", var1 = NA_character_, var2 = NA_character_,
      vcov   = resid_var, sdcor = sqrt(resid_var),
      crop   = cr,
      pct_var = round(100 * resid_var / total_var, 1)
    )) |>
    select(crop, component = grp, variance = vcov, sd = sdcor, pct_var)
  vc_lst[[cr]] <- vc_df

  # ── Fixed effect coefficients ─────────────────────────────────
  fe <- as.data.frame(coef(summary(m))) |>
    tibble::rownames_to_column("term") |>
    mutate(
      crop   = cr,
      ci_lo  = Estimate - 1.96 * `Std. Error`,
      ci_hi  = Estimate + 1.96 * `Std. Error`,
      sig    = case_when(
        abs(`t value`) >= 3.29 ~ "***",
        abs(`t value`) >= 2.58 ~ "**",
        abs(`t value`) >= 1.96 ~ "*",
        TRUE                   ~ ""
      )
    ) |>
    rename(estimate = Estimate, se = `Std. Error`, t_val = `t value`)
  coef_lst[[cr]] <- fe

  # ── Residuals ─────────────────────────────────────────────────
  resid_lst[[cr]] <- tibble(
    crop    = cr,
    fitted  = fit_marg,
    resid   = obs - fit_marg,
    obs     = obs
  )
}

perf_df  <- bind_rows(perf_lst)
vc_df    <- bind_rows(vc_lst)
coef_df  <- bind_rows(coef_lst)
resid_df <- bind_rows(resid_lst)

write_csv(perf_df, "outputs/model_performance.csv")
write_csv(vc_df,   "outputs/variance_decomposition.csv")
write_csv(coef_df, "outputs/model_coefficients.csv")

cat("\n--- Model Performance ---\n")
print(perf_df)

# -----------------------------------------------------------------
# Figure 1: Model performance bar chart (R²_marg, R²_cond, ICC)
# -----------------------------------------------------------------
perf_long <- perf_df |>
  select(crop, `R² marginal` = r2_marg,
         `R² conditional` = r2_cond, ICC = icc) |>
  pivot_longer(-crop, names_to = "metric", values_to = "value") |>
  mutate(
    metric = factor(metric,
                    levels = c("R² marginal", "R² conditional", "ICC")),
    crop   = factor(crop, levels = CROPS)
  )

p_perf <- perf_long |>
  ggplot(aes(x = crop, y = value, fill = crop)) +
  geom_col(width = 0.65, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.3f", value)),
            vjust = -0.4, size = 3.2, family = FONT) +
  facet_wrap(~metric, ncol = 3) +
  scale_fill_manual(values = crop_colors) +
  scale_y_continuous(limits = c(0, 1.08), breaks = seq(0, 1, 0.25)) +
  labs(
    x       = NULL,
    y       = "Value",
    title   = "Mixed-effects model performance by crop",
    caption = paste0(
      "R² marginal = variance explained by fixed effects only. ",
      "R² conditional = fixed + random effects.\n",
      "ICC = intraclass correlation (proportion of variance between sites/fields)."
    )
  ) +
  theme_academic() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 25, hjust = 1))

ggsave("outputs/fig_model_performance.png",
       p_perf, width = 10, height = 4.5, dpi = 300)

# -----------------------------------------------------------------
# Figure 2: Variance decomposition stacked bar
# -----------------------------------------------------------------
vc_plot <- vc_df |>
  mutate(
    component = case_when(
      grepl("admin2_gadm$", component) ~ "Site",
      grepl("fid|admin2_gadm:fid", component) ~ "Field",
      component == "Residual" ~ "Residual",
      TRUE ~ component
    ),
    component = factor(component, levels = c("Site", "Field", "Residual")),
    crop = factor(crop, levels = CROPS)
  ) |>
  group_by(crop, component) |>
  summarise(pct_var = sum(pct_var), .groups = "drop")

vc_colors <- c(Site = "#1A3A5C", Field = "#5B8DB8", Residual = "#C8D8E8")

p_vc <- vc_plot |>
  ggplot(aes(x = crop, y = pct_var, fill = component)) +
  geom_col(width = 0.6, color = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(pct_var >= 5, paste0(round(pct_var), "%"), "")),
            position = position_stack(vjust = 0.5),
            size = 3.2, color = "white", family = FONT, fontface = "bold") +
  scale_fill_manual(values = vc_colors, name = "Variance\ncomponent") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    x       = NULL,
    y       = "Share of total variance (%)",
    title   = "Variance decomposition by crop",
    caption = "High between-site/field ICC indicates strong spatial heterogeneity in baseline yield."
  ) +
  theme_academic() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

ggsave("outputs/fig_variance_decomp.png",
       p_vc, width = 7, height = 4.5, dpi = 300)

# -----------------------------------------------------------------
# Figure 3: Fixed-effect forest plot (lime terms + key soil vars)
# -----------------------------------------------------------------
# Focus on lime_tha, I(lime_tha^2), p_h_BP, ex_ac_BP
key_terms <- coef_df |>
  filter(grepl("lime_tha|p_h_BP|ex_ac_BP|rainfall", term)) |>
  mutate(
    term_lab = case_when(
      term == "lime_tha"      ~ "Lime (linear)",
      term == "I(lime_tha^2)" ~ "Lime (quadratic)",
      term == "p_h_BP"        ~ "Soil pH",
      term == "ex_ac_BP"      ~ "Exchangeable acidity",
      term == "rainfall_mm"   ~ "Rainfall (mm)",
      TRUE ~ term
    ),
    term_lab = factor(term_lab, levels = rev(c(
      "Lime (linear)", "Lime (quadratic)",
      "Soil pH", "Exchangeable acidity", "Rainfall (mm)"
    ))),
    crop = factor(crop, levels = CROPS)
  )

p_coef <- key_terms |>
  ggplot(aes(x = estimate, y = term_lab, color = crop, shape = crop)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi),
                 height = 0.25, linewidth = 0.6, alpha = 0.7) +
  geom_point(size = 3, alpha = 0.9) +
  geom_text(aes(label = sig), nudge_y = 0.3, size = 3.5,
            show.legend = FALSE, family = FONT) +
  facet_wrap(~crop, ncol = 5, scales = "free_x") +
  scale_color_manual(values = crop_colors) +
  scale_shape_manual(values = c(16, 17, 15, 18, 8)) +
  labs(
    x       = "Coefficient estimate (t/ha per unit covariate)",
    y       = NULL,
    title   = "Fixed-effect estimates: lime response and key soil covariates",
    caption = "Error bars = 95% CI (Wald).  *p<0.05  **p<0.01  ***p<0.001 (|t|>1.96, 2.58, 3.29)."
  ) +
  theme_academic() +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank())

ggsave("outputs/fig_coef_plot.png",
       p_coef, width = 13, height = 5, dpi = 300)

# -----------------------------------------------------------------
# Figure 4: Residual vs Fitted (marginal, fixed-effects prediction)
# -----------------------------------------------------------------
p_resid <- resid_df |>
  mutate(crop = factor(crop, levels = CROPS)) |>
  ggplot(aes(x = fitted, y = resid, color = crop)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey40", linewidth = 0.4) +
  geom_point(alpha = 0.25, size = 0.9) +
  geom_smooth(method = "loess", se = FALSE,
              color = "black", linewidth = 0.7) +
  facet_wrap(~crop, scales = "free", ncol = 3) +
  scale_color_manual(values = crop_colors) +
  labs(
    x       = "Fitted values — marginal (t/ha)",
    y       = "Residuals (t/ha)",
    title   = "Residual diagnostics: marginal (fixed-effect) predictions",
    caption = "Loess smoother overlaid. Systematic curvature would indicate model mis-specification."
  ) +
  theme_academic() +
  theme(legend.position = "none")

ggsave("outputs/fig_residuals.png",
       p_resid, width = 11, height = 6.5, dpi = 300)

cat("Saved: fig_model_performance.png | fig_variance_decomp.png\n")
cat("       fig_coef_plot.png | fig_residuals.png\n")
