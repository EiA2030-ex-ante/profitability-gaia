# =============================================================
# 05_sensitivity.R
# Sensitivity of profitability to crop-to-lime price ratio
#
# 4a — Heatmap: profit by (lime rate × price ratio)
# 4b — Break-even price ratio curve vs lime rate
# 4c — Profitability vs price ratio at optimal lime rate
#
# Outputs:
#   outputs/sensitivity_grid.csv
#   outputs/breakeven_price_ratio.csv
#   outputs/fig_sensitivity_heatmap.png
#   outputs/fig_breakeven_price_ratio.png
#   outputs/fig_profit_vs_price_ratio.png
# =============================================================

pacman::p_load(dplyr, readr, tidyr, purrr, ggplot2, ggthemes, extrafont)

extrafont::loadfonts(quiet = TRUE)
my_font    <- "Muli"
bar_colors <- c("#0E3065", "#FFBE00", "#FC3400", "#00640D", "#8F2D56")

# -----------------------------------------------------------------
# 0. Load inputs
# -----------------------------------------------------------------
curves   <- read_csv("outputs/response_curves.csv",
                     show_col_types = FALSE)
prices   <- read_csv("outputs/crop_prices.csv",
                     show_col_types = FALSE)
optimal  <- read_csv("outputs/optimal_lime_rate.csv",
                     show_col_types = FALSE)

# Price ratio grid
RP_GRID <- seq(0.5, 10, by = 0.25)

# -----------------------------------------------------------------
# 1. Build sensitivity grid
#    profit(L, r_p) = lime_price × [yield_resp × r_p − L]
# -----------------------------------------------------------------
sensitivity <- curves |>
  filter(lime_tha > 0) |>                      # exclude control (response = 0)
  left_join(prices |> select(crop, lime_price, price_ratio_obs),
            by = "crop") |>
  expand_grid(r_p = RP_GRID) |>
  mutate(
    profit     = lime_price * (yield_resp    * r_p - lime_tha),
    profit_lo  = lime_price * (yield_resp_lo * r_p - lime_tha),
    profit_hi  = lime_price * (yield_resp_hi * r_p - lime_tha)
  )

write_csv(sensitivity, "outputs/sensitivity_grid.csv")

# -----------------------------------------------------------------
# 2. Break-even price ratio: r_p_break(L) = L / yield_resp_mean(L)
# -----------------------------------------------------------------
breakeven <- curves |>
  filter(lime_tha > 0, yield_resp > 0) |>     # can't break even if response ≤ 0
  left_join(prices |> select(crop, price_ratio_obs), by = "crop") |>
  mutate(
    r_p_break    = lime_tha / yield_resp,
    r_p_break_hi = lime_tha / yield_resp_lo,   # wider CI → higher break-even
    r_p_break_lo = lime_tha / yield_resp_hi    # narrower CI → lower break-even
  ) |>
  mutate(
    r_p_break_hi = pmin(r_p_break_hi, max(RP_GRID)),   # clip to grid range
    r_p_break_lo = pmax(r_p_break_lo, min(RP_GRID))
  )

write_csv(breakeven, "outputs/breakeven_price_ratio.csv")

# -----------------------------------------------------------------
# 4a — Heatmap: profit by (lime rate × price ratio) per crop
# -----------------------------------------------------------------

# observed price ratio labels per crop
obs_labels <- prices |>
  select(crop, price_ratio_obs) |>
  mutate(label = sprintf("Observed\nr_p = %.1f", price_ratio_obs))

p_heat <- sensitivity |>
  ggplot(aes(x = lime_tha, y = r_p, fill = profit)) +
  geom_raster(interpolate = TRUE) +
  # break-even contour (profit = 0)
  geom_contour(aes(z = profit), breaks = 0,
               color = "white", linewidth = 0.7, linetype = "dashed") +
  # observed price ratio line
  geom_hline(
    data = obs_labels,
    aes(yintercept = price_ratio_obs),
    color = "white", linewidth = 0.6, linetype = "dotted",
    inherit.aes = FALSE
  ) +
  geom_text(
    data = obs_labels,
    aes(x = 6.5, y = price_ratio_obs + 0.35, label = label),
    color = "white", size = 2.8, hjust = 1, family = my_font,
    inherit.aes = FALSE
  ) +
  facet_wrap(~crop, ncol = 3) +
  scale_x_continuous(breaks = c(1, 2.5, 4, 7)) +
  scale_fill_gradient2(
    low      = "#C0392B",
    mid      = "white",
    high     = "#1A5276",
    midpoint = 0,
    labels   = scales::dollar_format(prefix = "$"),
    name     = "Profit\n(USD/ha)"
  ) +
  labs(
    x        = "Lime rate (t/ha)",
    y        = "Price ratio  (crop price / lime price)",
    title    = "Profitability by lime rate and crop-to-lime price ratio",
    subtitle = "White dashed = break-even (profit = 0)  |  White dotted = observed price ratio"
  ) +
  ggthemes::theme_clean(base_size = 12, base_family = my_font) +
  theme(
    strip.background   = element_rect(fill = "#d0e2f0", color = "black", linewidth = 0.4),
    strip.text         = element_text(color = "#0E3065", size = 11),
    legend.position    = "right",
    panel.grid         = element_blank(),
    plot.subtitle      = element_text(size = 9, color = "grey40")
  )

ggsave("outputs/fig_sensitivity_heatmap.png",
       p_heat, width = 12, height = 7, dpi = 300)
cat("Saved: outputs/fig_sensitivity_heatmap.png\n")

# -----------------------------------------------------------------
# 4b — Break-even price ratio vs lime rate
# -----------------------------------------------------------------
p_break <- breakeven |>
  ggplot(aes(x = lime_tha, y = r_p_break, color = crop, fill = crop)) +
  geom_ribbon(aes(ymin = r_p_break_lo, ymax = r_p_break_hi),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.1) +
  # observed price ratio per crop
  geom_hline(
    data = prices,
    aes(yintercept = price_ratio_obs, color = crop),
    linewidth = 0.5, linetype = "dashed"
  ) +
  geom_text(
    data = prices,
    aes(x = 6.8, y = price_ratio_obs + 0.25,
        label = sprintf("r_p = %.1f", price_ratio_obs),
        color = crop),
    size = 3, hjust = 1, family = my_font
  ) +
  scale_x_continuous(breaks = c(1, 2.5, 4, 5.5, 7)) +
  scale_y_continuous(limits = c(0, 10)) +
  scale_color_manual(values = bar_colors) +
  scale_fill_manual(values  = bar_colors) +
  facet_wrap(~crop, ncol = 3) +
  labs(
    x        = "Lime rate (t/ha)",
    y        = "Break-even price ratio  (crop / lime)",
    title    = "Minimum price ratio needed for profitability",
    subtitle = "Solid = break-even r_p  |  Dashed = observed r_p  |  Band = 95% CI\nFields below the solid line are profitable at that lime rate"
  ) +
  ggthemes::theme_clean(base_size = 12, base_family = my_font) +
  theme(
    legend.position    = "none",
    strip.background   = element_rect(fill = "#d0e2f0", color = "black", linewidth = 0.4),
    strip.text         = element_text(color = "#0E3065", size = 11),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.2),
    plot.subtitle      = element_text(size = 9, color = "grey40")
  )

ggsave("outputs/fig_breakeven_price_ratio.png",
       p_break, width = 12, height = 7, dpi = 300)
cat("Saved: outputs/fig_breakeven_price_ratio.png\n")

# -----------------------------------------------------------------
# 4c — Profitability vs price ratio at the optimal lime rate
# -----------------------------------------------------------------
opt_sensitivity <- sensitivity |>
  inner_join(optimal |> select(crop, opt_lime_tha), by = "crop") |>
  filter(lime_tha == opt_lime_tha)

# Break-even r_p at optimal lime rate
opt_breakeven <- opt_sensitivity |>
  group_by(crop) |>
  summarise(
    r_p_break_opt = approx(profit, r_p, xout = 0, rule = 2)$y,
    .groups = "drop"
  ) |>
  left_join(prices |> select(crop, price_ratio_obs), by = "crop") |>
  left_join(optimal |> select(crop, opt_lime_tha), by = "crop")

p_opt <- opt_sensitivity |>
  ggplot(aes(x = r_p)) +
  geom_ribbon(aes(ymin = profit_lo, ymax = profit_hi),
              fill = "#1871B8", alpha = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.4) +
  geom_line(aes(y = profit), color = "#0E3065", linewidth = 1.1) +
  # break-even r_p vertical line
  geom_vline(
    data = opt_breakeven,
    aes(xintercept = r_p_break_opt),
    color = "#C0392B", linetype = "dashed", linewidth = 0.6
  ) +
  # observed price ratio vertical line
  geom_vline(
    data = prices,
    aes(xintercept = price_ratio_obs),
    color = "#00640D", linetype = "dotted", linewidth = 0.7
  ) +
  # annotations
  geom_text(
    data = opt_breakeven,
    aes(x = r_p_break_opt + 0.2,
        y = -Inf,
        label = sprintf("Break-even\nr_p = %.1f", r_p_break_opt)),
    color = "#C0392B", vjust = -0.3, hjust = 0, size = 3, family = my_font
  ) +
  geom_text(
    data = prices,
    aes(x = price_ratio_obs + 0.2,
        y = Inf,
        label = sprintf("Observed\nr_p = %.1f", price_ratio_obs)),
    color = "#00640D", vjust = 1.3, hjust = 0, size = 3, family = my_font
  ) +
  facet_wrap(~crop, scales = "free_y", ncol = 3) +
  labs(
    x        = "Price ratio  (crop price / lime price)",
    y        = "Profit at optimal lime rate (USD/ha)",
    title    = "Profitability vs price ratio at the optimal lime rate",
    subtitle = "Red dashed = break-even price ratio  |  Green dotted = observed price ratio  |  Band = 95% CI"
  ) +
  ggthemes::theme_clean(base_size = 12, base_family = my_font) +
  theme(
    strip.background   = element_rect(fill = "#d0e2f0", color = "black", linewidth = 0.4),
    strip.text         = element_text(color = "#0E3065", size = 11),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.2),
    plot.subtitle      = element_text(size = 9, color = "grey40")
  )

ggsave("outputs/fig_profit_vs_price_ratio.png",
       p_opt, width = 12, height = 7, dpi = 300)
cat("Saved: outputs/fig_profit_vs_price_ratio.png\n")

# -----------------------------------------------------------------
# Summary table: break-even price ratio at each treatment level
# -----------------------------------------------------------------
breakeven_summary <- breakeven |>
  filter(lime_tha %in% c(1, 2.5, 7)) |>
  select(crop, lime_tha, r_p_break, r_p_break_lo, r_p_break_hi) |>
  left_join(prices |> select(crop, price_ratio_obs), by = "crop") |>
  mutate(
    margin_over_obs = price_ratio_obs - r_p_break,
    profitable_at_obs = margin_over_obs > 0
  ) |>
  mutate(across(where(is.numeric), \(x) round(x, 2)))

cat("\n--- Break-even price ratio at treatment levels ---\n")
print(breakeven_summary)
