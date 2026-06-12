# =============================================================
# 06_prob_profit.R
# Monte Carlo probability of profit across price ratios
#
# For each crop × lime rate × price ratio:
#   - Draw n_sim price pairs (crop & lime price uncertainty)
#   - Combine with empirical field-level yield response distribution
#   - P(profit > 0) = mean over (fields × sims) of I(profit > 0)
#
# Price uncertainty:
#   crop_price_s ~ N(r_p × lime_price, σ_c = 15%)  [truncated at 0]
#   lime_price_s ~ N(lime_price, σ_l = 10%)         [truncated at 0]
#
# Outputs:
#   outputs/prob_profit.csv
#   outputs/fig_prob_profit_lines.png
#   outputs/fig_prob_profit_heatmap.png
# =============================================================

pacman::p_load(dplyr, readr, tidyr, purrr, ggplot2, ggthemes, extrafont)

extrafont::loadfonts(quiet = TRUE)
my_font    <- "Muli"
bar_colors <- c("#0E3065", "#FFBE00", "#FC3400", "#00640D", "#8F2D56")
set.seed(42)

# -----------------------------------------------------------------
# 0. Load inputs
# -----------------------------------------------------------------
profit_yr1 <- read_csv("outputs/profitability_yr1.csv",
                       show_col_types = FALSE)
prices     <- read_csv("outputs/crop_prices.csv",
                       show_col_types = FALSE)

RP_GRID  <- seq(0.5, 10, by = 0.25)
N_SIM    <- 1000
SD_CROP  <- 0.15   # 15% CV on crop price
SD_LIME  <- 0.10   # 10% CV on lime price

LIME_RATES <- sort(unique(profit_yr1$lime_tha))   # observed treatment levels

# Treatment labels for plots
lime_labels <- setNames(
  paste0("T", seq_along(LIME_RATES) + 1, " (", LIME_RATES, " t/ha)"),
  as.character(LIME_RATES)
)

# -----------------------------------------------------------------
# Helper: P(profit > 0) for one (crop, lime_rate, r_p) cell
#
# Vectorised: profit_mat[field, sim] = Y[i] * pc[s] - L * pl[s]
# P(profit > 0) = mean(profit_mat > 0)
# -----------------------------------------------------------------
prob_positive <- function(yield_resp_vec, L, r_p, lime_price_base) {
  mu_crop <- r_p * lime_price_base
  mu_lime <- lime_price_base

  pc <- pmax(rnorm(N_SIM, mu_crop, SD_CROP * mu_crop), 0)
  pl <- pmax(rnorm(N_SIM, mu_lime, SD_LIME * mu_lime), 0)

  # Outer product: rows = fields, cols = sims
  profit_mat <- outer(yield_resp_vec, pc) -
    matrix(L * pl, nrow = length(yield_resp_vec), ncol = N_SIM, byrow = TRUE)

  mean(profit_mat > 0, na.rm = TRUE)
}

# -----------------------------------------------------------------
# 1. Compute P(profit > 0) for all cells
# -----------------------------------------------------------------
results <- list()

for (cr in unique(profit_yr1$crop)) {
  cat("Processing:", cr, "\n")

  lime_price_base <- prices |> filter(crop == cr) |> pull(lime_price)
  r_p_obs         <- prices |> filter(crop == cr) |> pull(price_ratio_obs)

  for (L in LIME_RATES) {

    yield_vec <- profit_yr1 |>
      filter(crop == cr, lime_tha == L) |>
      pull(yield_resp_obs)

    if (length(yield_vec) < 5) next   # skip cells with very few fields

    pp_vec <- map_dbl(RP_GRID, \(r_p)
      prob_positive(yield_vec, L, r_p, lime_price_base)
    )

    results[[paste(cr, L, sep = "_")]] <- tibble(
      crop        = cr,
      lime_tha    = L,
      r_p         = RP_GRID,
      prob_profit = pp_vec,
      r_p_obs     = r_p_obs
    )
  }
}

prob_df <- bind_rows(results) |>
  mutate(
    lime_lab = factor(lime_labels[as.character(lime_tha)],
                      levels = lime_labels)
  )

write_csv(prob_df |> select(-lime_lab),
          "outputs/prob_profit.csv")
cat("Saved: outputs/prob_profit.csv\n")

# P(profit > 0) at observed price ratio per crop × lime rate
prob_at_obs <- prob_df |>
  group_by(crop, lime_tha) |>
  summarise(
    prob_at_obs_rp = approx(r_p, prob_profit, xout = unique(r_p_obs), rule = 2)$y,
    r_p_obs = unique(r_p_obs),
    .groups = "drop"
  )

cat("\n--- P(profit > 0) at observed price ratio ---\n")
print(
  prob_at_obs |>
    mutate(lime_lab = lime_labels[as.character(lime_tha)]) |>
    select(crop, lime_lab, r_p_obs, prob_at_obs_rp) |>
    mutate(prob_at_obs_rp = scales::percent(prob_at_obs_rp, accuracy = 1))
)

# -----------------------------------------------------------------
# 2. Figure: P(profit > 0) vs price ratio — one line per lime rate
# -----------------------------------------------------------------

# Observed price ratio reference segment (per crop)
obs_ref <- prob_df |>
  distinct(crop, r_p_obs)

p_lines <- prob_df |>
  ggplot(aes(x = r_p, y = prob_profit, color = lime_lab, group = lime_lab)) +
  geom_vline(
    data = obs_ref,
    aes(xintercept = r_p_obs),
    color = "grey60", linetype = "dotted", linewidth = 0.6,
    inherit.aes = FALSE
  ) +
  geom_hline(yintercept = 0.5, linetype = "dashed",
             color = "grey70", linewidth = 0.35) +
  geom_line(linewidth = 1.0) +
  # dot at observed price ratio
  geom_point(
    data = prob_at_obs |>
      mutate(lime_lab = factor(lime_labels[as.character(lime_tha)],
                               levels = lime_labels)),
    aes(x = r_p_obs, y = prob_at_obs_rp, color = lime_lab),
    size = 2.5, shape = 21, fill = "white", stroke = 1.2
  ) +
  facet_wrap(~crop, ncol = 3) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25)
  ) +
  scale_color_manual(values = bar_colors) +
  labs(
    x        = "Price ratio  (crop price / lime price)",
    y        = "P(profit > 0)",
    color    = "Lime rate",
    title    = "Probability of profit across price ratios",
    subtitle = paste0(
      "Monte Carlo: n=", N_SIM, " draws | Crop price CV=", SD_CROP*100,
      "% | Lime price CV=", SD_LIME*100,
      "% | Dotted = observed price ratio | Open circle = P at observed r_p"
    )
  ) +
  ggthemes::theme_clean(base_size = 12, base_family = my_font) +
  theme(
    legend.position    = "bottom",
    strip.background   = element_rect(fill = "#d0e2f0", color = "black", linewidth = 0.4),
    strip.text         = element_text(color = "#0E3065", size = 11),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.2),
    plot.subtitle      = element_text(size = 8.5, color = "grey40")
  )

ggsave("outputs/fig_prob_profit_lines.png",
       p_lines, width = 12, height = 7, dpi = 300)
cat("Saved: outputs/fig_prob_profit_lines.png\n")

# -----------------------------------------------------------------
# 3. Figure: heatmap of P(profit > 0) by (lime rate × price ratio)
# -----------------------------------------------------------------
p_heat <- prob_df |>
  ggplot(aes(x = lime_lab, y = r_p, fill = prob_profit)) +
  geom_tile(color = "white", linewidth = 0.2) +
  # 50% contour
  geom_contour(aes(x = as.numeric(lime_lab), z = prob_profit),
               breaks = 0.5, color = "white",
               linewidth = 0.7, linetype = "dashed") +
  # observed price ratio
  geom_hline(
    data = obs_ref,
    aes(yintercept = r_p_obs),
    color = "white", linewidth = 0.6, linetype = "dotted",
    inherit.aes = FALSE
  ) +
  # P values as text in each cell
  geom_text(
    aes(label = scales::percent(prob_profit, accuracy = 1)),
    color = ifelse(prob_df$prob_profit > 0.6 | prob_df$prob_profit < 0.2,
                   "white", "grey20"),
    size = 2.5, family = my_font
  ) +
  facet_wrap(~crop, ncol = 3) +
  scale_fill_gradient2(
    low      = "#C0392B",
    mid      = "#F9E79F",
    high     = "#1A5276",
    midpoint = 0.5,
    labels   = scales::percent_format(accuracy = 1),
    name     = "P(profit\n> 0)"
  ) +
  labs(
    x        = "Lime rate",
    y        = "Price ratio  (crop price / lime price)",
    title    = "Probability of profit by lime rate and price ratio",
    subtitle = "White dashed = 50% threshold  |  White dotted = observed price ratio"
  ) +
  ggthemes::theme_clean(base_size = 12, base_family = my_font) +
  theme(
    strip.background = element_rect(fill = "#d0e2f0", color = "black", linewidth = 0.4),
    strip.text       = element_text(color = "#0E3065", size = 11),
    panel.grid       = element_blank(),
    axis.text.x      = element_text(size = 9),
    plot.subtitle    = element_text(size = 9, color = "grey40"),
    legend.position  = "right"
  )

ggsave("outputs/fig_prob_profit_heatmap.png",
       p_heat, width = 12, height = 7, dpi = 300)
cat("Saved: outputs/fig_prob_profit_heatmap.png\n")
