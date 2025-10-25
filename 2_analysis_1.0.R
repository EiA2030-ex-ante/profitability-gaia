pacman::p_load(
  brms,
  tidyverse,
  tidybayes,
  lme4,
  emmeans,
  ggthemes,
  viridis,
  plotly,
  janitor,
  readr,
  here,
  leaflet,
  randomForest,
  xgboost
)

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


color_plasma <- viridis::viridis(10, option = "D")
cols <- hcl.colors(10, "Zissou 1")

extrafont::loadfonts(quiet = F)
my_font_2 <- "Muli"
my_font <- "Frutiger"

#--------- 1. Data ------------------------------------------------------------
df <- readr::read_csv("tmp/data_y1.csv") |>
  select(-1) |>
  distinct() |>
  mutate(
    crop = as.factor(crop),
    country = as.factor(country),
    treatment = as.factor(treatment),
    lime_factor = factor(lime_tha)
  ) |>
  drop_na(yield_tha, lime_tha, fid, crop, country) |>
  mutate(treatment2 = paste0(treatment, " \n(", lime_tha, " t/ha)"))


# --- Descriptive summaries ---

# 1) Counts per country × site × crop × treatment
counts <- df %>%
  distinct() %>%
  count(country, admin2_gadm, crop, treatment)

# 1.1) heatmap of counts
# --- add totals per crop and per site ---
crop_totals <- counts %>%
  filter(treatment == "T1") %>%
  group_by(crop) %>%
  summarise(total_crop = sum(n), .groups = "drop")

site_totals <- counts %>%
  filter(treatment == "T1") %>%
  group_by(admin2_gadm) %>%
  summarise(total_site = sum(n), .groups = "drop")

# --- join labels ---
counts_labs <- counts %>%
  filter(treatment == "T1") %>%
  left_join(crop_totals, by = "crop") %>%
  left_join(site_totals, by = "admin2_gadm") %>%
  mutate(
    crop_lab = paste0(crop, " \n(N=", total_crop, ")"),
    site_lab = paste0(admin2_gadm, " \n(N=", total_site, ")")
  )

# --- plot with new labels ---
p_counts <- counts_labs %>%
  ggplot(aes(x = crop_lab, y = site_lab, fill = n)) +
  geom_tile(color = "white", width = 0.5) +
  geom_text(aes(label = n),
    color = "gold",
    size = 5,
    family = my_font
  ) +
  # scale_fill_gradient(low = "lightyellow", high = "gold") +
  # scale_fill_gradientn(colors = cols) +
  scale_fill_gradient2() +
  labs(
    title = "Number of farmer fields by Site × Crop",
    x = "Crop (total N)",
    y = "Sites (total N)",
    fill = "Observations"
  ) +
  ggthemes::theme_pander(base_size = 16, base_family = my_font) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16, face = "plain")
  )

p_counts

ggsave(
  "figures/observation_counts_heatmap.png",
  p_counts,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)
# plotly::ggplotly(p_counts)

# 2) Yield summaries by crop × treatment
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

# --- Plots ---

# 1) Boxplot of yields by treatment, faceted by crop
p_box <- df %>%
  ggplot(aes(x = treatment2, y = yield_tha, fill = admin2_gadm)) +
  geom_boxplot(
    alpha = 0.7,
    width = 0.6,
    staplewidth = 0.4,
    outliers = F,
    coef = 1.5,
    linewidth = 0.1
  ) +
  facet_wrap(~crop, scales = "free_y", ncol = 2) +
  labs(
    x = "",
    y = "Yield (t/ha)",
    title = "Yield distributions by treatment",
    fill = ""
  ) +
  scale_fill_manual(values = bar_colors) +
  guides(fill = guide_legend(ncol = 2), shape = guide_legend(ncol = 2)) +
  ggthemes::theme_clean(base_size = 18, base_family = my_font) +
  theme(
    legend.position = c(0.8, 0.05),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.background = element_rect(fill = NA, color = NA),
    # legend font
    legend.text = element_text(size = 12, family = my_font),
    plot.title = element_text(hjust = 0.5, size = 18, face = "plain"),
    strip.background = element_rect(fill = alpha("#1871B8", 0.1), color = "white"),
    panel.grid.major.y = element_line(linewidth = 0.1)
  )
p_box
plotly::ggplotly(p_box)

ggsave(
  "figures/yield_box_plot.png",
  p_box,
  width = 8,
  height = 8,
  units = "in",
  dpi = 300
)

# 2) Mean ± 95% CI plot
p_means <- yield_summary %>%
  ggplot(aes(
    x = treatment2,
    y = mean_yield,
    color = admin2_gadm,
    group = admin2_gadm
  )) +
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  geom_errorbar(
    aes(ymin = mean_yield - ci95, ymax = mean_yield + ci95),
    width = 0.1,
    position = position_dodge(width = 0.4)
  ) +
  facet_wrap(~crop, scales = "free_y", ncol = 2) +
  scale_color_manual(values = bar_colors) +
  guides(color = guide_legend(ncol = 2)) +
  labs(
    x = "",
    y = "Mean yield (t/ha)",
    title = "Mean yield ± 95% CI by treatment",
    color = ""
  ) +
  ggthemes::theme_clean(base_size = 18, base_family = my_font) +
  theme(
    legend.position = c(0.8, 0.05),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.background = element_rect(fill = NA, color = NA),
    # legend font
    legend.text = element_text(size = 12, family = my_font),
    plot.title = element_text(hjust = 0.5, size = 18, face = "plain"),
    strip.background = element_rect(fill = alpha("#1871B8", 0.1), color = "white"),
    panel.grid.major.y = element_line(linewidth = 0.1)
  )

p_means
plotly::ggplotly(p_means)

ggsave(
  "figures/yield_means_plot.png",
  p_means,
  width = 8,
  height = 8,
  units = "in",
  dpi = 300
)

# 3) Scatter + smoother (yield vs lime)
p_scatter <- df %>%
  ggplot(aes(x = lime_tha, y = yield_tha, color = admin2_gadm)) +
  # geom_point(alpha = 0.4) +
  geom_smooth(
    method = "loess",
    se = F,
    linewidth = 1
  ) +
  facet_wrap(~crop, scales = "free_y", ncol = 2) +
  scale_color_manual(values = bar_colors) +
  guides(color = guide_legend(ncol = 2)) +
  labs(
    x = "Lime rate (t/ha)",
    y = "Yield (t/ha)",
    title = "Yield response to lime rate (loess)",
    color = ""
  ) +
  ggthemes::theme_clean(base_size = 18, base_family = my_font) +
  theme(
    legend.position = c(0.8, 0.05),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.background = element_rect(fill = NA, color = NA),
    # legend font
    legend.text = element_text(size = 12, family = my_font),
    plot.title = element_text(hjust = 0.5, size = 18, face = "plain"),
    strip.background = element_rect(fill = alpha("#1871B8", 0.1), color = "white"),
    panel.grid.major.y = element_line(linewidth = 0.1)
  )

p_scatter

ggsave(
  "figures/yield_scatter_plot.png",
  p_scatter,
  width = 8,
  height = 8,
  units = "in",
  dpi = 300
)

# 4) Yield respose plots
# --- compute yield response relative to T1 ---
yield_resp <- df %>%
  group_by(country, admin2_gadm, fid, crop) %>%
  mutate(
    yield_T1 = mean(yield_tha[treatment == "T1"], na.rm = TRUE),
    yield_response = yield_tha - yield_T1
  ) %>%
  ungroup() %>%
  filter(treatment != "T1") %>% # exclude T1 (response = 0 by definition))
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

# 1) Boxplot of yield response
p_box_resp <- yield_resp %>%
  ggplot(aes(x = treatment2, y = yield_response, fill = admin2_gadm)) +
  geom_boxplot(
    alpha = 0.9,
    width = 0.5,
    staplewidth = 0.2,
    outliers = FALSE,
    coef = 1.5,
    linewidth = 0.1
  ) +
  facet_wrap(~crop, scales = "free_y", ncol = 2) +
  guides(fill = guide_legend(ncol = 2), shape = guide_legend(ncol = 2)) +
  labs(
    x = "",
    y = "Yield response (t/ha)",
    title = "Yield response distributions by treatment and site",
    fill = ""
  ) +
  scale_fill_manual(values = bar_colors) +
  ggthemes::theme_clean(base_size = 18, base_family = my_font) +
  theme(
    legend.position = c(0.8, 0.05),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.background = element_rect(fill = NA, color = NA),
    # legend font
    legend.text = element_text(size = 12, family = my_font),
    plot.title = element_text(hjust = 0.5, size = 18, face = "plain"),
    strip.background = element_rect(fill = alpha("#1871B8", 0.1), color = "white"),
    panel.grid.major.y = element_line(linewidth = 0.1)
  )

p_box_resp
plotly::ggplotly(p_box_resp)
ggsave(
  "figures/yield_response_box_plot.png",
  p_box_resp,
  width = 8,
  height = 8,
  units = "in",
  dpi = 400
)


# --- Non-parametric stats on yield response by treatment, grouped by site only for maize

maize_resp <- yield_resp %>%
  filter(crop == "Maize")

ggstatsplot::grouped_ggbetweenstats(
  data = maize_resp,
  x = treatment2,
  y = yield_response,
  grouping.var = admin2_gadm,
  p.adjust.method = "bonferroni",
  pairwise.comparisons = TRUE,
  p.adjust.method.pairwise = "bonferroni",
  conf.level = 0.95,
  nboot = 1000,
  type = "np",
  results.subtitle = FALSE
)

# beans
beans_resp <- yield_resp %>%
  filter(crop == "Beans")
ggstatsplot::grouped_ggbetweenstats(
  data = beans_resp,
  x = treatment2,
  y = yield_response,
  grouping.var = admin2_gadm,
  p.adjust.method = "bonferroni",
  pairwise.comparisons = TRUE,
  p.adjust.method.pairwise = "bonferroni",
  conf.level = 0.95,
  nboot = 1000,
  type = "np",
  results.subtitle = TRUE
)



ggstatsplot::grouped_ggbetweenstats(
  data = yield_resp,
  x = treatment2,
  y = yield_response,
  grouping.var = crop,
  p.adjust.method = "bonferroni",
  pairwise.comparisons = TRUE,
  p.adjust.method.pairwise = "bonferroni",
  conf.level = 0.95,
  nboot = 1000,
  type = "np",
  results.subtitle = FALSE,
  xlab = "",
  ylab = "Yield response (t/ha)",
  ggtheme = ggthemes::theme_clean(base_size = 16, base_family = my_font)
)


# 2) Mean ± 95% CI plot for yield response
p_means_resp <- resp_summary %>%
  ggplot(aes(
    x = treatment2,
    y = mean_resp,
    color = admin2_gadm,
    group = admin2_gadm
  )) +
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  geom_errorbar(
    aes(ymin = mean_resp - ci95, ymax = mean_resp + ci95),
    width = 0.1,
    position = position_dodge(width = 0.4)
  ) +
  facet_wrap(~crop, scales = "free_y", ncol = 2) +
  guides(color = guide_legend(ncol = 2), shape = guide_legend(ncol = 2)) +
  scale_color_manual(values = bar_colors) +
  labs(
    x = "",
    y = "Mean yield response (t/ha)",
    title = "Mean yield response ± 95% CI by treatment",
    color = ""
  ) +
  theme_minimal(base_size = 18, base_family = my_font) +
  theme(
    legend.position = c(0.8, 0.05),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.background = element_rect(fill = NA, color = NA),
    # legend font
    legend.text = element_text(size = 12, family = my_font),
    plot.title = element_text(hjust = 0.5, size = 18, face = "plain"),
    strip.background = element_rect(fill = alpha("#1871B8", 0.1), color = "white")
  )

p_means_resp
plotly::ggplotly(p_means_resp)
ggsave(
  "figures/yield_response_means_plot.png",
  p_means_resp,
  width = 8,
  height = 8,
  units = "in",
  dpi = 300
)

htmlwidgets::saveWidget(plotly::ggplotly(p_means_resp),
                        "figures/mean_yield_response.html",
                        selfcontained = TRUE)

# for cerial crops only

p_means_resp_maize_wheat <- resp_summary %>%
  filter(crop %in% c("Maize", "Wheat")) %>%
  ggplot(aes(
    x = treatment2,
    y = mean_resp,
    color = admin2_gadm,
    group = admin2_gadm
  )) +
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  geom_errorbar(
    aes(ymin = mean_resp - ci95, ymax = mean_resp + ci95),
    width = 0.2,
    position = position_dodge(width = 0.4)
  ) +
  facet_wrap(~crop, scales = "free_y", ncol = 2) +
  guides(color = guide_legend(nrow = 1), shape = guide_legend(ncol = 2)) +
  scale_color_manual(values = bar_colors) +
  labs(
    x = "",
    y = "Mean yield response (t/ha)",
    title = "Mean yield response ± 95% CI by treatment",
    color = ""
  ) +
  ggthemes::theme_clean(base_size = 18, base_family = my_font) +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = NA, color = NA),
    # legend font
    legend.text = element_text(size = 11, family = my_font),
    plot.title = element_text(hjust = 0.5, size = 18, face = "plain"),
    strip.background = element_rect(fill = alpha("#1871B8", 0.1), color = "white"),
    panel.grid.major.y = element_line(linewidth = 0.1)
  )
p_means_resp_maize_wheat

ggsave(
  "figures/yield_response_means_plot_maize_wheat.png",
  p_means_resp_maize_wheat,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)

p_means_resp_beans <- resp_summary %>%
  filter(!crop %in% c("Maize", "Wheat")) %>%
  ggplot(aes(
    x = treatment2,
    y = mean_resp,
    color = admin2_gadm,
    group = admin2_gadm
  )) +
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  geom_errorbar(
    aes(ymin = mean_resp - ci95, ymax = mean_resp + ci95),
    width = 0.1,
    position = position_dodge(width = 0.4)
  ) +
  facet_wrap(~crop, scales = "free_y", ncol = 2) +
  guides(color = guide_legend(ncol = 2), shape = guide_legend(ncol = 2)) +
  scale_color_manual(values = bar_colors) +
  labs(
    x = "",
    y = "Mean yield response (t/ha)",
    title = "Mean yield response ± 95% CI by treatment",
    color = ""
  ) +
  ggthemes::theme_clean(base_size = 18, base_family = my_font) +
  theme(
    legend.position = c(0.8, 0.05),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.background = element_rect(fill = NA, color = NA),
    # legend font
    legend.text = element_text(size = 11, family = my_font),
    plot.title = element_text(hjust = 0.5, size = 18, face = "plain"),
    strip.background = element_rect(fill = alpha("#1871B8", 0.1), color = "white"),
    panel.grid.major.y = element_line(linewidth = 0.1)
  )
p_means_resp_beans
ggsave(
  "figures/yield_response_means_plot_beans.png",
  p_means_resp_beans,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)

library(lme4)
library(emmeans)
shape_vals <- c(16, 17, 15, 11, 7, 8, 18, 4)
# 16 = filled circle, 17 = filled triangle, 15 = filled square, 3 = plus, etc.

mod_resp <- lmer(
  yield_response ~ treatment * crop * admin2_gadm +
    (1 | fid),
  data = yield_resp,
  REML = TRUE
)

emm_tbl <- emmeans(mod_resp, ~ treatment | crop * admin2_gadm)
emm_df <- as.data.frame(emm_tbl) |>
  mutate(
    treatment2 = case_when(
      treatment == "T2" ~ "T2 \n(1 t/ha)",
      treatment == "T3" ~ "T3 \n(2.5 t/ha)",
      treatment == "T4" ~ "T4 \n(7 t/ha)"
    )
  )
p_emm <- emm_df %>%
  ggplot(aes(
    x = treatment2,
    y = emmean,
    color = admin2_gadm,
    shape = admin2_gadm
  )) +
  geom_point(position = position_dodge(width = 0.4), size = 2.5) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.3,
    position = position_dodge(width = 0.4)
  ) +
  facet_wrap(~crop, scales = "free_y", ncol = 2) +
  # combine color + shape legends into one
  guides(color = guide_legend(nrow = 4), shape = guide_legend(nrow = 3)) +
  scale_color_manual(values = bar_colors) +
  scale_shape_manual(values = shape_vals) +
  labs(
    x = "",
    y = "Adjusted mean yield response (t/ha)",
    title = "Predicted means by crop × site × treatment",
    color = "",
    shape = ""
  ) +
  ggthemes::theme_clean(base_size = 18, base_family = my_font) +
  theme(
    legend.position = c(0.75, 0.13),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.background = element_rect(fill = NA, color = NA),
    legend.text = element_text(size = 12, family = my_font),
    plot.title = element_text(hjust = 0.5, size = 18, face = "plain"),
    strip.background = element_rect(fill = alpha("#1871B8", 0.1), color = "white"),
    panel.grid.major.y = element_line(linewidth = 0.1)
  )

p_emm
ggsave(
  "figures/yield_response_emmeans_plot.png",
  p_emm,
  width = 8,
  height = 8,
  units = "in",
  dpi = 300
)

# responce curve
mod_resp_curve <- lmer(
  yield_tha ~ (lime_tha + I(lime_tha^2)) * crop * admin2_gadm +
    (1 | fid),
  data = df,
  REML = TRUE
)
emm_curve_by <- emmeans(
  mod_resp_curve,
  specs = c("lime_tha", "crop", "admin2_gadm"),
  at = list(lime_tha = seq(0, 7, 1))
) %>%
  as.data.frame()

# 3) Plot response curves
p_curve_by <- emm_curve_by %>%
  ggplot(aes(
    x = lime_tha,
    y = emmean,
    color = admin2_gadm,
    group = admin2_gadm
  )) +
  geom_line(linewidth = 1) +
  geom_ribbon(
    aes(ymin = lower.CL, ymax = upper.CL, fill = admin2_gadm),
    alpha = 0.15,
    colour = NA
  ) +
  facet_wrap(~crop, scales = "free_y") +
  guides(color = guide_legend(nrow = 4)) +
  scale_color_manual(values = bar_colors) +
  scale_fill_manual(values = bar_colors) +
  labs(
    x = "Lime rate (t/ha)",
    y = "Yield (t/ha)",
    title = "Yield curves by crop × site (mixed model)",
    color = "",
    fill = ""
  ) +
  ggthemes::theme_clean(base_size = 18, base_family = my_font) +
  theme(
    legend.position = c(0.85, 0.15),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.background = element_rect(fill = NA, color = NA),
    legend.text = element_text(size = 12, family = my_font),
    plot.title = element_text(hjust = 0.5, size = 18, face = "plain"),
    strip.background = element_rect(fill = alpha("#1871B8", 0.1), color = "white"),
    panel.grid.major.y = element_line(linewidth = 0.1)
  )

p_curve_by

# yield response curve

emm_resp <- emm_curve_by %>%
  group_by(crop, admin2_gadm) %>%
  mutate(
    base_yield = emmean[lime_tha == 0],
    yield_response = emmean - base_yield,
    yield_response_lower = lower.CL - base_yield,
    yield_response_upper = upper.CL - base_yield
  ) %>%
  ungroup()

ggplot(
  emm_resp,
  aes(
    x = lime_tha,
    y = yield_response,
    color = admin2_gadm,
    group = admin2_gadm
  )
) +
  geom_line(size = 1) +
  facet_wrap(~crop, scales = "free_y") +
  labs(
    x = "Lime rate (t/ha)",
    y = "Predicted yield response (t/ha vs T1)",
    title = "Yield response curves by crop × site",
    color = "Site",
    fill = "Site"
  ) +
  theme_bw(base_size = 14)
response_curve_by <- emm_resp %>%
  filter(lime_tha > 0) %>% # exclude zero lime (response = 0 by definition)
  filter(!is.na(emmean)) %>%
  ggplot(aes(
    x = lime_tha,
    y = yield_response,
    color = admin2_gadm,
    group = admin2_gadm
  )) +
  geom_line(linewidth = 1) +
  # geom_ribbon(aes(ymin = yield_response_lower, ymax = yield_response_upper, fill = admin2_gadm),alpha = 0.15, colour = NA) +
  facet_wrap(~crop, scales = "free_y") +
  guides(color = guide_legend(nrow = 4)) +
  scale_color_manual(values = bar_colors) +
  scale_fill_manual(values = bar_colors) +
  labs(
    x = "Lime rate (t/ha)",
    y = "Yield (t/ha)",
    title = "Yield response curves by crop × site (mixed model)",
    color = "",
    fill = ""
  ) +
  ggthemes::theme_clean(base_size = 18, base_family = my_font) +
  theme(
    legend.position = c(0.85, 0.15),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.background = element_rect(fill = NA, color = NA),
    legend.text = element_text(size = 12, family = my_font),
    plot.title = element_text(hjust = 0.5, size = 18, face = "plain"),
    strip.background = element_rect(fill = alpha("#1871B8", 0.1), color = "white"),
    panel.grid.major.y = element_line(linewidth = 0.1)
  )

response_curve_by


emm_mg <- emm_curve_by %>%
  group_by(crop, admin2_gadm) %>%
  arrange(lime_tha) %>%
  mutate(
    marginal_gain = c(NA, diff(emmean)),
    # Δy between successive lime levels
    step = lime_tha # keep same x-axis (lime rate)
  ) %>%
  ungroup()

p_mg <- emm_mg %>%
  ggplot(aes(
    x = step,
    y = marginal_gain,
    color = admin2_gadm,
    group = admin2_gadm
  )) +
  geom_line(size = 1) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey50"
  ) +
  guides(color = guide_legend(nrow = 4)) +
  scale_color_manual(values = bar_colors) +
  facet_wrap(~crop, scales = "free_y") +
  labs(
    x = "Lime rate (t/ha)",
    y = "Marginal yield gain (t/ha per extra t lime)",
    title = "Marginal gain curves by crop × site",
    color = ""
  ) +
  ggthemes::theme_clean(base_size = 18, base_family = my_font) +
  theme(
    legend.position = c(0.85, 0.15),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.background = element_rect(fill = NA, color = NA),
    legend.text = element_text(size = 12, family = my_font),
    plot.title = element_text(hjust = 0.5, size = 18, face = "plain"),
    strip.background = element_rect(fill = alpha("#1871B8", 0.1), color = "white"),
    panel.grid.major.y = element_line(linewidth = 0.1)
  )

p_mg

# marginal gain = derivative wrt lime_tha
emm_mg <- emtrends(
  mod_resp_curve,
  ~ crop * admin2_gadm | lime_tha,
  var = "lime_tha",
  at = list(lime_tha = seq(0, 8, 0.5)),
  re.form = NULL,
  cov.reduce = FALSE
) %>%
  as.data.frame()

ggplot(
  emm_mg,
  aes(
    x = lime_tha,
    y = lime_tha.trend,
    color = admin2_gadm,
    group = admin2_gadm
  )
) +
  geom_line(size = 1) +
  geom_ribbon(
    aes(ymin = lower.CL, ymax = upper.CL, fill = admin2_gadm),
    alpha = 0.15,
    colour = NA
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey50"
  ) +
  facet_wrap(~crop, scales = "free_y") +
  labs(
    x = "Lime rate (t/ha)",
    y = "Marginal yield gain (t/ha per extra t lime)",
    title = "Marginal gain curves by crop × site",
    color = "Site",
    fill = "Site"
  ) +
  theme_bw(base_size = 14)

df2 <- df %>%
  mutate(lime_tha2 = lime_tha^2) %>%
  filter(crop %in% c("Maize", "Wheat")) # focus on cereals

# --- 2) Fit mixed model (site + field random effects) ---
mod <- lmer(
  yield_tha ~ lime_tha + lime_tha2 + crop +
    (1 | admin2_gadm) + (1 | fid),
  data = df2,
  REML = TRUE
)

# --- 3) Site × crop predicted curves ---
site_curves <- df2 %>%
  group_by(crop, admin2_gadm) %>%
  do({
    newdat <- data.frame(lime_tha = seq(min(.$lime_tha), max(.$lime_tha), length.out = 10))
    newdat$lime_tha2 <- newdat$lime_tha^2
    newdat$crop <- unique(.$crop)
    newdat$admin2_gadm <- unique(.$admin2_gadm)

    # include RE for site
    newdat$pred <- predict(
      mod,
      newdata = newdat,
      re.form = ~ (1 |
        admin2_gadm),
      allow.new.levels = TRUE
    )
    newdat
  }) %>%
  ungroup()

# --- 4) Pooled (fixed effects only) curve ---
pooled_curve <- data.frame(
  lime_tha = seq(min(df2$lime_tha), max(df2$lime_tha), length.out = 10),
  crop = "Maize" # adjust if you want per-crop pooled
) %>%
  mutate(lime_tha2 = lime_tha^2)
pooled_curve$pred <- predict(mod, newdata = pooled_curve, re.form = NA)

# --- 5) Observed means (points) ---
site_means <- df2 %>%
  group_by(crop, admin2_gadm, lime_tha) %>%
  summarise(
    mean_yield = mean(yield_tha, na.rm = TRUE),
    .groups = "drop"
  )

# --- 6) Plot ---
ggplot() +
  # site means as points
  geom_point(
    data = site_means,
    aes(
      x = lime_tha,
      y = mean_yield,
      color = interaction(crop, admin2_gadm),
      shape = crop
    ),
    size = 2
  ) +
  # site curves
  geom_line(
    data = site_curves,
    aes(
      x = lime_tha,
      y = pred,
      color = interaction(crop, admin2_gadm)
    ),
    linewidth = 1
  ) +
  # pooled curve (black dashed)
  geom_line(
    data = pooled_curve,
    aes(x = lime_tha, y = pred),
    color = "black",
    linetype = "dashed",
    size = 1
  ) +
  labs(
    x = "Lime rate (t/ha)",
    y = "Cereal yield (t/ha)",
    title = "Yield response curves by site × crop",
    color = "Crop, Site",
    shape = "Crop"
  ) +
  theme_bw(base_size = 14)

# Profitability analysis

price_tbl <- data.frame(
  country = c(
    "Ethiopia",
    "Ethiopia",
    "Ethiopia",
    "Ethiopia",
    "Rwanda",
    "Rwanda",
    "Rwanda",
    "Rwanda",
    "Rwanda",
    "Rwanda",
    "Tanzania",
    "Tanzania",
    "Tanzania",
    "Tanzania"
  ),
  admin2_gadm = c(
    "Jimma",
    "Jimma",
    "MisraqGojjam",
    "MisraqGojjam",
    "Burera",
    "Burera",
    "Ngororero",
    "Ngororero",
    "Nyaruguru",
    "Nyaruguru",
    "Geita",
    "Geita",
    "Mbozi",
    "Mbozi"
  ),
  crop = c(
    "Maize",
    "Soybean",
    "Fababean",
    "Wheat",
    "Beans",
    "Maize",
    "Beans",
    "Maize",
    "Beans",
    "Maize",
    "Beans",
    "Maize",
    "Beans",
    "Maize"
  ),
  N = c(184, 41, 23, 37, 148, 184, 148, 184, 148, 184, 148, 184, 148, 184),
  crop_price = c(280, 420, 380, 580, 390, 245, 395, 245, 400, 250, 390, 260, 385, 250),
  lime_price = c(85, 85, 85, 85, 120, 120, 120, 120, 120, 120, 100, 100, 100, 100)
)




df_resp <- df %>%
  group_by(country, admin2_gadm, fid, crop) %>%
  mutate(
    yield_T1 = mean(yield_tha[lime_tha == 0], na.rm = TRUE),
    yield_response = yield_tha - yield_T1
  ) %>%
  ungroup() %>%
  filter(!is.na(yield_response))


df_profit <- df_resp %>%
  left_join(price_tbl, by = c("country", "crop", "admin2_gadm")) %>%
  mutate(profit_gain = yield_response * crop_price - lime_tha * lime_price)

# 2.2 Profit gain by crop × site × treatment
df_profit %>%
  mutate(
    treatment2 = if_else(
      lime_tha == 0,
      "T1 (0 t/ha)",
      if_else(
        lime_tha == 1,
        "T2 (1 t/ha)",
        if_else(
          abs(lime_tha - 2.5) < 1e-6,
          "T3 (2.5 t/ha)",
          if_else(lime_tha == 7, "T4 (7 t/ha)", paste0("Lime=", lime_tha))
        )
      )
    ),
    treatment2 = factor(
      treatment2,
      levels = c("T1 (0 t/ha)", "T2 (1 t/ha)", "T3 (2.5 t/ha)", "T4 (7 t/ha)")
    )
  ) %>%
  filter(lime_tha > 0) %>% # exclude T1 (profit gain = 0 by definition)
  ggplot(aes(x = treatment2, y = profit_gain, fill = admin2_gadm)) +
  geom_boxplot(
    alpha = 0.9,
    width = 0.5,
    staplewidth = 0.2,
    outliers = FALSE,
    coef = 1.5,
    linewidth = 0.3
  ) +
  facet_wrap(~crop, scales = "free_y", ncol = 2) +
  guides(fill = guide_legend(ncol = 2), shape = guide_legend(ncol = 2)) +
  labs(
    x = "",
    y = "Profit gain (USD/ha)",
    title = "Profit gain by treatment and site",
    fill = ""
  ) +
  scale_fill_manual(values = bar_colors) +
  ggthemes::theme_clean(base_size = 18, base_family = my_font) +
  theme(
    legend.position = c(0.8, 0.05),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.background = element_rect(fill = NA, color = NA),
    # legend font
    legend.text = element_text(size = 12, family = my_font),
    plot.title = element_text(hjust = 0.5, size = 18, face = "plain"),
    strip.background = element_rect(fill = alpha("#1871B8", 0.1), color = "white"),
    panel.grid.major.y = element_line(linewidth = 0.1)
  )


crops <- unique(df_resp$crop)
reg_results <- list()
lmm_results <- list()

soil_cols <- c("p_h_BP", "soc_BP", "clay_BP", "cec_BP", "ecec_BP", "psi_BP")

for (cr in crops) {
  print(cr)
  dat <- df_resp %>%
    filter(crop == cr) %>%
    drop_na(yield_response, lime_tha, admin2_gadm)

  # Use only soil columns that exist
  soil_use <- intersect(names(dat), soil_cols)

  if (length(unique(dat$admin2_gadm)) < 2) {
    fixef_terms <- c("lime_tha", "I(lime_tha^2)", soil_use)
  } else {
    fixef_terms <- c("lime_tha", "I(lime_tha^2)", soil_use, "admin2_gadm")
  }

  f_lm <- reformulate(termlabels = fixef_terms, response = "yield_response")

  # Build formulas

  f_lmm <- as.formula(
    paste0(
      "yield_response ~ lime_tha + I(lime_tha^2) + ",
      paste(soil_use, collapse = " + "),
      if (length(unique(dat$admin2_gadm)) > 1) {
        " + (1|admin2_gadm)"
      } else {
        ""
      },
      " + (1|fid)"
    )
  )

  # Fit
  reg_results[[cr]] <- summary(lm(f_lm, data = dat))
  lmm_results[[cr]] <- summary(lme4::lmer(f_lmm, data = dat, REML = TRUE))
}

# Lime grid for predictions
lime_grid <- tibble(lime_tha = seq(0, 7, by = 0.5))

curve_list <- list()

for (cr in crops) {
  dat <- df_resp %>% filter(crop == cr)

  # Drop soil vars with <2 unique values
  soil_use <- soil_cols[sapply(dat[soil_cols], function(x) {
    length(unique(na.omit(x))) > 1
  })]

  # Build mixed model formula dynamically
  re_terms <- c()
  if (length(unique(dat$admin2_gadm)) > 1) {
    re_terms <- c(re_terms, "(1|admin2_gadm)")
  }
  if (length(unique(dat$fid)) > 1) {
    re_terms <- c(re_terms, "(1|admin2_gadm:fid)")
  }

  f_lmm <- as.formula(
    paste0(
      "yield_response ~ lime_tha + I(lime_tha^2)",
      if (length(soil_use) > 0) {
        paste0(" + ", paste(soil_use, collapse = " + "))
      } else {
        ""
      },
      if (length(re_terms) > 0) {
        paste0(" + ", paste(re_terms, collapse = " + "))
      } else {
        ""
      }
    )
  )

  m <- lmer(f_lmm, data = dat, REML = TRUE)

  # Site-level means for soil vars (to hold constant when predicting)
  site_means <- dat %>%
    group_by(admin2_gadm) %>%
    summarise(across(all_of(soil_use), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

  # Build EMMs per site across lime rates
  emms_site <- map_dfr(unique(dat$admin2_gadm), function(s) {
    ref <- lime_grid
    if (length(soil_use) > 0) {
      for (v in soil_use) {
        ref[[v]] <- site_means %>%
          filter(admin2_gadm == s) %>%
          pull(v)
      }
    }
    ref$admin2_gadm <- s

    em <- emmeans(
      m,
      ~lime_tha,
      at = list(lime_tha = ref$lime_tha),
      data = ref,
      re.form = NA
    )
    as.data.frame(em) %>% mutate(admin2_gadm = s, crop = cr)
  })

  curve_list[[cr]] <- emms_site
}

emm_curves <- bind_rows(curve_list)

# Plot curves
p_curves <- emm_curves %>%
  ggplot(aes(
    x = lime_tha,
    y = emmean,
    color = admin2_gadm,
    group = admin2_gadm
  )) +
  geom_line(linewidth = 1) +
  geom_ribbon(
    aes(ymin = lower.CL, ymax = upper.CL, fill = admin2_gadm),
    alpha = 0.15,
    color = NA
  ) +
  facet_wrap(~crop, scales = "free_y") +
  scale_color_manual(values = bar_colors) +
  scale_fill_manual(values = bar_colors) +
  labs(x = "Lime (t/ha)", y = "Predicted yield response (t/ha)", title = "Yield response curves by crop × site (LMM)") +
  ggthemes::theme_pander(base_size = 16, base_family = my_font) +
  theme(legend.position = "bottom")

p_curves

write_csv(emm_curves, "tmp/yield_response_curves_by_crop_site.csv")

# Per-crop ML models (predict yield response)
set.seed(123)
ml_results <- list()

for (cr in crops) {
  dat <- df_resp %>%
    filter(crop == cr) %>%
    drop_na()

  n_obs <- nrow(dat)
  if (n_obs < 30) {
    message("Skipping ", cr, " (only ", n_obs, " observations)")
    next
  }

  soil_use <- soil_cols[sapply(dat[soil_cols], function(x) length(unique(na.omit(x))) > 1)]

  # Build design matrix
  f_ml <- as.formula(paste0(
    "~ lime_tha + I(lime_tha^2)",
    if (length(soil_use) > 0) paste0(" + ", paste(soil_use, collapse = " + ")) else "",
    " + admin2_gadm"
  ))

  mm <- model.matrix(f_ml, data = dat)

  # Align X and y: drop intercept from X
  X <- mm[, -1, drop = FALSE]
  y <- dat$yield_response

  # Now X and y always have same rows
  stopifnot(nrow(X) == length(y))

  # Train/test split
  idx <- caret::createDataPartition(y, p = 0.7, list = FALSE)

  if (length(idx) == 0 || length(idx) == n_obs) {
    message("Using all data for training/testing in ", cr)
    Xtr <- X
    ytr <- y
    Xte <- X
    yte <- y
  } else {
    Xtr <- X[idx, , drop = FALSE]
    ytr <- y[idx]
    Xte <- X[-idx, , drop = FALSE]
    yte <- y[-idx]
  }

  # Random Forest
  rf_mod <- randomForest(
    x = Xtr, y = ytr,
    ntree = 500,
    mtry = max(2, floor(ncol(Xtr) / 3)),
    importance = TRUE
  )
  rf_pred <- predict(rf_mod, Xte)
  rf_perf <- caret::postResample(rf_pred, yte)

  # XGBoost
  dtr <- xgboost::xgb.DMatrix(data = Xtr, label = ytr)
  dte <- xgboost::xgb.DMatrix(data = Xte, label = yte)

  xgb_mod <- xgboost::xgb.train(
    data = dtr,
    nrounds = 300,
    eta = 0.08,
    max_depth = 6,
    subsample = 0.8,
    colsample_bytree = 0.8,
    objective = "reg:squarederror",
    verbose = 0
  )
  xgb_pred <- predict(xgb_mod, dte)
  xgb_perf <- caret::postResample(xgb_pred, yte)

  ml_results[[cr]] <- list(
    rf = list(model = rf_mod, perf = rf_perf),
    xgb = list(
      model = xgb_mod, perf = xgb_perf,
      feature_names = colnames(X)
    )
  )
}