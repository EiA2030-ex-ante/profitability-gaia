pacman::p_load(tidyverse,
               haven,
               janitor,
               ggthemes,
               ggstatsplot,
               leaflet,
               lme4,
               emmeans,
               nlme)


extrafont::loadfonts(q=T)

my_font <- "Frutiger"
#--------- 1. Load data ------------------------------------------------------------

all_data <- haven::as_factor(
  read_dta("data/raw/gaia_trials_all_countries_combined.dta"),
  only_labelled = TRUE
)


# how many observations do we have

tbl_1 <- all_data |>
  group_by(country, admin2_gadm, season, crop, treatment) |>
  summarise(n = n())

# where are the trials located

map_1 <- all_data |>
  select(fid, country, admin2_gadm, lat, lng, crop) |>
  distinct() |>
  filter(!is.na(lat)) |>
  group_by(fid, country, admin2_gadm, lat, lng) |>
  summarise(
    n_crops = n(),
    crops   = paste(unique(crop), collapse = " | ")  # crop names separated by |
  ) |>
  ungroup() |>
  distinct()

# create color palette by admin2_gadm

admin2_gadm <- unique(map_1$admin2_gadm)
# remove NA

admin2_gadm <- admin2_gadm[!is.na(admin2_gadm)]

color_plasma <- setNames(viridis::plasma(length(admin2_gadm) + 2), admin2_gadm)
cols <- hcl.colors(length(unique(admin2_gadm)), "Zissou 1")
setNames(cols, admin2_gadm)

pal <- colorFactor(
  palette = cols,
  # or use cols
  domain  = map_1$admin2_gadm
)

library(leaflet)
leaflet(map_1) |>
  addTiles() |>
  addCircleMarkers(
    ~lng,
    ~lat,
    radius = ~n_crops * 5,
    color = ~pal(admin2_gadm),
    stroke = FALSE,
    fillOpacity = 0.5,
    popup = ~ htmltools::HTML(
      paste0(
        "<table style='width:200px; border-collapse:collapse;'>",
        "<tr><th style='text-align:left;'>FID</th><td>", fid, "</td></tr>",
        "<tr><th style='text-align:left;'>Admin2</th><td>", admin2_gadm, "</td></tr>",
        "<tr><th style='text-align:left;'># Crops</th><td>", n_crops, "</td></tr>",
        "<tr><th style='text-align:left;'>Crops</th><td>", crops, "</td></tr>",
        "</table>"
      )
    )
  ) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addLegend(
    "bottomright",
    pal = pal,
    values = ~ admin2_gadm,
    title = "Admin2"
  )
# year 1 data

data_y1 <- all_data |>
  filter(harvest_year == 2022) |>
  filter(season == 1) |>
  select(country:tex_BP) |>
  select(-farmer_name)
glimpse(data_y1)

# Master year-1 dataset consumed by all downstream scripts.
# write.csv (not readr) on purpose: keeps the row-name first column that
# downstream readers were built against.
write.csv(data_y1, "data/data_y1.csv")

summary_df <- data_y1 %>%
  group_by(country, crop, lime_tha) %>%
  summarise(n_obs = sum(!is.na(yield_tha)), .groups = "drop")

# Heatmap plot
ggplot(summary_df, aes(x = country, y = crop, fill = n_obs)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n_obs), color = "black") +
  scale_fill_gradient(low = "lightyellow", high = "darkgreen") +
  labs(title = "Number of Observations by Country × Crop × Lime Rate",
       x = "Country",
       y = "Crop",
       fill = "Observations") +
  facet_wrap( ~ lime_tha) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary_df %>%
  filter(lime_tha == 0) %>%
  ggplot(aes(x = country, y = crop, fill = n_obs)) +
  geom_tile(color = "white", width = 0.5) +
  geom_text(aes(label = n_obs), color = "black") +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  labs(title = "Number of farmers") +
  theme_minimal(base_size = 14, base_family = my_font)


# Filter to Ethiopia Maize (example)
dat <- data_y1 %>%
  filter(country == "Ethiopia", tolower(crop) == "maize") %>%
  select(fid, lime_tha, yield_tha) %>%
  filter(!is.na(yield_tha))

# Add quadratic term
dat <- dat %>% mutate(lime2 = lime_tha^2)

# Mixed-effects quadratic model: random intercept by site (fid)
m_quad <- lmer(yield_tha ~ lime_tha + lime2 + (1 | fid), data = dat)

# Model summary
summary(m_quad)

# Create prediction grid
newdat <- data.frame(lime_tha = seq(0, 7, 0.1))
newdat$lime2 <- newdat$lime_tha^2
newdat$pred <- predict(m_quad, newdata = newdat, re.form = NA) # fixed effects only

# Observed treatment means ± SE
means <- dat %>%
  group_by(lime_tha) %>%
  summarise(
    mean = mean(yield_tha, na.rm = TRUE),
    se = sd(yield_tha, na.rm = TRUE) / sqrt(n())
  )

# Plot
ggplot(dat, aes(x = lime_tha, y = yield_tha)) +
  geom_point(alpha = 0.3) +
  geom_errorbar(
    data = means,
    aes(
      y = mean,
      ymin = mean - se,
      ymax = mean + se
    ),
    width = 0.1,
    color = "red"
  ) +
  geom_point(data = means,
             aes(y = mean),
             color = "red",
             size = 2) +
  geom_line(
    data = newdat,
    aes(x = lime_tha, y = pred),
    color = "blue",
    size = 1
  ) +
  labs(title = "Mixed-effects Quadratic (Ethiopia, Maize)", x = "Lime rate (t/ha)", y = "Yield (t/ha)") +
  theme_minimal(base_size = 14)

dat_2 <- dat %>% mutate(lime_factor = as.factor(lime_tha))

# Mixed model: yield ~ lime treatment (factor), random intercept by site
m_anova <- lmer(yield_tha ~ lime_factor + (1 | fid), data = dat_2)

# Model summary
summary(m_anova)

# Estimated marginal means (treatment means adjusted for site)
emm <- emmeans(m_anova, ~ lime_factor)
pairs(emm)   # pairwise comparisons
emm_df <- as.data.frame(emm)

# Plot treatment means with SE bars
ggplot(emm_df, aes(x = lime_factor, y = emmean)) +
  geom_point(size = 3, color = "red") +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.1) +
  labs(title = "ANOVA-style Mixed Model (Ethiopia Maize)", x = "Lime treatment (t/ha)", y = "Estimated mean yield (t/ha)") +
  theme_minimal(base_size = 14)


#---------------------------
A_start <- max(dat$yield_tha, na.rm = TRUE)
B_start <- A_start - mean(dat$yield_tha[dat$lime_tha == 0], na.rm = TRUE)
C_start <- 0.5

# Nonlinear mixed model: Mitscherlich with random intercept (A by fid)
m_mitsch <- nlme(
  yield_tha ~ A - B * exp(-C * lime_tha),
  data = dat,
  fixed = A + B + C ~ 1,
  random = A ~ 1 | fid,
  # allow asymptote A to vary by site
  start = c(A = A_start, B = B_start, C = C_start),
  na.action = na.omit,
  control = nlmeControl(
    pnlsTol = 0.1,
    msMaxIter = 200,
    pnlsMaxIter = 50
  )
)

summary(m_mitsch)

# Predictions for plotting
newdat <- data.frame(lime_tha = seq(0, 7, 0.1), fid = dat$fid[1])
newdat$pred <- predict(m_mitsch, newdata = newdat, level = 0) # population-level curve

# Observed treatment means
means <- dat %>%
  group_by(lime_tha) %>%
  summarise(
    mean = mean(yield_tha, na.rm = TRUE),
    se = sd(yield_tha, na.rm = TRUE) / sqrt(n())
  )

# Plot
ggplot(dat, aes(x = lime_tha, y = yield_tha)) +
  geom_point(alpha = 0.3) +
  geom_errorbar(
    data = means,
    aes(
      y = mean,
      ymin = mean - se,
      ymax = mean + se
    ),
    width = 0.1,
    color = "red"
  ) +
  geom_point(data = means,
             aes(y = mean),
             color = "red",
             size = 2) +
  geom_line(
    data = newdat,
    aes(x = lime_tha, y = pred),
    color = "darkgreen",
    size = 1
  ) +
  labs(title = "Mitscherlich Mixed Model (Ethiopia Maize)", x = "Lime rate (t/ha)", y = "Yield (t/ha)") +
  theme_minimal(base_size = 14)
#---------------------------
# Fixed effects (population-average parameters)
fixef(m_mitsch)

# Random effects for A by site (deviation from fixed A)
ranef_vals <- ranef(m_mitsch)

# Combine to get site-level A_s = A_fixed + u_s
site_asymptotes <- data.frame(fid = rownames(ranef_vals),
                              A_site = fixef(m_mitsch)["A"] + ranef_vals$A)

head(site_asymptotes)
summary(site_asymptotes$A_site)

# Rank sites by asymptote
top_sites <- site_asymptotes %>%
  arrange(desc(A_site)) %>%
  head(10)

bottom_sites <- site_asymptotes %>%
  arrange(A_site) %>%
  head(10)

top_sites
bottom_sites
ggplot(site_asymptotes, aes(x = reorder(fid, A_site), y = A_site)) +
  geom_point(color = "blue") +
  geom_smooth(method = "loess",
              color = "red",
              se = T) +
  coord_flip() +
  labs(title = "Estimated Asymptotes (A_s) by Site", x = "Site (fid)", y = "Asymptote yield (t/ha)") +
  theme_minimal(base_size = 14)

# Create a grid of lime rates and all sites
# All combinations of lime levels and sites
lime_grid <- expand.grid(lime_tha = seq(0, 7, 0.5), fid = unique(dat$fid))

# Predict with site-level random effects (level = 1)
lime_grid$pred_yield <- predict(m_mitsch, newdata = lime_grid, level = 1)

head(lime_grid)

# Select a handful of sites (e.g. top 5 by asymptote)
example_sites <- site_asymptotes %>%
  arrange(desc(A_site)) %>%
  slice(1:5) %>%
  pull(fid)

ggplot(filter(lime_grid, fid %in% example_sites),
       aes(x = lime_tha, y = pred_yield, color = fid)) +
  geom_line(size = 1) +
  labs(title = "Predicted Mitscherlich Curves for Selected Sites", x = "Lime rate (t/ha)", y = "Predicted yield (t/ha)") +
  theme_minimal(base_size = 14)
opt_rates <- lime_grid %>%
  group_by(fid) %>%
  slice_max(order_by = pred_yield, n = 1) %>%
  ungroup()

head(opt_rates)
