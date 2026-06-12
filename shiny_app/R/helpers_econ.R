# R/helpers_econ.R
discount_factor <- function(T, r) {
  T <- as.integer(T)
  r <- as.numeric(r)
  if (is.na(T) || T <= 0) {
    return(0)
  }
  sum(1 / (1 + r)^(1:T))
}

pv_factor <- function(T, r, decay = 0) {
  # decay is entered as proportion (0–1)
  if (T <= 0) {
    return(0)
  }
  t <- 1:T
  sum(((1 - decay)^(t - 1)) / ((1 + r)^t))
}


# =====================================================
# Risk-Adjusted Return + VaR
# =====================================================

mod_risk_analysis <- function(dat, crop_price, lime_price, disc_factor, n_sim = 1000) {
  crop_prices <- pmax(rnorm(n_sim, crop_price, crop_price * 0.15), 0)
  lime_prices <- pmax(rnorm(n_sim, lime_price, lime_price * 0.10), 0)

  res <- dat %>%
    mutate(
      npv_mean = map_dbl(lime_tha, \(L) mean((yield_resp * crop_prices * disc_factor) - (L * lime_prices))),
      npv_sd   = map_dbl(lime_tha, \(L) sd((yield_resp * crop_prices * disc_factor) - (L * lime_prices))),
      RAR      = npv_mean / npv_sd
    )

  p_rar <- ggplot(res, aes(lime_tha, RAR)) +
    geom_line(color = "#004D40", linewidth = 1.2) +
    labs(
      x = "Lime rate (t/ha)", y = "Risk-adjusted return (E[NPV]/SD[NPV])",
      title = "Risk-Adjusted Return Surface"
    ) +
    theme_minimal(base_size = 14)

  # Value-at-Risk
  npv_vec <- (dat$yield_resp * crop_price * disc_factor) - (dat$lime_tha * lime_price)
  VaR_5 <- quantile(npv_vec, 0.05, na.rm = TRUE)
  CVaR_5 <- mean(npv_vec[npv_vec <= VaR_5], na.rm = TRUE)

  p_var <- ggplot(data.frame(NPV = npv_vec), aes(NPV)) +
    geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "#009E73", color = "white") +
    geom_vline(xintercept = VaR_5, color = "red", linetype = "dashed") +
    geom_vline(xintercept = CVaR_5, color = "darkred", linetype = "dotted") +
    labs(
      x = "NPV (USD/ha)", y = "Density",
      title = "Value-at-Risk and Conditional VaR",
      subtitle = paste0("VaR5%=", round(VaR_5, 1), " | CVaR5%=", round(CVaR_5, 1))
    ) +
    theme_minimal(base_size = 14)

  list(plot_rar = ggplotly(p_rar), plot_var = ggplotly(p_var))
}


# =====================================================
# Risk-Adjusted Return + VaR
# =====================================================
mod_shocks <- function(dat, crop_price, lime_price, disc_factor) {
  shocks <- expand_grid(
    price_shock = c(-0.3, 0, +0.3),
    rainfall_shock = c(-0.2, 0, +0.2)
  )

  sim <- shocks %>%
    mutate(data = pmap(list(price_shock, rainfall_shock), \(ps, rs) {
      dat %>%
        mutate(
          yield_resp_s = yield_resp * (1 + rs),
          npv = (yield_resp_s * crop_price * (1 + ps)) * disc_factor - lime_tha * lime_price,
          P_pos = mean(npv > 0, na.rm = TRUE)
        ) %>%
        summarise(mean_P = mean(P_pos, na.rm = TRUE))
    })) %>%
    unnest(data)

  p <- ggplot(sim, aes(price_shock, rainfall_shock, fill = mean_P)) +
    geom_tile() +
    scale_fill_viridis_c(labels = scales::percent) +
    labs(
      x = "Price Shock (%)", y = "Rainfall Shock (%)", fill = "P(NPV>0)",
      title = "Conditional Profitability under Shocks"
    ) +
    theme_minimal(base_size = 14)

  ggplotly(p)
}

# =====================================================
# Spatial Scaling Curve
# =====================================================
mod_scaling <- function(df_map, dat, crop_price, lime_price, disc_factor) {
  benefit <- mean((dat$yield_resp * crop_price * disc_factor) - dat$lime_tha * lime_price, na.rm = TRUE)
  df_scale <- df_map %>%
    mutate(
      area_ha = runif(n(), 100, 5000),
      NPV_per_ha = benefit * runif(n(), 0.5, 1.2)
    ) %>%
    arrange(desc(NPV_per_ha)) %>%
    mutate(
      cum_area = cumsum(area_ha),
      cum_benefit = cumsum(NPV_per_ha * area_ha)
    )

  p <- ggplot(df_scale, aes(cum_area / sum(area_ha), cum_benefit / 1e6)) +
    geom_line(color = "#1871B8", linewidth = 1.3) +
    labs(
      x = "Adoption share", y = "Cumulative benefit (Million USD)",
      title = "Scaling Curve: Total Benefit vs Adoption Share"
    ) +
    theme_minimal(base_size = 14)

  ggplotly(p)
}

# =====================================================
# Dynamic Adoption Simulation
# =====================================================
mod_adoption <- function(df_econ, max_adopt = 0.8, T = 10, r = 0.1, decay = 0.05) {
  t <- 1:T
  A_t <- max_adopt / (1 + exp(-0.8 * (t - 4))) # logistic adoption curve
  benefit_t <- mean(df_econ$npv, na.rm = TRUE) * (1 - decay)^(t - 1)
  PV_benefit <- benefit_t * A_t / (1 + r)^t

  df_dyn <- data.frame(Year = t, Adoption = A_t, Benefit = PV_benefit)

  p <- ggplot(df_dyn, aes(Year)) +
    geom_line(aes(y = Adoption * 100, color = "Adoption (%)"), linewidth = 1.2) +
    geom_line(aes(y = Benefit / 1e6, color = "Discounted Benefit (Million USD)"), linewidth = 1.2) +
    scale_y_continuous(sec.axis = sec_axis(~ . * 1, name = "Discounted Benefit (M USD)")) +
    labs(x = "Year", y = "Adoption (%)", title = "Dynamic Adoption and Cumulative Benefit") +
    theme_minimal(base_size = 14)

  ggplotly(p)
}