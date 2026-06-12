# analysis/R/helpers_econ.R
# Pure economic helper functions used across the analysis pipeline
# (04_profitability.R, 08_additional_analysis.R, 09_quantile_lmm.R).
#
# The Shiny app (shiny_app/R/helpers_econ.R) defines the same two functions
# plus its interactive plotting modules. The formulas must stay identical in
# both places so app results match the pipeline.

# Present value of a 1-unit annuity over T years at discount rate r.
discount_factor <- function(T, r) {
  T <- as.integer(T)
  r <- as.numeric(r)
  if (is.na(T) || T <= 0) {
    return(0)
  }
  sum(1 / (1 + r)^(1:T))
}

# Present value factor for a benefit stream that decays at `decay` per year
# (proportion 0-1) over T years, discounted at rate r:
#   sum_t (1 - decay)^(t-1) / (1 + r)^t
pv_factor <- function(T, r, decay = 0) {
  if (T <= 0) {
    return(0)
  }
  t <- 1:T
  sum(((1 - decay)^(t - 1)) / ((1 + r)^t))
}
