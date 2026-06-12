# Analysis pipeline

Standalone research pipeline: raw GAIA trial data → yield response functions → profitability, NPV, sensitivity, risk, and distributional analysis. Methodology, equations, and adaptation guidance are in the [main README](../README.md).

## How to run

Open `analysis.Rproj` in RStudio (or `setwd()` to this folder) and source the scripts in numeric order. Scripts are independent steps connected only through files in `data/` and `outputs/`, so you can rerun any step without the ones before it as long as its inputs exist — and the derived inputs (`data/data_y1.csv`, `data/data_y1_rain.csv`, `data/selected_vars.rds`) are committed.

| Script | Needs | Produces |
|---|---|---|
| `00_prepare_data.R` | `data/raw/gaia_trials_all_countries_combined.dta` | `data/data_y1.csv` |
| `01_extract_rainfall.R` | `data/data_y1.csv`, `data/raw/annual_Rainfall_1981_2024.tif` (not committed — 274 MB, obtain separately) | `data/data_y1_rain.csv` |
| `02_variable_selection.R` | `data/data_y1_rain.csv` | `data/selected_vars.rds`, selection summary |
| `03_response_functions.R` | `data_y1_rain.csv`, `selected_vars.rds` | `outputs/response_curves.csv`, model summary, fig |
| `04_profitability.R` | response curves, `data/prices/*` , `R/helpers_econ.R` | crop prices, year-1 profit, NPV, optimal rate |
| `05_sensitivity.R` | response curves, crop prices, optimal rate | sensitivity grid, break-even ratios, figs |
| `06_prob_profit.R` | year-1 profit, crop prices | `prob_profit.csv`, figs |
| `07_model_performance.R` | `data_y1_rain.csv`, `selected_vars.rds` | performance, variance decomposition, coefficient & residual figs |
| `08_additional_analysis.R` | curves, prices, profit, NPV, data | BCR/ROI, IRR, pH-stratified, dominance, price ceiling |
| `09_quantile_lmm.R` | `data_y1_rain.csv`, crop prices | quantile response/profit/NPV curves, figs |

## Output catalog (`outputs/`)

**Tables**

| File | Contents |
|---|---|
| `variable_selection_summary.csv` | every candidate covariate × crop: NZV pass, LASSO coefficient, VIF, final decision, drop reason |
| `response_curves.csv` | crop × lime rate: mean yield response with 95% CI — the central agronomic result |
| `model_summary.csv` | LMM fixed-effect estimates, SEs, p-values per crop |
| `crop_prices.csv` | weighted crop-level retail and farmgate prices, lime price, baseline price ratio |
| `profitability_yr1.csv` | field-level year-1 profit per observation (heterogeneity preserved) |
| `npv_by_crop.csv` | crop × lime rate: 4-year NPV with CI |
| `optimal_lime_rate.csv` | NPV-maximizing lime rate per crop with NPV and CI at the optimum |
| `sensitivity_grid.csv` | profit over (crop × lime rate × price ratio) |
| `breakeven_price_ratio.csv` | minimum crop-to-lime price ratio for zero profit, by lime rate |
| `prob_profit.csv` | Monte Carlo P(profit > 0) over (crop × lime rate × price ratio) |
| `model_performance.csv` / `variance_decomposition.csv` / `model_coefficients.csv` | marginal & conditional R², RMSE, ICC; site/field/residual variance shares; coefficient table |
| `bcr_roi.csv` / `irr_by_crop.csv` / `lime_price_ceiling.csv` | benefit-cost ratio & ROI; internal rate of return; max lime price with NPV > 0 |
| `ph_stratified_profit.csv` | profit by baseline-pH quartile × lime rate (targeting result) |
| `quantile_response_curves.csv` / `quantile_response_summary.csv` / `quantile_profit_npv_curves.csv` | Q20/Q50/Q80 response and economics |

**Figures** — `fig_*.png`, named after the table they visualize (e.g. `fig_npv_curves.png`, `fig_sensitivity_heatmap.png`, `fig_prob_profit_heatmap.png`, `fig_quantile_npv.png`).

## Notes

- `R/helpers_econ.R` holds the pure economic functions (`pv_factor`, `discount_factor`). The Shiny app carries an identical copy of these two functions inside its own helpers so each component stays standalone — if you change a formula, change both.
- `data/prices/source/` documents price provenance (FAO/WFP extracts and the cleaning script).
