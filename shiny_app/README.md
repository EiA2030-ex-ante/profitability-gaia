# Lime Profitability Explorer (Shiny app)

Interactive companion to the [analysis pipeline](../analysis/): explore yield responses and lime profitability by country, site, crop, and model, with live price, time-horizon, and risk controls. See the [main README](../README.md) for the underlying methodology.

![The app's Data Overview tab: sidebar with site/crop/model selectors and price, Monte Carlo, and time-value sliders; panels showing observation counts, mean yields and yield responses with 95% CIs, and a map of trial fields](Screenshot_app.jpeg)

## Run

```r
# Open shiny_app.Rproj (or setwd() to this folder), then:
shiny::runApp()
```

Packages (loaded via `pacman` in `R/globals.R`): `shiny`, `bslib`, `leaflet`, `plotly`, `dplyr`, `readr`, `tidyr`, `purrr`, `scales`, `stringr`, `ggplot2`, `viridis`, `extrafont`.

## What's inside

| Path | Role |
|---|---|
| `app.R` | entry point; sources the four files in `R/` |
| `R/globals.R` | loads packages and everything in `data/` at startup; builds prediction catalog |
| `R/ui_main.R` | UI: sidebar selectors/sliders + tabs |
| `R/server_main.R` | reactive logic: cascading country → site → crop filters, price adjustment, plot rendering |
| `R/helpers_econ.R` | `pv_factor()`/`discount_factor()` plus the Risk/VaR, shock-scenario, scaling, and adoption modules |
| `docs/*.md` | methodology notes rendered inside the tabs via `includeMarkdown()` |

**Tabs:** Data Overview (trial map, yields, responses) · Agronomic Responses (model curves with profit/loss shading) · Net Revenue & NPV (optimal rate) · Monte Carlo (P(NPV > 0), NPV distribution) · Model Performance (R²/RMSE) · Advanced (Risk & VaR, price/rainfall shocks, scaling curve, adoption dynamics).

## Data (`data/`) — what the app needs at startup

| File | Required? | Notes |
|---|---|---|
| `yield_models_bundle.rds` | yes | per site × crop predictions and metrics for OLS / LMM / RF / XGBoost |
| `data_y1.csv` | yes | trial data for the Data Overview tab |
| `base_crop_prices.csv` | optional | falls back to 160 USD/t if missing |
| `base_lime_price.csv` | optional | falls back to 55 USD/t if missing |

`data_y1.csv` and the price files are copies of the analysis component's prepared inputs — kept here so the app folder deploys standalone (shinyapps.io / Posit Connect / Shiny Server: deploy this folder as-is).

## Rebuilding the model bundle

If the trial data changes, regenerate the bundle (working directory = this folder):

```r
source("data_prep/build_model_bundle.R")
# reads  data/data_y1.csv
# writes data/yield_models_bundle.rds (+ predictions and performance CSVs)
```

It fits OLS (quadratic), a linear mixed model, random forest, and XGBoost per country × site × crop and stores predictions over the lime-rate grid plus RMSE/R² metrics. Extra packages needed: `lme4`, `randomForest`, `xgboost`, `yardstick`.
