# Archive — superseded first-generation analysis

Kept for provenance only. **Nothing in `analysis/` or `shiny_app/` depends on this folder.**

This was the original approach: yield response curves estimated separately per site × crop (small N per group, no covariate adjustment), with profitability computed from those curves. It was superseded by the pooled crop-level mixed-model pipeline in `analysis/`, which fits one model per crop across all sites and lets soil and rainfall covariates absorb site differences.

| Item | What it was |
|---|---|
| `2_analysis_1.0.R` | exploratory yield/response analysis and figures (wrote `figures/`) |
| `3_profitability_analysis.R` | site × crop NPV, sensitivity, and break-even analysis (wrote `outputs/`) |
| `figures/`, `outputs/` | the outputs of the two scripts above, as last generated |
| `tmp/` | scratch: earlier app versions (`app_v1.R`, `app_v2.R`), one-off graph scripts, intermediate data (`data_y1.dta`) |

The scripts still reference the pre-reorganization paths (`tmp/`, `data/`, `figures/`) and would need path fixes to rerun — intentionally left untouched.
