# Agronomic Yield Response Modeling

## Overview
The agronomic analysis module quantifies crop yield response to lime application.  
For each combination of country, site, crop, and model, predicted yields (\\( \hat{Y} \\)) are estimated across lime application rates (\\( L \\), in t/ha).

## Functional Form
\\[
\hat{Y}_i = \beta_0 + \beta_1 L_i + \beta_2 L_i^2 + \epsilon_i
\\]

Where:
- \\( \hat{Y}_i \\): predicted yield (t/ha)
- \\( L_i \\): lime rate (t/ha)
- \\( \epsilon_i \\): residual error

Nonlinear responses are captured using parametric or machine learning models (LMM, RF, XGB).

## Relative Yield Response
\\[
Y_{resp,i} = \hat{Y}_i - \hat{Y}_0
\\]

Confidence intervals (\\( CI_{95} \\)) are derived as:

\\[
CI_{95} = 1.96 \times \frac{s}{\sqrt{n}}
\\]

## Interpretation
The yield response curves identify diminishing returns and breakeven points where marginal yield gains plateau.
