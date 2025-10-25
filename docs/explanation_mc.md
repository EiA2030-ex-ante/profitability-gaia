# Monte Carlo Simulation Framework

## Step 1 — Random Variables
\\[
P_c \sim \mathcal{N}(\mu_c, \sigma_c^2), \quad P_l \sim \mathcal{N}(\mu_l, \sigma_l^2)
\\]

## Step 2 — Profit Simulation
\\[
NPV_i(L) = (Y_{resp} P_{c,i}) D - L P_{l,i}
\\]

where \\( D = \sum_{t=1}^{T} \frac{(1-d)^{t-1}}{(1+r)^t} \\).

## Step 3 — Probability of Profitability
\\[
P(NPV > 0) = \frac{1}{N} \sum_{i=1}^{N} I(NPV_i(L) > 0)
\\]

## Interpretation
Monte Carlo analysis captures stochasticity in market conditions and quantifies the likelihood of profitability under uncertainty.
