# Power Analysis — Overview

## Reviewer Comment

> The manuscript does not include a power analysis or simulation study demonstrating that the available sample sizes are adequate for recovering variance components of the magnitude of interest, and this omission is consequential in at least two respects. First, the gender-stratified WLS subsamples are asked to support estimation of three multilevel models each containing up to nine PGIs, their pairwise interactions with one another, and their interactions with ascribed characteristics, a parameter burden that is difficult to reconcile with a family-level sample of this size; the standard errors of 0.06 on the individual IO estimates for WLS men, large relative to the point estimates themselves, are consistent with this concern. Second, and more fundamentally, the within-family design mechanically reduces the PGI signal available for estimation—within-family PGI variance is smaller than between-family variance while the absolute magnitude of measurement error remains constant—which implies that greater sample sizes are required to achieve equivalent statistical precision compared to a between-family design; the manuscript acknowledges this as a measurement error issue but does not discuss its implications for power.


---

## Power Analysis


### Estimands

Two quantities are assessed:

1. **`delta_within = (emptyind - condind) / totalvar`** — within-family variance reduction attributable to PGIs: (m0 → m1). Explicitly addresses the reviewer's concern, but is not related to the full model with all interactions, which is the real threat to power.

2. **`w = (condind - completeind) / totalvar`** — within-family variance reduction attributable to ascribed characteristics beyond PGIs: (m1 → m2). Hardest detection problem due to the large parameter burden (all PGIs, ascribed, and their interaction).



### Steps

**Step 1 — Calibrate parameters from observed WLS data**
Read variance components from estimated models.

| Sample | N families | ICC | delta_within | w_obs |
|--------|-----------|-----|--------------|-------|
| Brothers | 473 | 0.377 | 0.038 | 0.008 |
| Sisters | 612 | 0.373 | 0.033 | 0.025 |


**Step 2 — Data-generating process**
Generates synthetic sibling datasets with a controlled variance decomposition. Each PGI is split into a between-family component (f_j ~ N(0, 0.5)) and a within-family component (w_ij ~ N(0, 0.5)). PGI coefficients are calibrated so that within-family PGI contribution equals exactly `delta_within`. For `w`, ascribed variables additionally respect their real-world between/within variance structure; their coefficients are calibrated so their joint within-family contribution equals exactly `w_true`. Residual variances `sigma2_tau` and `sigma2_delta` are set to reproduce the observed ICC exactly.

**Step 3 — Fit models and extract variance components**
Fits the three-model sequence (m0, m1, m2) via `lmer`, reproducing the paper's full-interaction specification. Extracts `delta_within` (m0 → m1) and `w` (m1 → m2).

**Step 4 — Null distribution**
Runs `n_null` replications with the estimand set to zero (`delta_within = 0` for `delta_within`; `w_true = 0` for `w`) at the actual sample size of each subsample. The 95th percentile of the resulting distribution is the critical value for a one-sided alpha = 0.05 test.

**Step 5 — Power curve**
Loops over an effect size grid at the fixed observed N for each sample. For each grid point, runs `n_sim` replications and records the fraction where the estimate exceeds the null critical value. 

**Step 6 — Plot**
Power curve with x-axis = true effect size, y-axis = power. A dashed line marks 80% power; a red dotted line marks the observed effect size; a blue dotted line marks the minimum detectable effect (linearly interpolated where the curve crosses 80%).

### Scripts

- **`11_POWER_d_simulation.R`** — power for `delta_within`; fits m0 and m1 only (no ascribed variables required)
- **`12_POWER_w_simulation.R`** — power for `w`; fits all three models including the full m2 with ~210 fixed effects
