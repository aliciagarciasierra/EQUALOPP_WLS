# PGI-RC Correction

## Paragraph for the paper

Because polygenic indices are noisy proxies for the underlying additive genetic factor, the PGI contributions embedded in IOLib and IORad are attenuated relative to the true genetic signal. Following Becker et al. (2021), we apply a measurement-error correction that rescales each PGI contribution upward by a factor ρ = √(h²_SNP / R²_PGI) > 1, where h²_SNP is SNP heritability and R²_PGI is the variance in education explained by the PGI. Because our decomposition separates between-family and within-family components, we use two distinct correction factors — ρ_between and ρ_within — derived from the corresponding heritability and R² at each level. Heritability estimates are taken from two published reports: Scenario A draws on the WLS-specific GREML estimate of Becker et al. (2021; h²_pop = 0.172) and scales the within-family component proportionally from Howe et al. (2022), yielding h²_within ≈ 0.053; Scenario B uses the multi-cohort within-sibship estimates of Howe et al. (2022) directly (h²_pop = 0.13, h²_within = 0.04), which includes MoBa in the cohorts. Thus, for MoBa our preferred correction is the one that follows Howe et al. (2022). The sample-based R² values are estimated from our analytical samples: R²_between is obtained by regressing family-mean education on family-mean PGI (both residualized on a birth-year polynomial, sex, and genetic principal components), while R²_within is obtained from the analogous regression on within-family-demeaned residuals (without principal components). Because h² values are fractions of total phenotypic variance whereas R² values are fractions of level-specific variance, we rescale the latter to the total-variance scale via the ICC before dividing. The correction lowers IOLib — the noisy PGI inflated the residual family-level variance component, causing the liberal measure to over-attribute shared environmental variance to the genetic proxy — and raises IORad, since the within-family genetic contribution was underestimated. Figure X shows the original and corrected indices under both scenarios for WLS and MoBa.

## What the correction does

The gap $\text{IORad} - \text{IOLib}$ equals the share of total variance attributed to PGI EA. Because the PGI is a noisy proxy for the true additive genetic factor $g^*$, this share is attenuated. The correction (Becker et al. (2021)) rescales it upward using $\rho > 1$, the ratio of true genetic signal to observed PGI signal:

$$\rho = \sqrt{\frac{h^2_\text{SNP}}{R^2_\text{PGI}}} > 1$$


Because our decomposition distinguishes between-family and within-family contributions, we need two separate correction factors — $\rho_\text{between}$ and $\rho_\text{within}$ — each computed from the corresponding $h^2$ and $R^2$ at that level.

---

## Step 1. External heritability estimates ($h^2$)

We use two scenarios, differing in the source of $h^2_\text{pop}$. Both are for educational attainment measured in years of schooling.

### Scenario A — Becker et al. (2021), WLS-specific

- $h^2_\text{pop} = 0.172\ (\text{SE} = 0.103)$: WLS-specific GREML estimate, Supplement Table 4. 
- $h^2_\text{within}$: Becker does not report a within-family estimate. We impute it by scaling Howe's within/total ratio to Becker's larger $h^2_\text{pop}$, on the assumption that the share of genetic variance transmitted within families is constant across populations:

$$h^2_{\text{within},\text{Becker}} = h^2_{\text{within},\text{Howe}} \times \frac{h^2_{\text{pop},\text{Becker}}}{h^2_{\text{pop},\text{Howe}}} = 0.04 \times \frac{0.172}{0.13} \approx 0.053$$

- $h^2_\text{between} = h^2_\text{pop} - h^2_\text{within} \approx 0.119$

**Modeling choice**: the proportional scaling assumption is not testable from the available data. It produces a Becker scenario that is internally consistent (within/total ratio matches Howe), but the true within-family $h^2$ for WLS could differ. Using Howe's $h^2_\text{within} = 0.04$ directly would overestimate $h^2_\text{between}$ (since Becker's total is larger), and was therefore not used.


### Scenario B — Howe et al. (2022), Nature Genetics


The data sources included in the estimations include MoBa (see Supplementary Table 1).

- $h^2_\text{pop} = 0.13\ (95\%\ \text{CI: } 0.12\text{–}0.15)$
- $h^2_\text{within} = 0.04\ (95\%\ \text{CI: } 0.02\text{–}0.05)$
- $h^2_\text{between} = h^2_\text{pop} - h^2_\text{within} = 0.09$

This scenario is internally consistent: both $h^2$ components come from the same study and population. It is the more conservative scenario (smaller $h^2$, smaller correction).

### Additive decomposition

$h^2_\text{pop}$ decomposes additively because $h^2_\text{between}$ and $h^2_\text{within}$ are both fractions of total phenotypic variance — the numerators partition the total genetic variance with the same denominator throughout:

$$h^2_\text{pop} = h^2_\text{between} + h^2_\text{within}$$

---

## Step 2. Estimating $R^2$ from the data

We need two $R^2$ estimates from the WLS sibling sample: one for the between-family association between PGI and education, and one for the within-family association.

### Between-family $R^2$

1. Residualize both education and PGI on $\text{poly}(\text{birth\_year}, 2) \times \text{sex}$ and the top 10 genetic principal components (PCs) (Becker et al. 2021).
2. Compute family means of both residuals.
3. Regress mean education residual on mean PGI residual using OLS. $R^2_\text{between}$ is the $R^2$ of this regression.


### Within-family $R^2$

1. Residualize both education and PGI on $\text{poly}(\text{birth\_year}, 2) \times \text{sex}$ only. **No PCs** are included.
2. Demean both residuals within each family, isolating within-family variation.
3. Regress demeaned education on demeaned PGI using OLS. $R^2_\text{within}$ is the $R^2$ of this regression.


### ICC

We estimate the intraclass correlation (ICC) from an intercept-only mixed model, and use it in Step 3 to put $h^2$ and $R^2$ on the same scale.

### Consistency check

Because $R^2_\text{between}$ and $R^2_\text{within}$ use different denominators (between- and within-family variance respectively), they combine as a weighted average to give the population-level $R^2$:

$$R^2_\text{pop} = \text{ICC} \times R^2_\text{between} + (1 - \text{ICC}) \times R^2_\text{within}$$

This differs from the additive $h^2$ decomposition because $R^2$ components use level-specific denominators while $h^2$ components all use total variance. Becker et al. report $R^2_\text{pop} = 0.063$; our estimate is $R^2_\text{pop} = 0.065$.

---

## Step 3. Computing $\rho$

$h^2_\text{between}$ and $h^2_\text{within}$ from the literature are fractions of *total* phenotypic variance. $R^2_\text{between}$ and $R^2_\text{within}$ from our data are fractions of *between-* and *within-family* variance respectively. We convert $R^2$ to total-variance scale before dividing:

$$\rho_\text{between} = \sqrt{\frac{h^2_\text{between}}{\text{ICC} \times R^2_\text{between}}}, \qquad \rho_\text{within} = \sqrt{\frac{h^2_\text{within}}{(1 - \text{ICC}) \times R^2_\text{within}}}$$

---

## Step 4. Applying the correction

See `PGI_RC_derivations.md` for the full derivation.

### Notation

| Symbol | Definition |
|--------|-----------|
| $\sigma^2_\text{tot}$ | Total outcome variance: $\sigma^2(\tau_j) + \sigma^2(\delta_{ij})$, from M0 |
| $\sigma^2(\tau_j)$ | Between-family variance from M0 (intercept only) |
| $\sigma^2(\delta_{ij})$ | Within-family variance from M0 |
| $\sigma^2(\tau_j^*)$ | Between-family variance from M1 (conditioning on PGI) |
| $\sigma^2(\delta_{ij}^*)$ | Within-family variance from M1 |
| $\sigma^2(\tau_j')$ | Between-family variance from M2 (conditioning on PGI + ascribed) |
| $\sigma^2(\delta_{ij}')$ | Within-family variance from M2 |
| $\text{Sibcorr}$ | $\sigma^2(\tau_j) / \sigma^2_\text{tot}$ — sibling correlation (from M0, unaffected by PGI noise) |
| $\text{condcorr}$ | $\sigma^2(\tau_j^*) / \sigma^2_\text{tot}$ |
| $w$ | $[\sigma^2(\delta_{ij}^*) - \sigma^2(\delta_{ij}')] / \sigma^2_\text{tot}$ — within-family share explained by ascribed characteristics |
| $\Delta_\text{between}$ | $[\sigma^2(\tau_j) - \sigma^2(\tau_j^*)] / \sigma^2_\text{tot}$ — observed between-family genetic share |
| $\Delta_\text{within}$ | $[\sigma^2(\delta_{ij}) - \sigma^2(\delta_{ij}^*)] / \sigma^2_\text{tot}$ — observed within-family genetic share |
| $\rho_\text{between}$ | $\sqrt{h^2_\text{between} / (\text{ICC} \times R^2_\text{between})}$ |
| $\rho_\text{within}$ | $\sqrt{h^2_\text{within} / ((1 - \text{ICC}) \times R^2_\text{within})}$ |

### Corrected IOLib

$$\text{IOLib}_\text{RC} = \text{IOLib} - (\rho^2_\text{between} - 1)\,\Delta_\text{between}$$

Since $\rho_\text{between} > 1$, $\text{IOLib}_\text{RC} < \text{IOLib}$: the noisy PGI left too much variance in $\sigma^2(\tau_j^*)$, over-attributing shared-family-environment variance to the liberal measure.

### Corrected IORad

$$\text{IORad}_\text{RC} = \text{Sibcorr} + \rho^2_\text{within}\,\Delta_\text{within} + w$$

Since $\rho_\text{within} > 1$, $\text{IORad}_\text{RC} > \text{IORad}$: the within-family genetic contribution was underestimated.

### Verification

$$\text{IORad}_\text{RC} - \text{IOLib}_\text{RC} = \rho^2_\text{between}\,\Delta_\text{between} + \rho^2_\text{within}\,\Delta_\text{within}$$


---

## Methodological notes

**Consider both $h^2$ scenarios** The two sources (Becker vs. Howe) differ both in population and in method (GREML on WLS vs. within-family design in cohorts including MoBa). For WLS, Becker's estimate is sample-specific but has a large SE (0.103) and requires an assumption to obtain $h^2_\text{within}$. Howe's estimate is more precisely measured for the within-family component but comes from a different population.

**Uncertainty in estimation.** In the current method the correction factors  $\rho_{between}$ and $\rho_{within}$ are held fixed at their full-sample values across iterations. The variance components (and hence Δ_between, Δ_within, w, Sibcorr) vary across bootstrap samples. R²_between and R²_within are estimated from the full sample and held fixed.

Considerations: First, bootstrap SEs capture sampling variability in the variance components but not uncertainty in the external $h^2$ estimates. How to consider this too? Second, $R^2$ could be recomputed on each bootstrap resample. In practice the impact is small: $R^2_{between}$ is estimated from N_families observations and is more stable than the lmer variance components that drive most bootstrap variance. In addition, having a single correction factor for reporting is clearer conceptually.

**$h^2_\text{within}$ scaling for Becker is an assumption.** The proportional scaling $h^2_{\text{within},\text{Becker}} = 0.04 \times (0.172/0.13)$ assumes the within/total heritability ratio is the same in WLS as in MoBa. The alternative — using Howe's $h^2_\text{within} = 0.04$ unchanged with Becker's $h^2_\text{pop} = 0.172$ — would be internally inconsistent, artificially inflating $h^2_\text{between}$.


## References
A) Becker, Joel, et al. "Resource profile and user guide of the Polygenic Index Repository." Nature human behaviour 5.12 (2021): 1744-1758.
B) Howe, Laurence J., et al. "Within-sibship genome-wide association analyses decrease bias in estimates of direct genetic effects." Nature genetics 54.5 (2022): 581-592.