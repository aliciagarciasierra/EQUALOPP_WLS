# Derivations: PGI-RC Correction

## Notation

| Symbol | Definition |
|--------|-----------|
| $\sigma^2_\text{tot}$ | Total variance in the outcome: $\sigma^2(\tau_j) + \sigma^2(\delta_{ij})$ (from M0) |
| $\sigma^2(\tau_j)$ | Between-family variance from M0 (intercept only) |
| $\sigma^2(\delta_{ij})$ | Within-family (residual) variance from M0 |
| $\sigma^2(\tau_j^*)$ | Between-family variance from M1 (conditioning on PGI) |
| $\sigma^2(\delta_{ij}^*)$ | Within-family variance from M1 (conditioning on PGI) |
| $\sigma^2(\tau_j')$ | Between-family variance from M2 (conditioning on PGI + ascribed) |
| $\sigma^2(\delta_{ij}')$ | Within-family variance from M2 (conditioning on PGI + ascribed) |
| $w$ | $[\sigma^2(\delta_{ij}^*) - \sigma^2(\delta_{ij}')]\,/\,\sigma^2_\text{tot}$ |
| $\text{Sibcorr}$ | $\sigma^2(\tau_j)\,/\,\sigma^2_\text{tot}$ — sibling correlation (from M0, unaffected by PGI noise) |
| $\text{condcorr}$ | $\sigma^2(\tau_j^*)\,/\,\sigma^2_\text{tot}$ |
| $\Delta_\text{between}$ | $[\sigma^2(\tau_j) - \sigma^2(\tau_j^*)]\,/\,\sigma^2_\text{tot}$ — observed between-family genetic share |
| $\Delta_\text{within}$ | $[\sigma^2(\delta_{ij}) - \sigma^2(\delta_{ij}^*)]\,/\,\sigma^2_\text{tot}$ — observed within-family genetic share |
| $\rho_\text{between}$ | $\sqrt{h^2_\text{between}\,/\,(\text{ICC} \times R^2_\text{between})} \geq 1$ |
| $\rho_\text{within}$ | $\sqrt{h^2_\text{within}\,/\,((1-\text{ICC}) \times R^2_\text{within})} \geq 1$ |
| $g^*$ | True additive SNP factor; $\text{PGI} = (g^* + e)/\rho$ where $e$ is uncorrelated noise |

---

## Setup

The PGI is a noisy proxy for the true genetic factor:

$$\text{PGI} = \frac{g^* + e}{\rho}$$

where $e$ is estimation error uncorrelated with $g^*$ and with all other variables. M1 therefore absorbs less variance than it would with the true $g^*$. The observed genetic shares are:

$$\Delta_\text{between} = \frac{\sigma^2(\tau_j) - \sigma^2(\tau_j^*)}{\sigma^2_\text{tot}}, \qquad \Delta_\text{within} = \frac{\sigma^2(\delta_{ij}) - \sigma^2(\delta_{ij}^*)}{\sigma^2_\text{tot}}$$

Conditioning on $g^*$ instead of PGI absorbs $\rho^2$ times more variance at each level:

$$\sigma^2(\tau_j^*)_\text{true} = \sigma^2(\tau_j) - \rho^2_\text{between}\,\Delta_\text{between}\,\sigma^2_\text{tot}$$

$$\sigma^2(\delta_{ij}^*)_\text{true} = \sigma^2(\delta_{ij}) - \rho^2_\text{within}\,\Delta_\text{within}\,\sigma^2_\text{tot}$$

For M2, we approximate that the ascribed-characteristics contribution is unaffected by PGI noise (reasonable when PGI and ascribed variables are weakly correlated):

$$\sigma^2(\delta_{ij}^*)_\text{true} - \sigma^2(\delta_{ij}')_\text{true} \approx \sigma^2(\delta_{ij}^*) - \sigma^2(\delta_{ij}') = w \cdot \sigma^2_\text{tot}$$

---

## Corrected IOLib

Start from the definition (Eq. 4 in the analytical strategy):

$$\text{IOLib} = \frac{\sigma^2(\tau_j^*)}{\sigma^2_\text{tot}} + \frac{\sigma^2(\delta_{ij}^*) - \sigma^2(\delta_{ij}')}{\sigma^2_\text{tot}} = \text{condcorr} + w$$

Replace $\sigma^2(\tau_j^*)$ with the corrected value. From the definition of $\Delta_\text{between}$:

$$\sigma^2(\tau_j^*) = \sigma^2(\tau_j) - \Delta_\text{between}\,\sigma^2_\text{tot}$$

so the true $g^*$ would absorb $\rho^2_\text{between}$ times as much between-family variance:

$$\sigma^2(\tau_j^*)_\text{true} = \sigma^2(\tau_j) - \rho^2_\text{between}\,\Delta_\text{between}\,\sigma^2_\text{tot}$$

Dividing by $\sigma^2_\text{tot}$:

$$\frac{\sigma^2(\tau_j^*)_\text{true}}{\sigma^2_\text{tot}} = \text{Sibcorr} - \rho^2_\text{between}\,\Delta_\text{between}$$

Using the M2 approximation ($w$ is unchanged) and noting that $\text{condcorr} = \text{Sibcorr} - \Delta_\text{between}$:

$$\text{IOLib}_\text{RC} = \text{Sibcorr} - \rho^2_\text{between}\,\Delta_\text{between} + w = \text{condcorr} + \Delta_\text{between} - \rho^2_\text{between}\,\Delta_\text{between} + w$$

$$\boxed{\text{IOLib}_\text{RC} = \text{IOLib} - (\rho^2_\text{between} - 1)\,\Delta_\text{between}}$$

Since $\rho_\text{between} > 1$, IOLib decreases: the noisy PGI left too much variance in $\sigma^2(\tau_j^*)$, over-attributing shared-family-environment variance to the liberal measure.

---

## Corrected IORad

$\text{Sibcorr} = \sigma^2(\tau_j)/\sigma^2_\text{tot}$ comes from M0 (no predictors) and is unaffected by PGI noise. The bias enters through $\sigma^2(\delta_{ij}')_\text{true}$ in M2. Using the M2 approximation:

$$\sigma^2(\delta_{ij}')_\text{true} = \sigma^2(\delta_{ij}^*)_\text{true} - [\sigma^2(\delta_{ij}^*) - \sigma^2(\delta_{ij}')]$$

The corrected within-family variance explained by natural talents and ascribed characteristics is:

$$\sigma^2(\delta_{ij}) - \sigma^2(\delta_{ij}')_\text{true} = \rho^2_\text{within}\,\Delta_\text{within}\,\sigma^2_\text{tot} + [\sigma^2(\delta_{ij}^*) - \sigma^2(\delta_{ij}')]$$

Dividing by $\sigma^2_\text{tot}$ and adding Sibcorr:

$$\boxed{\text{IORad}_\text{RC} = \text{Sibcorr} + \rho^2_\text{within}\,\Delta_\text{within} + w}$$

Since $\rho_\text{within} > 1$, IORad increases: the within-family genetic contribution was underestimated because the noisy PGI absorbed less within-family variance than the true $g^*$ would.

---

## Verification

The corrected gap is $\rho^2$ times each genetic share:

$$\text{IORad}_\text{RC} - \text{IOLib}_\text{RC} = \rho^2_\text{between}\,\Delta_\text{between} + \rho^2_\text{within}\,\Delta_\text{within} $$


---

## Scale correction for ρ

$h^2_\text{between}$ and $h^2_\text{within}$ from the literature are fractions of **total** phenotypic variance. $R^2_\text{between}$ (from the family-means regression) and $R^2_\text{within}$ (from the within-family demeaned regression) are fractions of **between-** and **within-family** variance respectively. Dividing directly mixes scales. The ICC converts each $R^2$ to the total-variance scale:

$$\rho_\text{between} = \sqrt{\frac{h^2_\text{between}}{\text{ICC} \times R^2_\text{between}}}, \qquad \rho_\text{within} = \sqrt{\frac{h^2_\text{within}}{(1 - \text{ICC}) \times R^2_\text{within}}}$$

This is consistent with the additive decomposition of $R^2_\text{pop}$:

$$R^2_\text{pop} = \text{ICC} \times R^2_\text{between} + (1 - \text{ICC}) \times R^2_\text{within}$$

which differs from the additive $h^2$ decomposition ($h^2_\text{pop} = h^2_\text{between} + h^2_\text{within}$) precisely because $R^2$ components use level-specific denominators while $h^2$ components all use total variance as the denominator.
