### Analytical Strategy
We estimate liberal and radical inequality of opportunity using polygenic indices (PGIs) rather than observed skills as our measure of natural talents. Our approach relies on information about siblings.
First, for a given educational outcome E, we estimate an empty multilevel model with individuals i nested within families j:

Eij = α + τj + δij 										(1)

where α is the intercept, and the error term has a family-specific τj and an individual-specific component δij.
Second, we estimate a model that controls for natural talents nt (measured by PGIs) and their pairwise interactions:

Eij = α + ntij +τj*+ δij*									(2)

Third, we additionally control for ascribed characteristics X (gender, age, maternal age at birth, and birth order) that lead to inequality between siblings, including all pairwise interactions among and between natural talents and ascribed characteristics (so that gene-environment interactions are included in our analysis):

Eij = α + ntij + Xij + ntij * Xij + τj’+ δij’							(3)

From the variance components of these three models, we construct our measures of liberal and radical inequality of opportunity. Specifically, we measure liberal inequality of opportunity as:

IOLib = var(τj*) / [var(τj) + var(δij)] + [var(δij*) – var(δij’)]/[var(τj) + var(δij)]		(4)

Liberal inequality of opportunity thus consists of two components, both treated as unjust under the liberal conception. The first, var(τj*) / [var(τj) + var(δij)] is the sibling similarity that remains after natural talents are controlled; it captures the effect of ascribed characteristics shared by siblings The second, [var(δij*) – var(δij’)]/[var(τj) + var(δij)],  is the within-family variance attributable to ascribed characteristics that differ between siblings, such as gender and birth order. Because liberal inequality of opportunity treats both shared and sibling-varying ascribed characteristics as unjust sources of inequality, the two components are summed. 
Our measure of radical inequality of opportunity is:

IORad = var(τj)/[var(τj) + var(δij)] + [var(δij) – var(δij’)]/[var(τj) + var(δij)]			(5)

Radical inequality of opportunity also consists of two components. The first, var(τj)/[var(τj) + var(δij)], measures sibling similarity in education, and captures both shared family background and shared natural talents, which are both unjust from the perspective of radical inequality of opportunity. The second, [var(δij) – var(δij’)]/[var(τj) + var(δij)], is the within-family variance attributable to ascribed characteristics and natural talents that differ between siblings. Of the contribution of natural talents to radical inequality of opportunity, it is this within-family component that exploits the random segregation of genetic variants among siblings and can therefore be interpreted more credibly as causal. The two components are summed to obtain radical inequality of opportunity.
The difference between the two measures is the contribution of natural talents: radical inequality of opportunity exceeds liberal inequality of opportunity by the variance in education attributable to PGIs. Because PGIs enter the measure of  liberal inequality of opportunity only through the terms that are netted out, while they enter the measure of radical inequality of opportunity as a source of unjust inequality, the two measures align with the theoretical distinction: the impact of genetic differences on life chances contributes to radical but not to liberal inequality of opportunity.
We compute 95 percent confidence intervals by bootstrapping with 1,000 iterations in the WLS and 500  in MoBa, where the larger sample yields more stable estimates. We use a clustered bootstrap: when an individual is drawn, all members of their family are included, so that families are effectively resampled and the random-effects structure is preserved. Because radical inequality of opportunity is by definition at least as large as liberal inequality of opportunity, and the two are estimated from nested models, we assess the statistical significance of their difference with a one-sided p-value based on a normal approximation using the bootstrap standard error.
