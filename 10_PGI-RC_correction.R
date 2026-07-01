#####################################################################
###################### PGI-RC CORRECTION ############################
#####################################################################
# SET WD
setwd("~/Library/CloudStorage/OneDrive-UniversitédeLausanne/UNIL/projects/GenesSkills/EQUALOPP_WLS")
set.seed(123)
source("00_MASTER.R")

# Applies the Becker et al. (2021) measurement-error correction 


## CONFIG ---------------------------------------------------------------

outcome <- "education"
pgis_rc <- c("pgi_education")

# if WLS also set:
n_boot  <- 500
compute <- T


## READ DATA ------------------------------------------------------------

siblings <- readRDS(paste0("data/siblings_", outcome, ".rds"))
siblings <- siblings %>% mutate_if(is.numeric, scale)



## PARAMETERS ---------------------------------------------------

# ========= H^2: external estimates =========

# ** h²_population
h2_pop_Becker <- 0.172   # Becker et al. (2021, Supp. Table 4) — WLS-specific GREML
h2_pop_Howe   <- 0.13    # Howe et al. (2022, Nature Genetics) - Includes MoBa cohorts

# ** h²_within
# Howe et al. (2022) reports h²_within = 0.04 directly.
# Becker does not report h²_within; we scale Howe's estimate proportionally to
# Becker's larger h²_pop, assuming constant within/total ratio across populations.
h2_within_Howe   <- 0.04
h2_within_Becker <- h2_within_Howe * (h2_pop_Becker / h2_pop_Howe)   # ≈ 0.053

# ** h²_Between = h²_population − h²_within (additive decomposition)
h2_Between_Becker <- h2_pop_Becker - h2_within_Becker   # ≈ 0.119
h2_Between_Howe   <- h2_pop_Howe   - h2_within_Howe     # 0.09



# ========= R^2: sample-based estimates =========

# **** Between-family R² ****

# Residualize on poly(birth_year,2)*sex + PCs (Becker et al. 2021)
pc_vars        <- paste(PC[PC %in% names(siblings)], collapse = " + ")
resid_vars_bet <- paste0("poly(birth_year, 2) * sex + ", pc_vars)

siblings_bet <- siblings %>%
  mutate(
    edu_resid = resid(lm(as.formula(paste(outcome,         "~", resid_vars_bet)), data = siblings)),
    pgi_resid = resid(lm(as.formula(paste("pgi_education", "~", resid_vars_bet)), data = siblings))
  )

# Take family means
siblings_means <- siblings_bet %>%
  group_by(familyID) %>%
  summarise(edu_resid = mean(edu_resid, na.rm = TRUE),
            pgi_resid = mean(pgi_resid, na.rm = TRUE))

# Extract R^2 between
r2_Between <- summary(lm(edu_resid ~ pgi_resid, data = siblings_means))$r.squared


# **** Within-family R² ****

# Reresidualize on poly(birth_year,2)*sex only — no PCs
resid_vars_wit <- "poly(birth_year, 2) * sex"

siblings_wit <- siblings %>%
  mutate(
    edu_resid = resid(lm(as.formula(paste(outcome,         "~", resid_vars_wit)), data = siblings)),
    pgi_resid = resid(lm(as.formula(paste("pgi_education", "~", resid_vars_wit)), data = siblings))
  )
  
# Demean within family
siblings_wit <- siblings_wit %>%
  group_by(familyID) %>%
  mutate(edu_resid = edu_resid - mean(edu_resid, na.rm = TRUE),
         pgi_resid = pgi_resid - mean(pgi_resid, na.rm = TRUE)) %>%
  ungroup()

# Extract R^2 within
r2_within <- summary(lm(edu_resid ~ pgi_resid, data = siblings_wit))$r.squared


# **** ICC **** 
# From M0 on the residualized outcome (check consistency with Becker's)
m0_icc  <- lmer(as.formula(paste(outcome, "~ 1 + (1 | familyID)")), data = siblings_bet)
vc_icc  <- as.data.frame(VarCorr(m0_icc))
ICC     <- vc_icc[vc_icc$grp == "familyID", "vcov"] / sum(vc_icc$vcov)

# Implied R^2 total
r2_pop_implied <- ICC * r2_Between + (1 - ICC) * r2_within


# **** Compute rho (correction factor)
#
# SCALE NOTE: h²_between and h²_within are fractions of TOTAL phenotypic variance,
# while r2_Between and r2_within are fractions of BETWEEN- and WITHIN-family
# variance respectively. Dividing directly would mix scales and underestimate ρ.
# Convert both R² to fractions of total variance first:
#   R²_between_total = ICC  × r2_Between
#   R²_within_total  = (1−ICC) × r2_within
# Then ρ = sqrt(h² / R²_total), with both on the same (total-variance) scale.

rho_Between_Becker <- sqrt(h2_Between_Becker  / (ICC       * r2_Between))
rho_within_Becker  <- sqrt(h2_within_Becker   / ((1 - ICC) * r2_within))
rho_Between_Howe   <- sqrt(h2_Between_Howe    / (ICC       * r2_Between))
rho_within_Howe    <- sqrt(h2_within_Howe     / ((1 - ICC) * r2_within))

cat("\n--- Data parameters ---\n")
print(data.frame(
  Parameter   = c("R2_between (conditional)", "R2_between (total-scale)",
                  "R2_within  (conditional)", "R2_within  (total-scale)",
                  "ICC", "R2_pop (implied)", "R2_pop (Becker reported)"),
  Value       = round(c(r2_Between,        ICC * r2_Between,
                        r2_within,         (1 - ICC) * r2_within,
                        ICC,               r2_pop_implied,   0.063), 3)
), row.names = FALSE)

cat("\n--- Scenario parameters ---\n")
print(data.frame(
  Scenario    = c("Becker", "Howe"),
  h2_pop      = c(h2_pop_Becker,      h2_pop_Howe),
  h2_between  = round(c(h2_Between_Becker,  h2_Between_Howe),  3),
  h2_within   = round(c(h2_within_Becker,   h2_within_Howe),   3),
  rho_between = round(c(rho_Between_Becker, rho_Between_Howe), 3),
  rho_within  = round(c(rho_within_Becker,  rho_within_Howe),  3)
), row.names = FALSE)
cat("\n")


## POSSIBLE SCENARIOS ------------------------------------------------------------

#   A) Becker h²_pop = 0.172; h²_within scaled proportionally from Howe (≈ 0.053)
#   B) Howe   h²_pop = 0.13;  h²_within directly from Howe (0.04)

scenarios <- list(
  A = list(name = "Becker", rho_Between = rho_Between_Becker, rho_within  = rho_within_Becker),
  B = list(name = "Howe",   rho_Between = rho_Between_Howe,   rho_within  = rho_within_Howe)
)





## FUNCTIONS ------------------------------------------------------------

est_fun_rc <- function(family_ids, indices, outcome, natural_talents, pgis, full_data,
                       rho_Between, rho_within) {

  sampled_families <- family_ids[indices]
  data_sample      <- full_data[full_data$familyID %in% sampled_families, ]

  famID     <- "+ (1 | familyID)"
  ascr_vars <- paste(ASCRIBED[ASCRIBED %in% names(data_sample)], collapse = " + ")
  pgi_vars  <- paste(pgis, collapse = " + ")

  m0_vars <- "1"
  m1_vars <- paste0("(", pgi_vars, ")")
  m2_vars <- paste0("(", pgi_vars, " + ", ascr_vars, ")")

  m0 <- lmer(as.formula(paste(outcome, "~", m0_vars, famID)), data = data_sample)
  m1 <- lmer(as.formula(paste(outcome, "~", m1_vars, famID)), data = data_sample)
  m2 <- lmer(as.formula(paste(outcome, "~", m2_vars, famID)), data = data_sample)

  vcov_m0 <- as.data.frame(VarCorr(m0))
  vcov_m1 <- as.data.frame(VarCorr(m1))
  vcov_m2 <- as.data.frame(VarCorr(m2))

  emptyind    <- vcov_m0[vcov_m0$grp == "Residual", "vcov"]
  emptyfam    <- vcov_m0[vcov_m0$grp == "familyID", "vcov"]
  totalvar    <- emptyfam + emptyind
  condind     <- vcov_m1[vcov_m1$grp == "Residual", "vcov"]
  condfam     <- vcov_m1[vcov_m1$grp == "familyID", "vcov"]
  completeind <- vcov_m2[vcov_m2$grp == "Residual", "vcov"]

  Sibcorr  <- emptyfam / totalvar
  condcorr <- condfam  / totalvar
  w        <- (condind - completeind) / totalvar
  IOLIB <- w + condcorr

  gap_Between <- (emptyfam - condfam) / totalvar
  gap_within  <- (emptyind - condind) / totalvar

  # ==== Apply correction ====
  IOLIB_RC <- IOLIB - (rho_Between^2 - 1) * gap_Between
  IORAD_RC <- Sibcorr + w + rho_within^2 * gap_within
  # ==========================

  return(c(Sibcorr, IOLIB_RC, IORAD_RC))
}


compute_indexes_bootstrap_rc <- function(dataset, n_boot, outcome, pgis,
                                         rho_Between, rho_within) {

  bootstrap_results <- boot(
    data            = unique(dataset$familyID),
    statistic       = est_fun_rc,
    R               = n_boot,
    outcome         = outcome,
    natural_talents = "PGI",
    pgis            = pgis,
    full_data       = dataset,
    rho_Between     = rho_Between,
    rho_within      = rho_within
  )

  boot_estimates  <- data.frame(bootstrap_results$t)
  names(boot_estimates) <- INDICES

  point_estimates <- data.frame(t(bootstrap_results$t0))
  names(point_estimates) <- INDICES

  boot_estimates  <- boot_estimates  %>% mutate(diff = IORAD - IOLIB)
  point_estimates <- point_estimates %>% mutate(diff = IORAD - IOLIB)

  results <- map_df(names(boot_estimates), function(index) {
    boots   <- boot_estimates[[index]]
    value   <- point_estimates[[index]]
    se_boot <- sd(boots)
    CI      <- quantile(boots, c(.025, .975))
    z       <- value / se_boot
    p_val   <- 1 - pnorm(z)
    data.frame("Index"  = index,          "Outcome" = outcome,       "Estimate" = value,
               "SE"     = se_boot,
               "Lower"  = CI[["2.5%"]],   "Upper"   = CI[["97.5%"]], "pval"    = p_val,
               "N"      = nrow(dataset),
               "rhoB"   = rho_Between,
               "rhoW"   = rho_within)
  })

  return(results)
}


## RUN ------------------------------------------------------------------

if (compute) {
  
  results_list <- mclapply(scenarios, function(s) {
    # Compute
    res_pci_rc <- compute_indexes_bootstrap_rc(siblings, n_boot, outcome, pgis_rc, s$rho_Between, s$rho_within)
    # Create tag
    # Save
    saveRDS(res_pci_rc, paste0("PGI-RC-correction/results/results_", outcome, "_PGI_RC_", s$name, ".rds"))
  }, mc.cores = 2)

}



## PLOT -----------------------------------------------------------------

source("00a_MOBA_RESULTS.R")

# WLS
ci_rc_Becker <- readRDS(paste0("PGI-RC-correction/results/results_", outcome, "_PGI_RC_Becker.rds"))
ci_rc_Howe   <- readRDS(paste0("PGI-RC-correction/results/results_", outcome, "_PGI_RC_Howe.rds"))
original     <- readRDS(paste0("results/results_", outcome, "_PGI_parents.rds"))

wls_data <- bind_rows(
  original     %>% filter(Dataset == "WLS", Index != "diff") %>% mutate(Panel = "Original"),
  ci_rc_Becker %>% filter(Index != "diff") %>% mutate(Panel = "Corrected (Becker et al.)"),
  ci_rc_Howe   %>% filter(Index != "diff") %>% mutate(Panel = "Corrected (Howe et al.)"),
) %>%
  mutate(
    Dataset = "WLS",
    Index   = factor(Index, levels = INDICES),
    Panel   = factor(Panel, levels = c("Original", "Corrected (Becker et al.)", "Corrected (Howe et al.)"))
  ) %>%
  mutate_if(is.numeric, round, 2)

moba_data <- moba_pgi_rc %>% mutate(Dataset = "MoBa")

plot_data <- bind_rows(wls_data, moba_data) %>%
  mutate(Dataset = factor(Dataset, levels = c("WLS", "MoBa")))


# Plot resulting indices
ggplot(plot_data, aes(x = Outcome, y = Estimate, fill = Index)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper),
                position = position_dodge(0.7),
                width = 0.25, alpha = 0.9) +
  geom_text(aes(label = Estimate, y = Upper),
            position = position_dodge(width = 0.7),
            vjust = -1, size = 5) +
  labs(x = " ", y = "Inequality of Opportunity in Education \n") +
  facet_grid(Dataset ~ Panel) +
  guides(fill = guide_legend(nrow = 1, byrow = FALSE, title = NULL,
                             keywidth = 1.2, keyheight = 1.2, default.unit = "cm")) +
  theme_bw(base_size = 22) +
  theme(
    axis.title.x    = element_blank(),
    axis.ticks.x    = element_blank(),
    axis.text.x     = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position    = "top",
    strip.background   = element_rect(fill = "white"),
    strip.text         = element_text(size = 18),
    legend.text        = element_text(size = 18),
    legend.spacing.y   = unit(0.1, "pt")
  ) +
  scale_y_continuous(limits = c(0, 0.7), expand = expansion(mult = c(0, 0))) +
  scale_fill_manual(labels = INDICES.labs, values = c("#F0B70F", "#7ABA3A", "#E83B3F"))

ggsave("PGI-RC-correction/plots/PGI_RC_correction.pdf", width = 12, height = 10)



## CHECK WLS -----------------------------------------------------------------

## Population-rho correction of RAD-LIB gap = ρ²_pop × (IORad − IOLib)
## Three sources for ρ_pop:
##   (1) Becker reported: ρ = 1.649 (Supp. Table 4, WLS-specific)
##   (2) Becker h²_pop + our R²_pop: ρ = sqrt(h²_pop_Becker / r2_pop_implied)
##   (3) Howe   h²_pop + our R²_pop: ρ = sqrt(h²_pop_Howe   / r2_pop_implied)
## Compare each corrected gap to our decomposed scenarios
#
#rho_pop_Becker_reported <- 1.649
#rho_pop_Becker_implied  <- sqrt(h2_pop_Becker / r2_pop_implied)
#rho_pop_Howe_implied    <- sqrt(h2_pop_Howe   / r2_pop_implied)
#
## Original gap
#gap_obs <- original %>% filter(Index == "diff",Dataset == "WLS") %>% pull(Estimate)
#
## Corrected gaps
#gap_Becker <- ci_rc_Becker %>% filter(Index == "diff") %>% pull(Estimate)
#gap_Howe   <- ci_rc_Howe %>%   filter(Index == "diff") %>% pull(Estimate)
#
## All estimates
#check_data <- data.frame(
#  Method = factor(c(
#    "Observed",
#    "Single-rho: Becker (reported)",
#    "Single-rho: Becker (implied)",
#    "Decomposed: Becker h²",
#    "Single-rho: Howe (implied)",
#    "Decomposed: Howe h²"
#  ), levels = c(
#    "Observed",
#    "Single-rho: Becker (reported)",
#    "Single-rho: Becker (implied)",
#    "Decomposed: Becker h²",
#    "Single-rho: Howe (implied)",
#    "Decomposed: Howe h²"
#  )),
#  Gap = c(
#    round(gap_obs,2),
#    round(rho_pop_Becker_reported^2 * gap_obs,2),
#    round(rho_pop_Becker_implied^2  * gap_obs,2),
#    round(gap_Becker,2),
#    round(rho_pop_Howe_implied^2    * gap_obs,2),
#    round(gap_Howe,2)
#  ),
#  Source = c("Observed", "Becker (WLS)", "Becker (WLS)", "Becker (WLS)", "Howe (MoBa)", "Howe (MoBa)")
#)
#
#cat("\n--- Single-rho gap check ---\n")
#print(check_data)
#
## Plot
#ggplot(check_data, aes(x = Gap, y = Method, colour = Source)) +
#  geom_vline(xintercept = round(gap_obs,2), linetype = "dashed", colour = "grey50") +
#  geom_point(size = 4) +
#  geom_text(aes(label = round(Gap, 3)), hjust = -0.3, size = 4.5) +
#  scale_colour_manual(values = c(Observed = "black", `Becker (WLS)` = "#E83B3F", `Howe (MoBa)` = "#7ABA3A")) +
#  labs(x = "Corrected gap (IORad - IOLib)", y = NULL,
#       title = "WLS Gap check: single-rho vs decomposed correction") +
#  theme_bw(base_size = 14) +
#  theme(legend.position = "right", panel.grid.minor = element_blank()) +
#  xlim(0, max(check_data$Gap) * 1.15)
#
#ggsave("PGI-RC-correction/plots/PGI_RC_gap_check_WLS.png", width = 9, height = 5)
#
