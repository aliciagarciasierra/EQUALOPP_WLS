##############################################################
######## POWER ANALYSIS — w (within-family ascribed IOP) ####
##############################################################

# Assess power to detect w = (condind - completeind) / totalvar,
# the within-family component of IOLib (ascribed contribution
# beyond PGIs). 

setwd("~/Library/CloudStorage/OneDrive-UniversitédeLausanne/UNIL/projects/GenesSkills/EQUALOPP_WLS")
rm(list = ls())
set.seed(123)
source("00_MASTER.R")


###### ------ CONFIG ----- #######

# compute power analysis?
compute      <- T
# replications for null distribution
n_null       <- 500
# replications per power grid cell (lower than 11_ due to m2 cost)
n_sim        <- 500
# PGI signal: TRUE = all in pgi_education; FALSE = equal across 7 PGIs
concentrated <- T
# TRUE = paper spec (pairwise PGI×PGI and PGI×ascribed interactions)
# FALSE = main effects only (reduced parameter burden; results show negligible difference)
full_interactions <- F


# Number of PGIs in analysis
n_pgis <- length(PGIs)
# observed: all WLS sibling families have exactly 2 members
mean_sib     <- 2

# Where to run analysis
samples <-  c("Brothers","Sisters","Pooled")

# Define grid around observed values
w_grid <- c(0.002, 0.005, 0.008, 0.01, 0.015, 0.03, 0.05)


######################################################
##########  STEP 1: DATA-GENERATING PROCESS  ########
######################################################

# PGI decomposition: between-family f_j ~ N_indiv(0, 0.5), within-family w_ij ~ N_indiv(0, 0.5).
# Each ASCRIBED variable respects its real-world distribution and between/within structure:
#   sex              — binary Bernoulli(0.5); constant within Brothers/Sisters (var_w = 0)
#   birth_year       — continuous; var_b = 0.70, var_w = 0.30
#   mother_age_birth — continuous; predominantly between families: var_b = 0.85, var_w = 0.15
#   birth_order      — deterministic sequence(n_sibs); var_w = 0.25 for dyads
#   pc1–pc10         — family-level constants (population stratification); var_w = 0
#
# Gamma calibration: equal within-family contribution from each active variable (n_eff) so that their 
# join contribution to within variance is the desired w_true:
#   gamma_k = sqrt(w_true / (n_eff * var_w_k))
#
# Variance accounting (normalised, totalvar = 1):
#   (emptyind - condind)     / totalvar = delta_within
#   (condind  - completeind) / totalvar = w_true
#
# Residual variance components (not explained by PGI or ascribed):
#   sigma2_tau   = icc_obs - delta_within - w_true (family)
#   sigma2_delta = 1 - icc_obs - delta_within - w_true (individual)


simulate_sibling_data_w <- function(n_families,
                                     mean_sib     = 2,
                                     icc_obs      = 0.40,
                                     delta_within = 0.03,
                                     w_true       = 0.02,
                                     n_pgis       = 7,
                                     concentrated = TRUE,
                                     sex_const    = NA_integer_) {

  
  # ======= Sample structure =======
  
  if (icc_obs - delta_within - w_true < 0 || 1 - icc_obs - delta_within - w_true < 0)
    stop("w_true + delta_within too large for icc_obs: residual variances would go negative.")

  # Sample size and structure
  n_sibs  <- rep(as.integer(mean_sib), n_families)
  N_indiv <- sum(n_sibs)
  fam_id  <- rep(seq_len(n_families), n_sibs)

  # Residual variance components
  sigma2_tau   <- icc_obs - delta_within - w_true
  sigma2_delta <- 1 - icc_obs - delta_within - w_true

  # Family level noise independent of PGIs and ascribed variables
  tau <- rnorm(n_families, 0, sqrt(sigma2_tau))

  
  # ======= PGIs =======
  
  # * Set PGI weights so that within-family variance of pgi_score equals delta_within
  beta_vec <- if (concentrated) {
    # Signal concentrated in one PGI (education, as in data)
    c(sqrt(delta_within / 0.5), rep(0, n_pgis - 1))
    # Signal equally distributed across PGIs
  } else rep(sqrt(delta_within / (n_pgis * 0.5)), n_pgis)

  # * PGI values simulation between and within
  between_draws <- rnorm(n_families * n_pgis, mean = 0, sd = sqrt(0.5))
  within_draws  <- rnorm(N_indiv    * n_pgis, mean = 0, sd = sqrt(0.5))
  pgi_between   <- matrix(between_draws, nrow = n_families, ncol = n_pgis)
  pgi_within    <- matrix(within_draws,  nrow = N_indiv,          ncol = n_pgis)
  
  # * Sum between and within variation of simualted PGIs
  pgi_mat           <- pgi_between[fam_id, ] + pgi_within
  colnames(pgi_mat) <- paste0("pgi", seq_len(n_pgis))
  
  # * Weight by betas
  pgi_score <- as.vector(pgi_mat %*% beta_vec)

  
  # ======= Ascribed variables =======
  
  # * Each variable is split into between-family and within-family components.
  # Variance proportions calibrated to real-world structure (var_b + var_w = total variance).

  var_b_birth_year       <- 0.70; var_w_birth_year       <- 0.30
  var_b_mother_age_birth <- 0.85; var_w_mother_age_birth <- 0.15
  var_w_birth_order      <- 0.25   # purely within-family (deterministic sequence)
  var_w_sex              <- if (!is.na(sex_const)) 0 else 0.125   # 0 when sex is constant

  # sex: binary, constant in Brothers/Sisters, random in Pooled
  sex <- if (!is.na(sex_const)) rep(as.integer(sex_const), N_indiv) else rbinom(N_indiv, 1, 0.5)

  # birth_year: more between than within (narrow WLS birth cohort)
  birth_year_between <- rnorm(n_families, mean = 0, sd = sqrt(var_b_birth_year))
  birth_year_within  <- rnorm(N_indiv,    mean = 0, sd = sqrt(var_w_birth_year))
  birth_year         <- birth_year_between[fam_id] + birth_year_within

  # mother_age_birth: predominantly between families
  mother_age_between <- rnorm(n_families, mean = 0, sd = sqrt(var_b_mother_age_birth))
  mother_age_within  <- rnorm(N_indiv,    mean = 0, sd = sqrt(var_w_mother_age_birth))
  mother_age_birth   <- mother_age_between[fam_id] + mother_age_within

  # birth_order: purely within-family (deterministic sequence per family)
  birth_order <- as.numeric(sequence(n_sibs))

  # pc1-pc10: purely between-family (population stratification shared by all siblings)
  pc_between       <- matrix(rnorm(n_families * 10, mean = 0, sd = 1), nrow = n_families, ncol = 10)
  pc_mat           <- pc_between[fam_id, ]
  colnames(pc_mat) <- paste0("pc", 1:10)

  # * Gamma calibration: equal within-family contribution from each active variable
  var_w_vec <- c(sex              = var_w_sex, 
                 birth_year       = var_w_birth_year,
                 mother_age_birth = var_w_mother_age_birth,
                 birth_order      = var_w_birth_order,
                 setNames(rep(0, 10), paste0("pc", 1:10)))   # Notice that PCs do not vary across siblings
  # Active effects within family
  n_eff     <- sum(var_w_vec > 0)
  # Resulting weight vector for within family ascribed contribution 
  gamma_vec <- ifelse(var_w_vec > 0, sqrt(w_true / (n_eff * var_w_vec)), 0)
  # Full data of simulated ascribed characteristics
  asc_df    <- data.frame(sex              = sex, 
                          birth_year       = birth_year,
                          mother_age_birth = mother_age_birth,
                          birth_order      = birth_order,
                          as.data.frame(pc_mat))
  # * Weight by gammas
  asc_score <- as.vector(as.matrix(asc_df) %*% gamma_vec)

  # ======= Simulated outcome =======
  
  # Random noise
  eps <- rnorm(N_indiv, 0, sqrt(sigma2_delta))
  # Simulated education
  E   <- pgi_score + asc_score + tau[fam_id] + eps
  # Full simulated data
  cbind(data.frame(fam_id = fam_id, E = E), as.data.frame(pgi_mat), asc_df)
}


######################################################
##########  STEP 2: FIT MODELS & EXTRACT VCs  #######
######################################################

# Always fits all three models. Returns w, IOLIB, and IORAD.

fit_io_models_w <- function(df, n_pgis = 7, full_interactions = TRUE) {

  pgi_names <- paste0("pgi", seq_len(n_pgis))
  asc_names <- ASCRIBED[ASCRIBED %in% names(df)]
  famID     <- "(1 | fam_id)"
  pgi_vars  <- paste(pgi_names,  collapse = " + ")
  ascr_vars <- paste(asc_names, collapse = " + ")

  f_m0 <- as.formula(paste("E ~ 1 +", famID))
  f_m1 <- if (full_interactions)
    as.formula(paste("E ~", paste0("(", pgi_vars, ")^2"), "+", famID))
  else
    as.formula(paste("E ~", pgi_vars, "+", famID))
  f_m2 <- if (full_interactions)
    as.formula(paste("E ~", paste0("(", pgi_vars, " + ", ascr_vars, ")^2"), "+", famID))
  else
    as.formula(paste("E ~", pgi_vars, "+", ascr_vars, "+", famID))

  m0 <- lmer(f_m0, data = df, REML = TRUE)
  m1 <- lmer(f_m1, data = df, REML = TRUE)
  m2 <- lmer(f_m2, data = df, REML = TRUE)

  vc0 <- as.data.frame(VarCorr(m0))
  vc1 <- as.data.frame(VarCorr(m1))
  vc2 <- as.data.frame(VarCorr(m2))

  emptyind    <- vc0$vcov[vc0$grp == "Residual"]
  emptyfam    <- vc0$vcov[vc0$grp == "fam_id"]
  totalvar    <- emptyfam + emptyind
  condind     <- vc1$vcov[vc1$grp == "Residual"]
  condfam     <- vc1$vcov[vc1$grp == "fam_id"]
  completeind <- vc2$vcov[vc2$grp == "Residual"]

  Sibcorr  <- emptyfam / totalvar
  condcorr <- condfam  / totalvar
  w        <- (condind  - completeind) / totalvar
  v        <- (emptyind - completeind) / totalvar

  list(w = w)
}


######################################################
##########  STEP 3: SINGLE-CELL SIMULATION  #########
######################################################

# Runs n_sim replications for one w_true value.
# delta_within is held fixed at the observed value for each sample.
# Returns data.frame with columns w and IORAD across replications.

run_sim_cell_w <- function(n_families, delta_within, w_true,
                            n_sim        = 100,
                            mean_sib     = 2,
                            icc_obs      = 0.40,
                            n_pgis       = 7,
                            concentrated = TRUE,
                            sex_const    = NA_integer_) {

  # Function to generate estimates
  one_rep <- function(i) {
    # Simulate data
    df  <- simulate_sibling_data_w(n_families, mean_sib, icc_obs, delta_within,
                                    w_true, n_pgis, concentrated, sex_const)
    # Fit models
    out <- fit_io_models_w(df, n_pgis, full_interactions)
    # Extract estimates
    data.frame(w = out$w)
  }

  # Apply function and return NULL if an iteration fails
  results <- lapply(seq_len(n_sim), function(i) {
    tryCatch(one_rep(i), error = function(e) NULL)
  })

  # Return non null results
  do.call(rbind, Filter(Negate(is.null), results))
}


######################################################
###############  RUN POWER ANALYSIS ##################
######################################################


# ------ PARAMETERS CALIBRATED FROM DATA ----- 

# Sample structure
sibs <- readRDS("data/siblings_education.rds")
N_fam_brothers <- sibs |> group_by(familyID) |> filter(all(sex == 0)) |> pull(familyID) |> n_distinct()
N_fam_sisters  <- sibs |> group_by(familyID) |> filter(all(sex == 1)) |> pull(familyID) |> n_distinct()
N_fam_pooled   <- sibs |> pull(familyID) |> n_distinct()
n_fam_lookup <- c(Brothers = N_fam_brothers, Sisters = N_fam_sisters, Pooled = N_fam_pooled)

# Variance components
if (full_interactions) {
  xlsx_lookup <- c(
    Brothers = "results/by_outcome/full_results_education_PGI_Brothers.xlsx",
    Sisters  = "results/by_outcome/full_results_education_PGI_Sisters.xlsx",
    Pooled   = "results/by_outcome/full_results_education_PGI.xlsx"
  )
} else {
  xlsx_lookup <- c(
    Brothers = "results/nointeractions/full_results_education_PGI_Brothers.xlsx",
    Sisters  = "results/nointeractions/full_results_education_PGI_Sisters.xlsx",
    Pooled   = "results/nointeractions/full_results_education_PGI.xlsx"
  )
}
# - function to extract values
read_vc_params <- function(path) {
  results  <- read_xlsx(path, sheet = "Full results")
  null     <- results[results$Model == "NULL MODEL",        ]
  cond     <- results[results$Model == "CONDITIONAL MODEL", ]
  complete <- results[results$Model == "COMPLETE MODEL",    ]
  list(icc          = null$emptyfam / null$totalvar,
       delta_within = (null$emptyind - cond$condind)        / null$totalvar,
       w_obs        = (cond$condind  - complete$completeind) / null$totalvar)
}


# ------ LOOP OVER ALL SAMPLES ----- 

# w_observed:
# brothers: 0.007771967
# sisters: 0.02499889
# pooled: 0.01754966

for (sample_tag in samples) {

  # Extract parameters calibrated from specific sample
  params       <- read_vc_params(xlsx_lookup[sample_tag])
  n_families   <- n_fam_lookup[sample_tag]
  icc_obs      <- params$icc
  delta_within <- params$delta_within
  w_observed   <- params$w_obs
  sex_const    <- switch(sample_tag, Brothers = 0L, Sisters = 1L, Pooled = NA_integer_)

  cat(glue("\n===== {sample_tag} (N_indiv={n_families}) =====\n"))
  cat(glue("ICC: {round(icc_obs,4)}  |  delta_within: {round(delta_within,4)}  |  w_obs: {round(w_observed,4)}\n"))
  
  # Tags to identify analyses
  int_tag  <- ifelse(full_interactions, "fullint", "noint")
  conc_tag <- ifelse(concentrated, "concentrated", "equal")
  tag      <- glue("{conc_tag}_{sample_tag}_{int_tag}_{n_sim}")
  null_tag <- glue("{conc_tag}_{sample_tag}_{int_tag}_{n_sim}_Null")

  ######################################################
  ##########  STEP 4: NULL DISTRIBUTION  ##############
  ######################################################

  # Null: w_true = 0 (ascribed add nothing within families beyond PGIs).
  # delta_within is held at observed value — PGI signal is always present.
  
  
  if (compute) {
      
    cat("Estimating null distributions for w...\n")
    # Run simulations
    null_draws <- run_sim_cell_w(n_families,
                                 delta_within = delta_within, w_true = 0,
                                 n_sim = n_null, mean_sib = mean_sib,
                                 icc_obs = icc_obs, n_pgis = n_pgis,
                                 concentrated = concentrated, sex_const = sex_const)
    # Extract critical values
    # -- 95th percentile of null estimates: values this large occur by chance 5% of the time.
    # -- An observed estimate above this threshold means rejection of the null (one-sided alpha = 0.05).
    crit <- c(w = quantile(null_draws$w, 0.95))
    # Save
    saveRDS(crit, glue("PowerAnalysis/results/nulls/power_w_{null_tag}.rds") )
    
  
    ######################################################
    ##########  STEP 5: POWER CURVE  ####################
    ######################################################
  
    cat("Running power curve for w...\n")
    
    # Loop over w_grid at fixed n_families and fixed delta_within = observed.
    curve_results <- mclapply(w_grid, function(wv) {
      
      # * Run n_sim simulations
      draws <- run_sim_cell_w(n_families,
                               delta_within = delta_within, w_true = wv,
                               n_sim = n_sim, mean_sib = mean_sib,
                               icc_obs = icc_obs, n_pgis = n_pgis,
                               concentrated = concentrated, sex_const = sex_const)
      
      # * Compute power: proportion of replications where the true effect w_true was large enough to be detected.
      data.frame(w_true  = wv,
                 power_w = mean(draws$w > crit["w.95%"]))
      
    }, mc.cores = detectCores() - 1)

    # Combine power values for each w value
    power_curve <- do.call(rbind, curve_results)
    # Save
    saveRDS(power_curve, glue("PowerAnalysis/results/power_w_{tag}.rds"))
    cat("Power curve saved.\n")
      
    }

  
  
  ######################################################
  ##########  STEP 6: PLOT  ###########################
  ######################################################
  
  # Read desired power curve
  power_curve <- readRDS(glue("PowerAnalysis/results/power_w_{tag}.rds"))
  
  # Make sure x axis is in order
  power_curve <- power_curve[order(power_curve$w_true), ]
  
  # Linear interpolation to find smallest w_true where power crosses 80%
  idx_above <- which(!is.na(power_curve$power_w) & power_curve$power_w >= 0.80)
  if (length(idx_above) == 0) {
    det_w <- NA_real_
  } else {
    fa    <- min(idx_above)
    det_w <- if (fa == 1) power_curve$w_true[fa] else {
      d_lo <- power_curve$w_true[fa - 1]; d_hi <- power_curve$w_true[fa]
      p_lo <- power_curve$power_w[fa - 1]; p_hi <- power_curve$power_w[fa]
      d_lo + (0.80 - p_lo) / (p_hi - p_lo) * (d_hi - d_lo)
    }
  }
  
  p <- ggplot(power_curve, aes(x = w_true, y = power_w)) +
    geom_line(linewidth = 1.2, color = "steelblue") +
    geom_point(size = 2.5, color = "steelblue") +
    geom_hline(yintercept = 0.80, linetype = "dashed", color = "grey40") +
    annotate("text", x = max(w_grid) * 0.98, y = 0.82, label = "80% power",
             hjust = 1, size = 3, color = "grey40") +
    geom_vline(xintercept = w_observed, linetype = "dotted", color = "red") +
    annotate("text", x = w_observed + 0.001, y = 0.05,
             label = glue("observed = {round(w_observed, 3)}"), hjust = 0, size = 3, color = "red") +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    labs(
      title = glue("Power to detect within-family ascribed contribution - {sample_tag} (N families={n_families})"),
      x     = "True w = (condind - completeind) / totalvar",
      y     = "Power (one-sided, alpha = 0.05)"
    ) +
    theme_bw(base_size = 11)
  
  # If found, add detectable effect to the plot
  if (!is.na(det_w))
    p <- p +
    geom_vline(xintercept = det_w, linetype = "dotted", color = "steelblue") +
    annotate("text", x = det_w + 0.001, y = 0.15,
             label = glue("detectable = {round(det_w, 3)}"), hjust = 0, size = 3, color = "steelblue")
  
  print(p)
  ggsave(glue("PowerAnalysis/plots/power_w_{tag}.pdf"), p, width = 8, height = 5)
  cat(glue("Plot saved: PowerAnalysis/plots/power_w_{tag}.pdf\n"))
  
}



