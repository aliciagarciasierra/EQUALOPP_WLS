##############################################################
######## POWER ANALYSIS — delta_within (within-family PGI IOP) ###
##############################################################

# Assess power to detect delta_within = (emptyind - condind) / totalvar,
# the within-family variance reduction attributable to PGIs (m0 -> m1).

setwd("~/Library/CloudStorage/OneDrive-UniversitédeLausanne/UNIL/projects/GenesSkills/EQUALOPP_WLS")
rm(list = ls())
set.seed(123)
source("00_MASTER.R")


###### ------ CONFIG ----- #######

# compute power analysis?
compute      <- F
# replications for null distribution
n_null       <- 500
# replications per power grid cell
n_sim        <- 500
# observed: all WLS sibling families have exactly 2 members
mean_sib     <- 2
# PGI signal: TRUE = all in pgi_education; FALSE = equal across 7 PGIs
concentrated <- T

# Grid of delta_within effect sizes to sweep
delta_grid <- seq(0.01, 0.06, by = 0.01)
n_pgis     <- length(PGIs)

# Where to run analysis
samples <- c("Brothers", "Sisters")


######################################################
##########  STEP 1: DATA-GENERATING PROCESS  ########
######################################################

# PGI decomposition: between-family f_j ~ N(0, 0.5), within-family w_ij ~ N(0, 0.5).
# PGI coefficients are calibrated so that within-family PGI contribution equals exactly delta_within.
#
# Variance accounting (normalised, totalvar = 1):
#   (emptyind - condind) / totalvar = delta_within
#
# Residual variance components (not explained by PGIs):
#   sigma2_tau   = icc_obs - delta_within (family)
#   sigma2_delta = 1 - icc_obs - delta_within (individual)

simulate_sibling_data <- function(n_families,
                                   mean_sib     = 2,
                                   icc_obs      = 0.40,
                                   delta_within = 0.03,
                                   n_pgis       = 7,
                                   concentrated = TRUE) {

  # ======= Sample structure =======

  if (icc_obs - delta_within < 0 || 1 - icc_obs - delta_within < 0)
    stop("delta_within too large for icc_obs: residual variances would go negative.")

  n_sibs  <- rep(as.integer(mean_sib), n_families)
  N_indiv <- sum(n_sibs)
  fam_id  <- rep(seq_len(n_families), n_sibs)

  # Residual variance components
  sigma2_tau   <- icc_obs - delta_within
  sigma2_delta <- 1 - icc_obs - delta_within

  # Family level noise independent of PGIs
  tau <- rnorm(n_families, mean = 0, sd = sqrt(sigma2_tau))


  # ======= PGIs =======

  # * Set PGI weights so that within-family variance of pgi_score equals delta_within
  beta_vec <- if (concentrated) {
    # Signal concentrated in one PGI (education, as in data)
    c(sqrt(delta_within / 0.5), rep(0, n_pgis - 1))
  } else {
    # Signal equally distributed across PGIs
    rep(sqrt(delta_within / (n_pgis * 0.5)), n_pgis)
  }

  # * PGI values: between and within components
  between_draws <- rnorm(n_families * n_pgis, mean = 0, sd = sqrt(0.5))
  within_draws  <- rnorm(N_indiv    * n_pgis, mean = 0, sd = sqrt(0.5))
  pgi_between   <- matrix(between_draws, nrow = n_families, ncol = n_pgis)
  pgi_within    <- matrix(within_draws,  nrow = N_indiv,    ncol = n_pgis)

  # * Sum between and within components
  pgi_mat           <- pgi_between[fam_id, ] + pgi_within
  colnames(pgi_mat) <- paste0("pgi", seq_len(n_pgis))

  # * Weight by betas
  pgi_score <- as.vector(pgi_mat %*% beta_vec)


  # ======= Simulated outcome =======

  # Random noise
  eps <- rnorm(N_indiv, mean = 0, sd = sqrt(sigma2_delta))
  # Simulated education
  E   <- pgi_score + tau[fam_id] + eps
  # Full simulated data
  cbind(data.frame(fam_id = fam_id, E = E), as.data.frame(pgi_mat))
}


######################################################
##########  STEP 2: FIT MODELS & EXTRACT VCs  #######
######################################################

# Fits m0 (null) and m1 (conditional on PGIs) using REML.
# Returns delta_within = (emptyind - condind) / totalvar.

fit_io_models <- function(df, n_pgis = 7) {

  pgi_names <- paste0("pgi", seq_len(n_pgis))
  famID     <- "(1 | fam_id)"
  pgi_vars  <- paste(pgi_names, collapse = " + ")

  f_m0 <- as.formula(paste("E ~ 1 +", famID))
  f_m1 <- as.formula(paste("E ~", paste0("(", pgi_vars, ")^2"), "+", famID))

  m0 <- suppressWarnings(lmer(f_m0, data = df, REML = TRUE))
  m1 <- suppressWarnings(lmer(f_m1, data = df, REML = TRUE))

  vc0 <- as.data.frame(VarCorr(m0))
  vc1 <- as.data.frame(VarCorr(m1))

  emptyind <- vc0$vcov[vc0$grp == "Residual"]
  emptyfam <- vc0$vcov[vc0$grp == "fam_id"]
  totalvar <- emptyfam + emptyind
  condind  <- vc1$vcov[vc1$grp == "Residual"]

  list(delta_within = (emptyind - condind) / totalvar)
}


######################################################
##########  STEP 3: SINGLE-CELL SIMULATION  #########
######################################################

# Runs n_sim replications for one delta_within value.
# Returns data.frame with column delta_within across replications.

run_sim_cell <- function(n_families, delta_within,
                          n_sim        = 500,
                          mean_sib     = 2,
                          icc_obs      = 0.40,
                          n_pgis       = 7,
                          concentrated = TRUE) {

  one_rep <- function(i) {
    df  <- simulate_sibling_data(n_families, mean_sib, icc_obs, delta_within,
                                  n_pgis, concentrated)
    out <- fit_io_models(df, n_pgis)
    data.frame(delta_within = out$delta_within)
  }

  results <- lapply(seq_len(n_sim), function(i) {
    tryCatch(one_rep(i), error = function(e) NULL)
  })

  do.call(rbind, Filter(Negate(is.null), results))
}


######################################################
##########  RUN POWER ANALYSIS  #####################
######################################################

# ------ PARAMETERS CALIBRATED FROM DATA ------

sibs           <- readRDS("data/siblings_education.rds")
N_fam_brothers <- sibs |> group_by(familyID) |> filter(all(sex == 0)) |> pull(familyID) |> n_distinct()
N_fam_sisters  <- sibs |> group_by(familyID) |> filter(all(sex == 1)) |> pull(familyID) |> n_distinct()
N_fam_pooled   <- sibs |> pull(familyID) |> n_distinct()
n_fam_lookup   <- c(Brothers = N_fam_brothers, Sisters = N_fam_sisters, Pooled = N_fam_pooled)

xlsx_lookup <- c(
  Brothers = "results/by_outcome/full_results_education_PGI_Brothers.xlsx",
  Sisters  = "results/by_outcome/full_results_education_PGI_Sisters.xlsx",
  Pooled   = "results/by_outcome/full_results_education_PGI.xlsx"
)

read_vc_params <- function(path) {
  results <- read_xlsx(path, sheet = "Full results")
  null    <- results[results$Model == "NULL MODEL",        ]
  cond    <- results[results$Model == "CONDITIONAL MODEL", ]
  list(icc          = null$emptyfam / null$totalvar,
       delta_within = (null$emptyind - cond$condind) / null$totalvar)
}


# ------ LOOP OVER ALL SAMPLES ------

for (sample_tag in samples) {

  params         <- read_vc_params(xlsx_lookup[sample_tag])
  n_families     <- n_fam_lookup[sample_tag]
  icc_obs        <- params$icc
  delta_observed <- params$delta_within

  cat(glue("\n===== {sample_tag} (N={n_families}) =====\n"))
  cat(glue("ICC: {round(icc_obs, 4)}  |  delta_within: {round(delta_observed, 4)}\n"))

  # tags to identify analysis
  conc_tag <- ifelse(concentrated, "concentrated", "equal")
  tag      <- glue("{conc_tag}_{sample_tag}_{n_sim}")
  null_tag <- glue("{conc_tag}_{sample_tag}_{n_sim}_Null")

  ######################################################
  ##########  STEP 4: NULL DISTRIBUTION  ##############
  ######################################################

  # Null: delta_within = 0 (PGIs add no within-family signal).

  if (compute) {
    cat("Estimating null distribution...\n")
    null_draws <- run_sim_cell(n_families,
                                delta_within = 0,
                                n_sim = n_null, mean_sib = mean_sib,
                                icc_obs = icc_obs, n_pgis = n_pgis,
                                concentrated = concentrated)
    # 95th percentile of null estimates: values this large occur by chance 5% of the time.
    # An observed estimate above this threshold triggers rejection of the null (one-sided alpha = 0.05).
    crit <- quantile(null_draws$delta_within, 0.95)
    saveRDS(crit, glue("PowerAnalysis/results/nulls/power_d_{null_tag}.rds"))


    ######################################################
    ##########  STEP 5: POWER CURVE  ####################
    ######################################################

    cat("Running power curve...\n")

    curve_results <- mclapply(delta_grid, function(dv) {
      draws <- run_sim_cell(n_families,
                             delta_within = dv,
                             n_sim = n_sim, mean_sib = mean_sib,
                             icc_obs = icc_obs, n_pgis = n_pgis,
                             concentrated = concentrated)
      # Proportion of replications where the estimated delta_within exceeded the null critical value
      data.frame(delta = dv,
                 power = mean(draws$delta_within > crit))
    }, mc.cores = detectCores() - 1)

    power_curve <- do.call(rbind, curve_results)
    saveRDS(power_curve, glue("PowerAnalysis/results/power_d_{tag}.rds"))
    cat("Power curve saved.\n")
  }


  ######################################################
  ##########  STEP 6: PLOT  ###########################
  ######################################################

  power_curve <- readRDS(glue("PowerAnalysis/results/power_d_{tag}.rds"))
  power_curve <- power_curve[order(power_curve$delta), ]

  # Linear interpolation to find smallest delta_within where power crosses 80%
  idx_above <- which(!is.na(power_curve$power) & power_curve$power >= 0.80)
  if (length(idx_above) == 0) {
    det_delta <- NA_real_
  } else {
    fa        <- min(idx_above)
    det_delta <- if (fa == 1) power_curve$delta[fa] else {
      d_lo <- power_curve$delta[fa - 1]; d_hi <- power_curve$delta[fa]
      p_lo <- power_curve$power[fa - 1]; p_hi <- power_curve$power[fa]
      d_lo + (0.80 - p_lo) / (p_hi - p_lo) * (d_hi - d_lo)
    }
  }

  p <- ggplot(power_curve, aes(x = delta, y = power)) +
    geom_line(linewidth = 1.2, color = "steelblue") +
    geom_point(size = 2.5, color = "steelblue") +
    geom_hline(yintercept = 0.80, linetype = "dashed", color = "grey40") +
    annotate("text", x = max(delta_grid) * 0.98, y = 0.82, label = "80% power",
             hjust = 1, size = 3, color = "grey40") +
    geom_vline(xintercept = delta_observed, linetype = "dotted", color = "red") +
    annotate("text", x = delta_observed + 0.001, y = 0.05,
             label = glue("observed = {round(delta_observed, 3)}"), hjust = 0, size = 3, color = "red") +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    labs(
      title = glue("Power to detect within-family PGI variance reduction - {sample_tag} (N families={n_families})"),
      x     = "True delta_within = (emptyind - condind) / totalvar",
      y     = "Power (one-sided, alpha = 0.05)"
    ) +
    theme_bw(base_size = 11)

  if (!is.na(det_delta))
    p <- p +
      geom_vline(xintercept = det_delta, linetype = "dotted", color = "steelblue") +
      annotate("text", x = det_delta + 0.001, y = 0.15,
               label = glue("detectable = {round(det_delta, 3)}"), hjust = 0, size = 3, color = "steelblue")

  print(p)
  ggsave(glue("PowerAnalysis/plots/power_d_{tag}.png"), p, width = 8, height = 5)
  cat(glue("Plot saved: PowerAnalysis/plots/power_d_{tag}.png\n"))
}
