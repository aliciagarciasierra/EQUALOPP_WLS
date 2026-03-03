

##############################################################
######## LIBERAL AND RADICAL EOP ##########################
######## USING WISCONSIN LONGITUDINAL STUDY #################
##############################################################

# Authors: Michael Grätz, Alicia García-Sierra & Sonia Petrini

# Data: WLS


#######################################################
#########   PREPARE THE ENVIRONMENT ################
######################################################

# CLEAN

#rm(list=ls()) 

# LOAD PACKAGES

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
  library(stringr)
  library(tidyr)
  library(labelled)
  library(readxl)
  library(lme4)
  library(fixest)
  library(writexl)
  library(dplyr)
  library(boot)
  library(Matrix) # note that an updated version of R might be required for this package to work correctly in our analyses
  library(openxlsx)
  library(ggplot2)
  #library(pilot)
  library(ggpubr)
  library(missMDA)
  library(parallel)
  library(car)
  library(mice)
  library(parallel)
  library(xtable)
  library(glue)
  library(patchwork)
  library(viridisLite)
})

`%!in%` <- function(x, y) !(x %in% y)

# SET WD 

#setwd("~/Library/CloudStorage/OneDrive-UniversitédeLausanne/EQUALOPP/PROJECT/WLS/EQUALOPP_WLS")

# OPEN RAW DATA (first time)

#data <- read_dta("data/wls_bl_14_03.dta") # main dataset
#pgi_cog<- read_dta("data/Lee_idpub_shuffled.dta") # PGIs cognitive
#pgi_noncog <- read_dta("data/Turley_idpub_shuffled.dta") # PGIs non-cognitive


# SAVE IN RDA (for faster read following times)

#saveRDS(data, file = "data/data.rds")
#saveRDS(pgi_cog, file = "data/pgi_cog.rds")
#saveRDS(pgi_noncog, file = "data/pgi_noncog.rds")



########################## GLOBALS ####################################

INDICES      <- c("Sibcorr", "IOLIB", "IORAD")
INDICES.labs <- c("Sibcorr" = "Sibling correlation", "IOLIB" = "Liberal IOP", "IORAD" = "Radical IOP")

# Outcomes
# Used for the analysis:
OUTCOMES      <- c("education")    # "income", "wealth", "health_pc"

# All outcomes:
OUTCOMES_full <- c("education", "income", "wealth", "health_self", "health_illness", "health_hospital", "health_pc")
OUTCOMES.labs <- c("education"       = "Education", 
                   "grades"          = "School grades",
                   "occupation"      = "Occupation", 
                   "income"          = "Income Ind.",
                   "wealth"          = "Wealth", 
                   "health_self"     = "Health Self-Rep", 
                   "health_illness"  = "Health N Illnesses", 
                   "health_hospital" = "Health Hospitalizations", 
                   "health_pc"       = "Health")


# Natural talents
NT      = c("PGI", "observed")
nt.labs = c("PGI"     = "PGIs", 
            "observed"= "Observed skills")

# - observed
OBSERVED_NON_COG <- c("extraversion", "openness", "neuroticism", "conscientiousness", "agreeableness")
OBSERVED_COG     <- "centile_rank_IQ"  # or "IQ"
OBSERVED <- c(OBSERVED_NON_COG, OBSERVED_COG)

# - pgis
PGIs <- c("pgi_education", "pgi_cognitive", 
          "pgi_depression", "pgi_extraversion", "pgi_neuroticism", "pgi_openness",
          "pgi_risk", "pgi_well_being", "pgi_adhd")


PC          <- paste0("pc", 1:10)

# Ascribed characteristics
ASCRIBED <- c("sex", "birth_year", "mother_age_birth", "birth_order", PC)










########################## FUNCTIONS ####################################

add_stars <- function(p_values) {
  # add significance stars
  ifelse(p_values < 0.001, "***",
         ifelse(p_values < 0.01, "**",
                ifelse(p_values < 0.05, "*", "")))
}


#-------------- Function to impute parent ages from siblings
choose_parent_year <- function(parent_by, other_by) {
  if (all(is.na(parent_by))) return(NA_real_)
  
  # parental info per row: does this row know this parent & the other one?
  info_count <- (!is.na(parent_by)) + (!is.na(other_by))
  
  uniq_vals <- unique(na.omit(parent_by))
  
  if (length(uniq_vals) == 1) {
    # no conflict
    uniq_vals
  } else {
    # conflict: pick value from row with most parental info
    best_idx <- which(
      !is.na(parent_by) &
        info_count == max(info_count[!is.na(parent_by)])
    )[1]
    parent_by[best_idx]
  }
}



#-------------- Function to compute the main indexes 
compute_indexes <- function(outcome, data, natural_talents) {
  
  # ------- models specifications
  m0_vars <- "1"
  famID   <- "+ (1 | familyID)"
  
  # Combine available variables with a + 
  ascr_vars    <- paste(ASCRIBED[ASCRIBED %in% names(data)], collapse=" + ")
  pgi_vars     <- paste(PGIs,     collapse=" + ")
  obs_vars     <- paste(OBSERVED, collapse=" + ")
  
  # Combine variable sets together
  if(natural_talents == "PGI") {
    m1_vars <- paste0("(", pgi_vars, ")^2")
    m2_vars <- paste0("(", pgi_vars, "+", ascr_vars,")^2")
    
  } else if(natural_talents == "observed") {
    m1_vars <- paste0("(", obs_vars, ")^2")
    m2_vars <- paste0("(", obs_vars, "+", ascr_vars,")^2")
  } else (print("select a valid definition of natural talents"))
  
  
  # 1) NULL MODEL
  m0 <- lmer(as.formula(paste(outcome, "~", m0_vars, famID)), data = data)
  
  vcov_m0 <- as.data.frame(VarCorr(m0))
  emptyind <- vcov_m0[vcov_m0$grp == "Residual", "vcov"]
  emptyfam <- vcov_m0[vcov_m0$grp == "familyID", "vcov"]
  totalvar <- emptyfam + emptyind
  
  # 2) CONDITIONAL MODEL
  m1 <- lmer(as.formula(paste(outcome, "~", m1_vars, famID)), data = data)
  
  vcov_m1 <- as.data.frame(VarCorr(m1))
  condind <- vcov_m1[vcov_m1$grp == "Residual", "vcov"]
  condfam <- vcov_m1[vcov_m1$grp == "familyID", "vcov"]
  
  # 3) COMPLETE MODEL
  m2 <- lmer(as.formula(paste(outcome, "~", m2_vars, famID)), data = data)
  
  vcov_m2 <- as.data.frame(VarCorr(m2))
  completeind <- vcov_m2[vcov_m2$grp == "Residual", "vcov"]
  completefam <- vcov_m2[vcov_m2$grp == "familyID", "vcov"]
  
  # Index computations
  Sibcorr  <- emptyfam / totalvar
  condcorr <- condfam / totalvar
  w <- (condind - completeind) / totalvar
  v <- (emptyind - completeind) / totalvar
  
  IOLIB <- w + condcorr
  IORAD <- v + Sibcorr
  
  # Create results data frame
  result_df <- data.frame(
    Outcome = outcome,
    Model = c("NULL MODEL", "CONDITIONAL MODEL", "COMPLETE MODEL"),
    emptyind = c(emptyind, NA, NA),
    emptyfam = c(emptyfam, NA, NA),
    totalvar = c(totalvar, NA, NA),
    condind = c(NA, condind, NA),
    condfam = c(NA, condfam, NA),
    completeind = c(NA, NA, completeind),
    completefam = c(NA, NA, completefam),
    Sibcorr = c(Sibcorr, NA, NA),
    condcorr = c(condcorr, NA, NA),
    w = c(w, NA, NA),
    v = c(v, NA, NA),
    IOLIB = c(IOLIB, NA, NA),
    IORAD = c(NA, NA, IORAD)
  )
  
  return(result_df)
}


#-------------- Function to compute Clustered Bootstrapping
est_fun <- function(data, indices, outcome, natural_talents) {
  
  # Prepare variables
  famID        <- "+ (1 | familyID)"
  ascr_vars    <- paste(ASCRIBED[ASCRIBED %in% names(data)], collapse=" + ")
  pgi_vars     <- paste(PGIs,     collapse=" + ")
  
  # Prepare formulas
  m0_vars      <- "1"
  m1_vars <- paste0("(", pgi_vars, ")^2")
  m2_vars <- paste0("(", pgi_vars, "+", ascr_vars,")^2")
  
  # Subset the data for this bootstrap sample
  data_sample <- data[indices, ]
  
  # Cluster re-sampling
  # get family ID of sampled individuals
  sampled_families <- unique(data$familyID)[indices]
  
  # get the sibling of each sampled individual
  data_sample <- data[data$familyID %in% sampled_families, ]
  
  # Compute the models and the statistics (similar to compute_indexes function)
  m0 <- lmer(as.formula(paste(outcome, "~", m0_vars, famID)), data = data_sample)
  m1 <- lmer(as.formula(paste(outcome, "~", m1_vars, famID)), data = data_sample)
  m2 <- lmer(as.formula(paste(outcome, "~", m2_vars, famID)), data = data_sample)
  
  # Extract variance components
  vcov_m0 <- as.data.frame(VarCorr(m0))
  vcov_m1 <- as.data.frame(VarCorr(m1))
  vcov_m2 <- as.data.frame(VarCorr(m2))
  
  # Perform your index computations (same as in compute_indexes)
  emptyind    <- vcov_m0[vcov_m0$grp == "Residual", "vcov"]
  emptyfam    <- vcov_m0[vcov_m0$grp == "familyID", "vcov"]
  totalvar    <- emptyfam + emptyind
  condind     <- vcov_m1[vcov_m1$grp == "Residual", "vcov"]
  condfam     <- vcov_m1[vcov_m1$grp == "familyID", "vcov"]
  completeind <- vcov_m2[vcov_m2$grp == "Residual", "vcov"]
  completefam <- vcov_m2[vcov_m2$grp == "familyID", "vcov"]
  
  Sibcorr  <- emptyfam / totalvar
  condcorr <- condfam / totalvar
  w <- (condind - completeind) / totalvar
  v <- (emptyind - completeind) / totalvar
  
  IOLIB <- w + condcorr
  IORAD <- v + Sibcorr
  
  # Return a numeric vector with the key statistics
  return(c(Sibcorr, IOLIB, IORAD))
}




# ------- Function to compute estimates and CIs of indices
compute_indexes_bootstrap <- function(dataset, n_boot, outcome) {
  
  # Run bootstrapping
  bootstrap_results <- boot(data            = dataset, 
                            statistic       = est_fun, 
                            outcome         = outcome,
                            R               = n_boot,
                            natural_talents = natural_talents
  )
  
  # Boot runs: rename
  boot_estimates <- data.frame(bootstrap_results$t)
  names(boot_estimates) <- INDICES
  
  # Point estimates: rename
  point_estimates <- data.frame(t(bootstrap_results$t0))
  names(point_estimates) <- INDICES
  
  # Add IOP difference
  boot_estimates  <- boot_estimates %>% mutate(diff=IORAD-IOLIB)
  point_estimates <- point_estimates %>% mutate(diff=IORAD-IOLIB)
  
  # Calculate SE and CI for each Index
  results <- map_df(names(boot_estimates), function(index) {
    
    # Estimates
    boots = boot_estimates[[index]]
    value = point_estimates[[index]] 
    
    # SE and CI
    se_boot <- sd(boots)
    CI    = quantile(boots, c(.025, .975))
    
    # one-sided p-value for H0: value <= 0, H1: value > 0
    z     <- value / se_boot
    p_val <- 1 - pnorm(z)
    
    # results
    data.frame("Index"=index,        "Outcome"=outcome,     "Estimate"=value, 
               "Lower"=CI[["2.5%"]], "Upper"=CI[["97.5%"]], "pval"=p_val,
               "N"    = nrow(dataset))
  })
  
  return(results)
  
}


