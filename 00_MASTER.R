

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

rm(list=ls()) 

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
  library(pilot)
  library(ggpubr)
  library(missMDA)
  library(parallel)
  library(car)
  library(mice)
  library(parallel)
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

INDICES <- c("Sibcorr","IOLIB", "IORAD")
INDICES.labs <- c("Sibcorr" = "Sibling correlation", "IOLIB" = "Liberal IOP", "IORAD" = "Radical IOP")

# outcomes
OUTCOMES <- c("education","income", "wealth", "health_pc")
OUTCOMES_full <- c("education", "income", "wealth","health_self", "health_illness", "health_hospital", "health_pc")
OUTCOMES.labs <- c("education" = "Education", "occupation" = "Occupation", 
  "income_ind" = "Income Ind", "income" = "Income", 
  "wealth" = "Wealth", "health_self" = "Health Self-Rep", 
  "health_illness" = "Health N Illnesses", "health_hospital" ="Health Hospitalizations", "health_pc" = "Health")

# ascribed
ASCRIBED <- c("sex", "birth_year", "mother_age_birth", "father_age_birth", "birth_order")

# ability
ABILITY_DEFS <- c("polygenic indices", "observed ability")

# - observed
OBSERVED_NON_COG <- c("extraversion", "openness", "neuroticism", "conscientiousness", "agreeableness")
OBSERVED_COG     <- "centile_rank_IQ"  # or "IQ"

# - pgi
PGI_COG     <- c("pgi_education", "pgi_cognitive", "pgi_math_ability")
PGI_NON_COG <- c("pgi_depression", "pgi_well_being", "pgi_neuroticism")
PGIs        <- c(PGI_COG, PGI_NON_COG)
PC_COG      <- paste0("pc", 1:10, "cog")


# multiple imputation
m      <- 25

# for formulas
ascr_vars    <- paste(ASCRIBED,         collapse=" + ")
pgi_vars     <- paste(PGIs,             collapse=" + ")
cog_vars     <- paste(OBSERVED_COG,     collapse=" + ")
noncog_vars  <- paste(OBSERVED_NON_COG, collapse=" + ")






########################## FUNCTIONS ####################################

#-------------- Function to compute the main indexes 
compute_indexes <- function(outcome, data) {
  
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


#-------------- Function to be bootstrapped
est_fun <- function(data, indices, outcome) {
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



