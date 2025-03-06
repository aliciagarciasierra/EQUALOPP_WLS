
setwd("P:/AddHealth/Contract/24112301-Gratz")

suppressPackageStartupMessages( {
  
  library(dplyr)
  library(haven)
  library(ggplot2)
  library(factoextra)
  library(lme4)
  library(MuMIn)
  library(caret)
  library(BMA)
  library(partR2)
  library(ISCO08ConveRsions)
  library(modi)
  library(rsq)
  library(sjmisc)
  library(fixest)
  library(ltm)
  
  # from other script, might be useful
  library(ggrepel)
  library(tidyr)
  library(parallel)
  library(boot)
  library(Hmisc)
  library(xtable)
  library(ggforce)
  library(CCMHr)
  library(ggnewscale)
  library(openxlsx)
  library(readxl)

})

select    <- dplyr::select
summarize <- dplyr::summarize
melt      <- reshape2::melt

'%!in%' <- function(x,y)!('%in%'(x,y))

dir <- "Work/GenesEquality/"






# GLOBAL VARIABLES
n_boot <- 1000
INDICES <- c("Sibcorr","IOLIB","IORAD")

OUTCOMES  <- c("education","income", "health")
ASCRIBED <- c("male","birth_year","parent_age_birth","birth_order")
PGIs <- c("PSEDU1","PSCOGF", "PSINTELL",
              "PSNEURO2","PSDEP2","PSLIFES2")
PCs      <- c(paste0(rep("PSPC",9),1:9), "PSPC10")
NON_COG_home <- c("neu1"="H1PF30",  "neu2"="H1PF32", "neu3"="H1PF33",           # neuroticism
                  "neu4"= "H1PF34", "neu5"="H1PF35", "neu6"="H1PF36",
                  "con1"="H1PF18","con2"="H1PF19", 
                  "con3"="H1PF20","con4"= "H1PF21"                              # conscientiousness
                  )
NON_COG_sch <- c("ext1"="S62B", "ext2"="S62E","ext3"="S62O")                    # extraversion
ABILITY <- c("cognitive","neuroticism","conscientiousness")

# outcomes
EDU_VARS  <- c("H4ED2","H5OD11")
OCCU_VARS <- c("H4LM8","H5LM12")
# income4 = H4EC2, income4bestguess = H4EC3
# health = H4GH1


# labels
OUTCOMES.labs <- c("education"="Education",
                   "income"="Income",
                   "health"="Health")
INDICES.labs <- c("Sibcorr" = "Sibling correlation", 
                  "IOLIB" = "Liberal IOP", 
                  "IORAD" = "Radical IOP")






# ---- CROSSWALKS ---- 

cross_edu_4    <- data.frame(
  "highest_edu" = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 96, 98),
  "years_edu"   = c(8,10,12,13,14,14, 16, 17, 18, 19, 20, 18, 20, NA, NA)
)

cross_edu_5    <- data.frame(
  "highest_edu" = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, NA),
  "years_edu"   = c(8,10,12,12,13,14,14,14,14,16,17,18,19,20,18,20, NA)
)

cross_region <- data.frame(
  "region"      = c(1, 2, 3, 4, 99),
  "region_name" = c("West","Midwest","Northeast","South","No location")
)

cross_income <- data.frame(
  "income"     = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 96, 97, 98),
  "income_mid" = c(2500, 7500, 12500, 17000, 22500, 27500, 35000, 45000, 62500, 87500, 125000, 175000, 250000, NA, NA, NA)
)

cross_p_edu  <- data.frame(
  "p_edu"     = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 96),
  "years_edu" = c(8,10,12,12,12,14,14,17,19, 0, NA)
)

cross_assets <- data.frame(
  "hh_assets"  = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 96, 98),
  "assets_mid" = c(2500, 7500, 17500, 37500, 75000, 175000, 375000, 750000, 1500000, NA, NA )
)

cross_debts <- data.frame(
  "hh_debts"  = c(1, 2, 3, 4, 5, 6, 7, 8, 98),
  "debts_mid" = c(500, 2500, 7500, 17500, 37500, 75000, 175000, 300000 , NA)
)

cross_CENS_SOC <- readxl::read_excel("Work/crosswalks/cross_CENS_SOC.xls", skip = 4)[,-c(1,2)]
colnames(cross_CENS_SOC) <- c("census", "soc")
cross_SOC_ISCO <- readxl::read_excel("Work/crosswalks/cross_SOC_ISCO.xls", skip = 5)[,c(1,4)]
colnames(cross_SOC_ISCO) <- c("isco", "soc")







# ----- DATA RECODING -----

noncog_recode_na <- function(noncog)    ifelse(noncog %in% c(6,8, 9, 96:99) | is.na(noncog), NA, noncog)

refused_to_na    <- function(variable)  ifelse(variable==96 | is.na(variable), NA, variable)

invert <- function(vector) {
  # invert scale
  max <- max(vector, na.rm=T)
  min <- min(vector, na.rm=T)
  max + min - vector
}




# ----- COMPUTING FUNCTIONS -----


# R2 functions
compute_R2 <- function(model, adj="plain") {
  if (adj=="adjusted") {
    summary(model)$adj.r.squared*100
  } else if (adj=="plain") {
    summary(model)$r.squared*100
  }
}

compute_part_R2 <- function(model, reduced_model, adj="plain") {
  adj_logical <- ifelse(adj=="plain", F, T)
  rsq.partial(model,reduced_model, adj_logical)$partial.rsq*100
}




load_data_y <- function(ability, outcome) {
  data <- readRDS("Work/data/data.rds")
  # choose ability
  remove_ability <- switch(ability, "genotype"=ABILITY_VARS, "phenotype"=PGI_VARS)
  data <- select(data, -any_of(remove_ability))
  # choose outcome
  data$y <- switch(outcome, 
                   "education"  = data$education, "income" = data$income, "GPA" = data$GPA)
  select(data, -AID, -any_of(OUTCOME_VARS))
}


# --- Bayesian Model Averaging ---
estimate_inequality <- function(data, ability, outcome) {
  
  # -- DATA --
  data <- load_data_y(ability, outcome)
  
  
  # -- MODELS --
  
  # (1) NULL MODEL
  m0 <- lmer(y ~ 1 + (1 | FAMID), data)
  
  
  
  
  
}





