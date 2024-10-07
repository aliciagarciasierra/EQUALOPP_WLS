

##############################################################
######## LIBERAL AND RADICAL EOP ##########################
######## USING WISCONSIN LONGITUDINAL STUDY #################
##############################################################

# Authors: Michael Grätz, Alicia García-Sierra & Sonia Petrini

# Data: WLS

# Date of preparation of this script: Sept 2025

#######################################################
#########   PREPARE THE ENVIRONMENT ################
######################################################

# CLEAN

rm(list=ls()) 

# LOAD PACKAGES

library(tidyverse)
library(haven)
library(stringr)
library(tidyr)
library(labelled)
library(readxl)
library(lme4)
library(fixest)
library(dplyr)
library(boot)
library(Matrix) # note that an updated version of R might be required for this package to work correctly in our analyses
library(openxlsx)
library(ggplot2)
library(pilot)
library(dplyr)
library(ggpubr)
library(missMDA)
library(parallel)

`%!in%` <- function(x, y) !(x %in% y)


# SET WD 

#setwd("/Users/spetrini/Library/Mobile Documents/com~apple~CloudDocs/University/UNIL/projects/Inequality-Across-Families-and-PGIs/WLS/EQUALOPP_WLS")


# OPEN RAW DATA (first time)

#data <- read_dta("data/wls_bl_14_03.dta") # main dataset
#pgi_cog <- read_dta("data/Lee_idpub_shuffled.dta") # PGIs cognitive
#pgi_noncog <- read_dta("data/Turley_idpub_shuffled.dta") # PGIs non-cognitive


# SAVE IN RDA (for faster read following times)

#saveRDS(data, file = "data/data.rds")
#saveRDS(pgi_cog, file = "data/pgi_cog.rds")
#saveRDS(pgi_noncog, file = "data/pgi_noncog.rds")




# GLOBALS 
INDICES <- c("Sibcorr","IOLIB", "IORAD")
INDICES.labs <- c("Sibcorr" = "Sibling correlation", "IOLIB" = "Liberal IOP", "IORAD" = "Radical IOP")
stopifnot(names(INDICES.labs) == INDICES)

OUTCOMES <- c("education", "occupation","income_ind", 
                  "income_hh", "wealth", "health_self", "health_illness", "health_pc")
OUTCOMES.labs <- c("education" = "Education", "occupation" = "Occupation", 
  "income_ind" = "Income Ind", "income_hh" = "Income HH", 
  "wealth" = "Wealth", "health_self" = "Health Self-Rep", 
  "health_illness" = "Health N Illnesses", "health_pc" = "Health PC")
stopifnot(names(OUTCOMES.labs) == OUTCOMES)

OBSERVED_NON_COG <- c("extraversion", "openness", "neuroticism", "conscientiousness", "agreeableness")
OBSERVED_COG     <- "centile_rank_IQ"  # or "IQ"

ABILITY_DEFS <- c("polygenic indices", "observed ability")
PC_COG     <- paste0("pc", 1:10, "cog")
PC_NON_COG <- paste0("pc", 1:10, "noncog")


# settings
n_boot <- 50





