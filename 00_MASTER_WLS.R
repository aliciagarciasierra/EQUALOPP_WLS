

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

# SET WD 

setwd("/Users/agarcias/Library/CloudStorage/OneDrive-UniversitédeLausanne/EQUALOPP/PROJECT/WLS/data")

# OPEN RAW DATA

data <- read_dta("wls_bl_14_03.dta") # main dataset
pgi_cog<- read_dta("Lee_idpub_shuffled.dta") # PGIs cognitive
pgi_noncog <- read_dta("Turley_idpub_shuffled.dta") # PGIs non-cognitive

