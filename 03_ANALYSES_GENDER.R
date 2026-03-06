
#####################################################################
###################### IMPUTED DATA ANALYSES ########################
#####################################################################


rm(list=ls()) 
set.seed(123)
source("00_MASTER.R")


########################## SETUP ####################################

# Compute for desired outcome and natural talents
outcome         <- "education"
natural_talents <- "PGI"
moba_sample     <- "parents"   # "parents" or "children"

# Bootstrapping:
n_boot <- 1000



########################## MODELS ESTIMATION ####################################



# Check number of observations by gender:

siblings = readRDS(paste0("data/siblings_",outcome,".rds"))

lapply(c(0,1), function(which_sex) {
  # Set label
  sex_lab <- ifelse(which_sex==0,"Brothers","Sisters")
  # Filter by gender
  siblings <- siblings %>%
    group_by(familyID) %>%
    filter(all(sex == which_sex)) %>%
    ungroup() %>% 
    select(-sex) 
  # Print sample size
  paste0(sex_lab, " N:", nrow(siblings))
  
})




######  Run for PGIs


###### Run for each gender:
mclapply(c(0,1), function(which_sex) {
  
  # ------- set label
  sex_lab <- ifelse(which_sex==0,"Brothers","Sisters")
  
  # ------- check
  print(paste0("which group: ", sex_lab))
  print(paste0("which talent: ",natural_talents))
  print(paste0("n bootstraps: ",n_boot))
  
  
  ########################## MODELS ESTIMATION ####################################
  
  # ------- read data
  siblings <- readRDS(paste0("data/siblings_",outcome,".rds"))
  
  # ------- only keep one sex
  siblings <- siblings %>%
      group_by(familyID) %>%
      filter(all(sex == which_sex)) %>%
      ungroup() %>% 
      select(-sex)
  
  
  # ------- rescale
  siblings <- siblings %>%
    mutate_if(is.numeric, scale)
  
  
  
  ######################## FULL RESULTS ##############################
  
  # ------- Compute indices for all outcomes
  print("compute main results")
  final_results = compute_indexes(outcome, siblings, natural_talents)
  
  
  
  ##################### CONFIDENCE INTERVALS ############################

  # ------- Run bootstrapping
  print("compute bootstrapping")
  boot_results = compute_indexes_bootstrap(siblings, n_boot, outcome)
  
  
  
  ##################### SAVE RESULTS ############################
  
  # Create a new Excel workbook
  wb <- createWorkbook()
  
  # Add results sheet
  addWorksheet(wb, "Full results")
  writeData(wb, "Full results", final_results)
  
  # Add confidence intervals sheet
  addWorksheet(wb, "For plotting")
  writeData(wb, "For plotting", boot_results)
  
  # Save the workbook to an Excel file
  saveWorkbook(wb, paste0("results/by_outcome/full_results_",outcome,"_",natural_talents,"_",sex_lab,".xlsx"), overwrite = TRUE)
  
  
  
}, mc.cores=4) # end of sex loop

cat("Finished computation")









