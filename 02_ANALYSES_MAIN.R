#####################################################################
###################### MAIN ANALYSES ################################
#####################################################################


rm(list=ls()) 

set.seed(123)
source("00_MASTER.R")


########################## SETUP ####################################

# Compute for desired outcomes and natural talents, even only one
outcome         <- "education"
natural_talents <- "PGI"
  
# Bootstrapping iterations:
n_boot  <- 1000




########################## MODELS ESTIMATION ####################################

# ------- log
print(outcome)
print(natural_talents)

# ------- read data
siblings <- readRDS(paste0("data/siblings_",outcome,".rds"))

# ------- re-scale
siblings <- siblings %>%
  mutate_if(is.numeric, scale)



######################## FULL CALCULATIONS ##############################
  
# ------- Compute indices for all outcomes
print("compute main results")
final_results = compute_indexes(outcome, siblings, natural_talents)



##################### CONFIDENCE INTERVALS ############################


# ------- Run bootstrapping
print("compute bootstrapping")
ci_summary = compute_indexes_bootstrap(siblings, n_boot, outcome)


##################### SAVE RESULTS ############################

# Create a new Excel workbook
wb <- createWorkbook()

# Add results sheet
addWorksheet(wb, "Full results")
writeData(wb, "Full results", final_results)

# Add confidence intervals sheet
addWorksheet(wb, "For plotting")
writeData(wb, "For plotting", ci_summary)

# Save the workbook to an Excel file
saveWorkbook(wb, paste0("results/by_outcome/full_results_",outcome,"_",natural_talents,".xlsx"), overwrite = TRUE)


 






