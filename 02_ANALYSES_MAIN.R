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


 




########################## COMBINE WITH MOBA ####################################

# === MoBa

# Parents
ci_summaryMoBa <- read.delim(text="Index	Outcome	Estimate	Lower	Upper	pval	N
Sibcorr	education	0.37420625	0.36016743	0.3898473	0	19514
IOLIB	education	0.32063291	0.30463092	0.3349338	0	19514
IORAD	education	0.39904685	0.38562276	0.4130919	0	19514
diff	education	0.07841393	0.07238116	0.0847985	0	19514", sep = "\t")

# Children
#ci_summaryMoBa <- read.delim(text="Index	Outcome	Estimate	Lower	Upper	pval	N
#Sibcorr	education	0.39806344	0.37070421	0.41979626	0	6740
#IOLIB	education	0.34577913	0.32002460	0.36793881	0	6740
#IORAD	education	0.43095272	0.40635397	0.45467043	0	6740
#diff	education	0.08517359	0.07476835	0.09619981	0	6740", sep = "\t") 


# === WLS 
ci_summaryWLS <- readWorkbook(paste0("results/by_outcome/full_results_",outcome,"_",natural_talents,".xlsx"), 
                              sheet = "For plotting") 


# Combine 
data <- bind_rows(mutate(ci_summaryMoBa,Dataset="MoBa"), 
                  mutate(ci_summaryWLS, Dataset="WLS"))


# Save all
saveRDS(data, paste0("results/results_",outcome,"_",natural_talents,".rds"))




