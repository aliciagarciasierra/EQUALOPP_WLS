

###################################################################################
###################### ANALYSES WLS   ###########################################
###################################################################################

source("00_MASTER_WLS.R")

siblings <- readRDS("data/siblings.rds")

# apply transformations

#siblings <- siblings %>% 
#  mutate(wealth = ifelse(wealth == 0, 0.000001, wealth)) %>%
#  mutate(wealth = log10(wealth))

siblings <- siblings %>%
  mutate_if(is.numeric, scale)


                          
#-------------- Function to compute the main indexes 

compute_indexes <- function(outcome_var, siblings) {
  
  # 1) NULL MODEL
  m0 <- lmer(as.formula(paste(outcome_var, "~ 1 + (1 | familyID)")), data = siblings)
  
  vcov_m0 <- as.data.frame(VarCorr(m0))
  emptyind <- vcov_m0[2, 4]
  emptyfam <- vcov_m0[1, 4]
  totalvar <- (vcov_m0[1, 4] + vcov_m0[2, 4])
  
  # 2) CONDITIONAL MODEL
  m1 <- lmer(as.formula(paste(outcome_var, "~ (",  
        paste(OBSERVED_COG, collapse=" + ")," + ", paste(OBSERVED_NON_COG, collapse=" + ") ,")^2 + (1 | familyID)")), data = siblings)
  
  vcov_m1 <- as.data.frame(VarCorr(m1))
  condind <- vcov_m1[2, 4]
  condfam <- vcov_m1[1, 4]
  
  # 3) COMPLETE MODEL
  m2 <- lmer(as.formula(paste(outcome_var,  " ~ (", paste(ASCRIBED, collapse=" + ")," + ",  
             paste(OBSERVED_COG, collapse = " + "), " + ", paste(OBSERVED_NON_COG, collapse = " + "), 
     ")^2 + (1 |familyID)")), data = siblings)
  
  vcov_m2 <- as.data.frame(VarCorr(m2))
  completeind <- vcov_m2[2, 4]
  completefam <- vcov_m2[1, 4]
  
  # Index computations
  sibcorr <- emptyfam / totalvar
  condcorr <- condfam / totalvar
  w <- (condind - completeind) / totalvar
  v <- (emptyind - completeind) / totalvar
  
  IOLIB <- w + condcorr
  IORAD <- v + sibcorr
  
  # Create results data frame
  result_df <- data.frame(
    Outcome = outcome_var,
    Model = c("NULL MODEL", "CONDITIONAL MODEL", "COMPLETE MODEL"),
    emptyind = c(emptyind, NA, NA),
    emptyfam = c(emptyfam, NA, NA),
    totalvar = c(totalvar, NA, NA),
    condind = c(NA, condind, NA),
    condfam = c(NA, condfam, NA),
    completeind = c(NA, NA, completeind),
    completefam = c(NA, NA, completefam),
    sibcorr = c(sibcorr, NA, NA),
    condcorr = c(condcorr, NA, NA),
    w = c(w, NA, NA),
    v = c(v, NA, NA),
    IOLIB = c(IOLIB, NA, NA),
    IORAD = c(NA, NA, IORAD)
  )
  
  return(result_df)
}

#---------------- Store the results from the main analyses

print("compute main results")
all_results   <- mclapply(OUTCOMES, compute_indexes, siblings=siblings, mc.cores=4)
final_results <- do.call(rbind.data.frame, all_results)




#---------- Function to compute confidence intervals through bootsrapping

compute_indexes_bootstrap <- function(siblings, num_bootstrap_samples, outcome_var, results) {
  fit_model_and_compute_indexes <- function(bootstrap_data, indices) {
    # Subset the data for this bootstrap sample
    data_sample <- bootstrap_data[indices, ]
    
    # Compute the models and the statistics (similar to compute_indexes function)
    m0 <- lmer(as.formula(paste(outcome_var, "~ 1 + (1 | familyID)")), data = data_sample)
    m1 <- lmer(as.formula(paste(outcome_var, "~ (",  
               paste(OBSERVED_COG, collapse=" + ")," + ", paste(OBSERVED_NON_COG, collapse=" + ") ,")^2 + (1 | familyID)")), data = siblings)
    
    m2 <- lmer(as.formula(paste(outcome_var, "~ (", paste(ASCRIBED, collapse=" + ")," + ",  
     paste(OBSERVED_COG, collapse=" + ")," + ", paste(OBSERVED_NON_COG, collapse=" + ") ,")^2 + (1 |familyID)")), data = siblings)
    
    # Extract variance components
    vcov_m0 <- as.data.frame(VarCorr(m0))
    vcov_m1 <- as.data.frame(VarCorr(m1))
    vcov_m2 <- as.data.frame(VarCorr(m2))
    
    # Perform your index computations (same as in compute_indexes)
    emptyind <- vcov_m0[2, 4]
    emptyfam <- vcov_m0[1, 4]
    totalvar <- emptyfam + emptyind
    
    condind <- vcov_m1[2, 4]
    condfam <- vcov_m1[1, 4]
    
    completeind <- vcov_m2[2, 4]
    completefam <- vcov_m2[1, 4]
    
    sibcorr <- emptyfam / totalvar
    condcorr <- condfam / totalvar
    w <- (condind - completeind) / totalvar
    v <- (emptyind - completeind) / totalvar
    
    IOLIB <- w + condcorr
    IORAD <- v + sibcorr
    
    # Return a numeric vector with the key statistics
    return(c(IOLIB, IORAD, sibcorr))
  }
  
  # Run the bootstrapping
  bootstrap_results <- boot(siblings, fit_model_and_compute_indexes, R = num_bootstrap_samples)
  
  # Calculate standard errors for the bootstrapped estimates
  se_iolib <- sd(bootstrap_results$t[, 1])  # Index 1 for IOLIB
  se_iorad <- sd(bootstrap_results$t[, 2])  # Index 2 for IORAD
  se_sibcorr <- sd(bootstrap_results$t[, 3])  # Index 3 for sibcorr
  
  # Calculate confidence intervals
  upcilib <- as.numeric(results$IOLIB[1]) + 1.96 * se_iolib
  bottomcilib <- as.numeric(results$IOLIB[1]) - 1.96 * se_iolib
  upcirad <- as.numeric(results$IORAD[3]) + 1.96 * se_iorad
  bottomcirad <- as.numeric(results$IORAD[3]) - 1.96 * se_iorad
  upcisib <- as.numeric(results$sibcorr[1]) + 1.96 * se_sibcorr
  bottomcisib <- as.numeric(results$sibcorr[1]) - 1.96 * se_sibcorr
  
  # Return confidence intervals
  cis <- matrix(c(upcilib, upcirad, upcisib, bottomcilib, bottomcirad, bottomcisib), nrow = 3, ncol = 2)
  rownames(cis) <- c("Liberal", "Radical", "Sibling correlation")
  colnames(cis) <- c("Upper", "Lower")
  
  return(cis)
}



# --------- Compute bootstrapping

print("compute bootstrapping")

start = Sys.time()
ci_list <- mclapply(OUTCOMES, function(outcome) {
  outcome_results   <- filter(final_results, Outcome==outcome)
  bootstrap_results <- compute_indexes_bootstrap(siblings, num_bootstrap_samples = n_boot, outcome_var = outcome, results = outcome_results)
  
  # Get coefficients from the results
  iolib   <- (outcome_results$IOLIB[1])  # Assuming IOLIB is in the first row for the outcome
  iorad   <- (outcome_results$IORAD[3])   # Assuming IORAD is in the third row
  sibcorr <- (outcome_results$sibcorr[1])  # Assuming sibcorr is in the first row
  
  # Populate the ci_summary data frame
  data.frame("Index"    = c("IOLIB", "IORAD", "Sibcorr"),
             "Lower"    = c(bootstrap_results["Liberal", "Lower"], bootstrap_results["Radical", "Lower"], bootstrap_results["Sibling correlation", "Lower"]),
             "Upper"    = c(bootstrap_results["Liberal", "Upper"], bootstrap_results["Radical", "Upper"], bootstrap_results["Sibling correlation", "Upper"]),
             "Estimate" = c(iolib, iorad, sibcorr), 
             "Outcome"  = outcome)
}, mc.cores = 4)

stop = Sys.time()
print(paste0(round(as.numeric(difftime(as.POSIXct(stop), as.POSIXct(start), units = "secs")), 3), " Seconds"))

ci_summary <- do.call(rbind.data.frame, ci_list)


# Remove confidence intervals from the final_results
final_results <- final_results[ , !names(final_results) %in% c("CI_Upper_IOLIB", "CI_Lower_IOLIB", 
                                                               "CI_Upper_IORAD", "CI_Lower_IORAD",
                                                               "CI_Upper_Sibcorr", "CI_Lower_Sibcorr")]

plot(density(siblings$wealth))


#----------------Store bootstrapping results

# Create a new Excel workbook
wb <- createWorkbook()

# Add results sheet
addWorksheet(wb, "Full results")
writeData(wb, "Full results", final_results)

# Add confidence intervals sheet
addWorksheet(wb, "For plotting")
writeData(wb, "For plotting", ci_summary)

# Save the workbook to an Excel file
saveWorkbook(wb, paste0("results/full_results_",OBSERVED_COG,".xlsx"), overwrite = TRUE)






#---------------- Plot the graph

# Read the data 
data_graph <- read_excel(paste0("results/full_results_",OBSERVED_COG,".xlsx"), sheet = "For plotting")

# Set theme 
set_pilot_family("Avenir Next Medium", title_family = "Avenir Next Demi Bold")

# Custom order 
data_graph$Index   <- factor(data_graph$Index, levels = INDICES)
data_graph$Outcome <- factor(data_graph$Outcome, levels = OUTCOMES)

# Create the bar graph
  # Main elements
ggplot(data_graph, aes(x = Outcome, y = Estimate, fill = Index)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                position = position_dodge(0.9), width = 0.25, alpha = 0.4) +
  labs(title = " ", x = " ", y = " ") +
  geom_text(aes(label = round(Estimate, 2)), position = position_dodge(width = 1), vjust =-1.5 ,hjust=-0.1) + 
  
  # Add labels
  scale_x_discrete(labels = OUTCOMES.labs) +
  scale_fill_discrete(labels = INDICES.labs) +
  # Theme
  guides(fill = guide_legend(title = NULL)) +
  theme_bw()
  #theme_pilot(axis_title_size = 14,
  #            axis_text_size = 13,
  #            legend_text_size = 13,
  #            legend_title_size = 13,
  #            legend_position = "right")
#
# Save the plot
ggsave(paste0("plots/results_plot_",OBSERVED_COG,".pdf"), width = 13, height = 6, dpi = 300)
pdf(NULL)


