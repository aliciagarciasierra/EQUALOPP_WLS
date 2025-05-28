#####################################################################
###################### IMPUTED DATA ANALYSES ########################
#####################################################################


rm(list=ls()) 
set.seed(123)
source("00_MASTER.R")


########################## SETUP ####################################

# Compute for desired outcomes, even only one
outcomes <- OUTCOMES   # OUTCOMES is defined in 00_MASTER.R

# Bootstrapping:
n_boot <- 100



########################## MODELS ESTIMATION ####################################



# Check number of observations by gender:
sapply(outcomes, function(outcome) {
  
  paste(outcome)
  
  # Read all
  df = readRDS(paste0("data/siblings_",outcome,".rds"))
  
  lapply(c(0,1), function(which_sex) {
    # Set label
    sex_lab <- ifelse(which_sex==0,"sons","daughters")
    # Filter by gender
    df <- df %>%
      group_by(familyID) %>%
      filter(all(sex == which_sex)) %>%
      ungroup() %>% 
      select(-sex) 
    # Print sample size
    paste0(sex_lab, " N:", nrow(df))
    
  })
  
})



######  Run for both PGIs and observed abilities:
for (natural_talents in NT) {
  
  ######  Run for each outcome:
  for (outcome in outcomes) {
    
    ###### Run for each gender:
    for (which_sex in c(0,1)) {
      
      # ------- set label
      sex_lab <- ifelse(which_sex==0,"sons","daughters")
      
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
      
      
      
      ######################## POINT VALUES ##############################
      
      # ------- Compute indices for all outcomes
      print("compute main results")
      final_results = compute_indexes(outcome, siblings, natural_talents)
      
      
      
      ##################### CONFIDENCE INTERVALS ############################
      
      # ------- Function to compute Cluster Bootstrapping
      compute_indexes_bootstrap <- function(dataset, n_boot, outcome, final_results) {
        
        # Run bootstrapping
        bootstrap_results <- boot(data            = dataset, 
                                  statistic       = est_fun, 
                                  outcome         = outcome,
                                  R               = n_boot,
                                  natural_talents = natural_talents
        )
        
        # Calculate SE and CI for each Index
        rows <- lapply(1:length(INDICES), function(i) {
          index <- INDICES[i]
          boots <- bootstrap_results$t[, i]
          
          # confidence intervals
          se    = sd(boots)
          value = final_results[index][!is.na(final_results[index])]  # only non Na value in column
          up    = value + 1.96 * se
          low   = value - 1.96 * se
          
          # results
          data.frame("Index"=index, "Outcome"=outcome, "Estimate"=value, "Lower"=low, "Upper"=up)
        })
        results <- bind_rows(rows) %>% mutate("N"=nrow(siblings))
        
        return(results)
        
      }
      
      
      # ------- Run bootstrapping
      print("compute bootstrapping")
      boot_results = compute_indexes_bootstrap(siblings, n_boot, outcome, final_results)
      
      
      
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
      
      
      
    } # end of sex loop
    
    
  } # end of outcomes loop
  
  
} # end of natural_talents loop








######################### GRAPH BY ABILITY AND SEX ###############################

# Read data
all_ci_summary <- lapply(c(0,1), function(which_sex) {
  # Save sex label
  sex_lab <- ifelse(which_sex==0,"sons","daughters")
  
  # Read by both ability definitions
  all_ci_summary <- lapply(NT, function(natural_talents) {
    data <- readWorkbook(paste0("results/by_outcome/full_results_",outcome,"_",natural_talents,"_",sex_lab,".xlsx"), sheet = "For plotting") 
    data %>% mutate(ability = natural_talents)
  })
  
  # Combine them
  bind_rows(all_ci_summary) %>% mutate(sex = sex_lab)
  
})
# Combine all data by sex and ability 
ci_summary <- bind_rows(all_ci_summary)


# Custom order 
ci_summary$Index   <- factor(ci_summary$Index,   levels = INDICES)
ci_summary$Outcome <- factor(ci_summary$Outcome, levels = outcomes)


# Create the bar graph
# Main elements
ggplot(ci_summary, aes(x = Outcome, y = Estimate, fill = Index)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                position = position_dodge(0.9), width = 0.25, alpha = 0.4) +
  facet_grid(ability~sex) +
  labs(title = " ", x = " ", y = " ") +
  geom_text(aes(label = round(Estimate, 2)), position = position_dodge(width = 1), vjust =-1.5 ,hjust=-0.1) + 
  
  # Add labels
  scale_fill_discrete(labels = c("Sibcorr" = "Sibling correlation", 
                                 "IOLIB" = "Liberal IOP", 
                                 "IORAD" = "Radical IOP")) +
  # Theme
  guides(fill = guide_legend(title = NULL)) +
  # Theme adjustments
  theme_bw(base_size = 15) +  # Set base font size
  theme(
    axis.title.x = element_text(size = 16, face = "bold"),  # Increase x-axis title size
    axis.title.y = element_text(size = 16, face = "bold"),  # Increase y-axis title size
    axis.text.x = element_text(size = 14),  # Increase x-axis text size
    axis.text.y = element_text(size = 14),  # Increase y-axis text size
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    plot.margin = margin(10, 10, 10, 10)  # Add space around the plot
  ) +
  ylim(c(-0.05,0.7)) +
  ggtitle(OUTCOMES.labs[outcome])


# Save the plot
ggsave(paste0("plots/by_sex_and_ability.png"), width = 10, height = 8, dpi = 300)



