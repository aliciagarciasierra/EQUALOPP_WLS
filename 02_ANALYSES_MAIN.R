#####################################################################
###################### MAIN ANALYSES ################################
#####################################################################


rm(list=ls()) 

set.seed(123)
source("00_MASTER.R")


########################## SETUP ####################################

# Compute for desired outcomes, even only one
outcomes <- OUTCOMES   # OUTCOMES is defined in 00_MASTER.R

# Bootstrapping:
n_boot  <- 10




########################## MODELS ESTIMATION ####################################


# Check number of observations:
sapply(outcomes, function(outcome) {
  N = readRDS(paste0("data/siblings_",outcome,".rds")) %>% nrow() 
  paste(outcome, " N:", N)
})



# Run for both PGIs and observed abilities:
for (natural_talents in NT) {

  # ------- print which definition
  print(natural_talents)
  
  # Run for each outcome:
  for (outcome in outcomes) {
      
    # ------- print which outcome
    print(outcome)
    
    # ------- read data
    siblings <- readRDS(paste0("data/siblings_",outcome,".rds"))
  
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
    ci_summary = compute_indexes_bootstrap(siblings, n_boot, outcome, final_results)
    
    
    
    
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
    
    
    
    
    ################### GRAPHS SEPARATED BY OUTCOME #########################
  
    # Create plot title
    title = paste0(OUTCOMES.labs[outcome], " with ", nt.labs[natural_talents])
    
    # Filter data for the current outcome
    data_subset <- readWorkbook(paste0("results/by_outcome/full_results_",outcome,"_",natural_talents,".xlsx"), sheet = "For plotting")
    
    # Sort Indices
    data_subset$Index <- factor(data_subset$Index, levels=INDICES)
    
    # Create the plot for the current outcome
    p <- ggplot(data_subset, aes(x = Outcome, y = Estimate, fill = Index)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                    position = position_dodge(0.9), width = 0.25, alpha = 0.4) +
      labs(title = title, x = " ", y = " ") +  # Set custom title
      geom_text(aes(label = round(Estimate, 2)), 
                position = position_dodge(width = 1), vjust = -1.5, hjust = -0.1, size = 4) + 
      
      # Add labels
      scale_x_discrete(labels = OUTCOMES.labs) +
      scale_fill_discrete(labels = INDICES.labs) +
      
      # Customize legend
      guides(fill = guide_legend(title = NULL)) +
      
      # Theme adjustments
      theme_bw(base_size = 15) +  # Set base font size
      theme(
        axis.title.x = element_blank(),  
        axis.text.x = element_blank(),  
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 16, face = "bold"),  # Increase y-axis title size
        axis.text.y = element_text(size = 14),  # Increase y-axis text size
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        plot.margin = margin(10, 10, 10, 10),  # Add space around the plot
        legend.position = "none",  # Remove the legend
        plot.title = element_text(hjust = 0.5)) +
      scale_y_continuous(limits = c(0, 0.5)) 
    
    
    
    # To view the plot
    p
    
    # To save it
    saveRDS(p, paste0("plots/by_outcome/",outcome,"_",natural_talents,".rds"))

  
  } # end of outcomes loop
  
  
    
  
  
  
  ######################### GRAPHS ALL OUTCOMES ###############################
  
  all_ci_summary <- lapply(outcomes, function(outcome) {
    readWorkbook(paste0("results/by_outcome/full_results_",outcome,"_",natural_talents,".xlsx"), sheet = "For plotting")
  })
  ci_summary <- bind_rows(all_ci_summary)
  
  # Custom order 
  ci_summary$Index   <- factor(ci_summary$Index,   levels = INDICES)
  ci_summary$Outcome <- factor(ci_summary$Outcome, levels = OUTCOMES)
  
  # Create the bar graph
  # Main elements
  ggplot(ci_summary, aes(x = Outcome, y = Estimate, fill = Index)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                  position = position_dodge(0.9), width = 0.25, alpha = 0.4) +
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
    ylim(c(-0.05,0.52)) +
    ggtitle(paste0("",nt.labs[natural_talents]))
  
  
    # Save the plot
  ggsave(paste0("plots/",natural_talents,".png"), width = 13, height = 6, dpi = 300)
  

  
}  # end of natural_talents loop
 

