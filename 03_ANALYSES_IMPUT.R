#####################################################################
###################### IMPUTED DATA ANALYSES ########################
#####################################################################


rm(list=ls()) 
set.seed(123)
source("00_MASTER.R")



########################## SETUP ####################################

# Compute for desired outcomes, even only one
outcomes <- "education"   # OUTCOMES is defined in 00_MASTER.R

# Bootstrapping:
n_boot <- 100

# Number of imputed datasets:

m <- 25

# Check number of siblings per family
data_list <-readRDS(paste0("data/final_datasets_",outcome,"_MI.rds"))
n_siblings_list <- lapply(data_list, function(dataset) {
  n_siblings <- dataset %>% 
    group_by(familyID) %>% 
    summarise(count = n_distinct(ID)) %>%
    ungroup()
  
  return(n_siblings)
})

# Check siblings
n_siblings_first_dataset <- n_siblings_list[[1]]
summary(n_siblings_first_dataset$count) # only 2


# Run for both PGIs and observed abilities:
for (natural_talents in NT) {
  
  # Run for each outcome:
  lapply(outcomes, function(outcome) {
      
      # ------- check
      print(paste0("which talent: ",natural_talents))
      print(paste0("n bootstraps: ",n_boot))
      print(paste0("m: ",m))
      
      
      
      ########################## MODELS ESTIMATION ####################################
      
      # ------- read data
      data_list <-readRDS(paste0("data/final_datasets_",outcome,"_MI.rds"))
      
      # check sample size and families
      data_example <- data_list[[1]]
      n_distinct(data_example$ID)
      n_distinct(data_example$familyID)
      
      # ------- scale numeric variables
      data_list <- lapply(data_list, function(df) {
        df %>% mutate_if(is.numeric, scale)
      })
      
      # ------- subset all data for testing (change m for less imputed datasets)
      data_list <- data_list[1:m]
      
      
      
      ######################## POINT VALUES ##############################
      
      
      print("Compute main results over imputed datasets")
      
      # ------- Compute indices for multiple datasets and outcomes
      all_results_list <- mclapply(data_list, 
                                   compute_indexes, 
                                   outcome = outcome,
                                   natural_talents = natural_talents,
                                   mc.cores = 4)
      
    
      # ------- Flatten the list of lists into a single data frame
      final_results <- bind_rows(all_results_list)
      
      
      # ------- Compute the mean across imputed datasets
      final_results <- final_results %>%
        group_by(Outcome, Model) %>%
        summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop")
      
      
      
      
      ##################### CONFIDENCE INTERVALS ############################
      
      
      print("compute bootstrapping over imputed datasets")
      
      # ------- Define function for Multiple Imputation + Cluster Bootstrapping
      mi_cluster_boot <- function(data_list, outcome, n_boot) {
        
        # Perform bootstrapping in parallel for imputed datasets
        boot_results <- mclapply(data_list, function(dataset) {
      
          boot(data            = dataset, 
               statistic       = est_fun, 
               outcome         = outcome,
               R               = n_boot,
               natural_talents = natural_talents)
        }
        , mc.cores = detectCores() - 1
        )
        
        # ---- Point estimates
        point_estimates <- lapply(boot_results, function(boot_obj) boot_obj$t0)
        # Convert 
        point_estimates <- do.call(rbind, point_estimates) %>% data.frame()
        # Add column with difference
        point_estimates$diff <- point_estimates$X3-point_estimates$X2
        
        
        # ---- Boot estimates
        boot_matrices <- lapply(boot_results, function(boot_obj) {
          # Assign names
          colnames(boot_obj$t) <- INDICES
          # Convert to df
          boot_df <- data.frame(boot_obj$t)
          # Add column with difference
          boot_df <- boot_df %>% mutate(diff = IORAD-IOLIB)
          # Return 
          boot_df
        })
        
        # Get column names 
        coef_names <- colnames(boot_matrices[[1]])
        
        # Combine results using Rubin's rules
        mi_boot_results <- lapply(seq_along(coef_names), function(j) {
          theta_i  <- point_estimates[, j]
          boot_sds <- sapply(boot_matrices, function(mat) sd(mat[, j]))
          
          theta_MI <- mean(theta_i)
          W <- mean(boot_sds^2)
          B <- var(theta_i)
          T_var <- W + (1 + 1/length(data_list)) * B
          df <- (length(data_list) - 1) * (1 + (W / ((1 + 1/length(data_list)) * B)))^2
          ci_lower <- theta_MI - qt(0.975, df) * sqrt(T_var)
          ci_upper <- theta_MI + qt(0.975, df) * sqrt(T_var)
          
          # Compute p-value
          t_stat <- theta_MI / sqrt(T_var)
          p_value <- 2 * pt(-abs(t_stat), df)
          
          return(data.frame(
            Index    = coef_names[j],
            Outcome  = outcome,
            Estimate = theta_MI,
            Lower    = ci_lower, 
            Upper    = ci_upper,
            pval     = p_value
          ))
        })
        
        return(do.call(rbind, mi_boot_results))
      }
      
      
      # ------- Run bootstrapping
      print("compute bootstrapping")
      boot_results <- mi_cluster_boot(data_list, outcome, n_boot)
      
      
      
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
      saveWorkbook(wb, paste0("results/by_outcome/full_results_",outcome,"_",natural_talents,"_MI.xlsx"), overwrite = TRUE)
      
      

  }) # end of outcomes loop
  
  
  
  
  
  
  
  
  
  
  
  ######################### GRAPH ALL OUTCOMES ###############################
  
  all_ci_summary <- lapply(outcomes, function(outcome) {
    readWorkbook(paste0("results/by_outcome/full_results_",outcome,"_",natural_talents,"_MI.xlsx"), sheet = "For plotting")
  })
  ci_summary <- bind_rows(all_ci_summary)
  
  # Take out difference
  ci_summary <- ci_summary %>% filter(Index %!in% c("diff"))
  
  # Custom order 
  ci_summary$Index   <- factor(ci_summary$Index,   levels = INDICES)
  ci_summary$Outcome <- factor(ci_summary$Outcome, levels = outcomes)
  
  
  # Create the bar graph
  # Main elements
  ggplot(ci_summary, aes(x = Outcome, y = Estimate, fill = Index)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                  position = position_dodge(0.9), width = 0.25, alpha = 0.4) +
    labs(title = " ", x = " ", y = " ") +
    geom_text(aes(label = round(Estimate, 2)), position = position_dodge(width = 1), vjust =-3.5 ,hjust=0) + 
    
    # Add labels
    scale_fill_discrete(labels = c("Sibcorr" = "Sibling correlation", 
                                   "IOLIB" = "Liberal IOP", 
                                   "IORAD" = "Radical IOP")) +
    # Theme
    guides(fill = guide_legend(title = NULL)) +
    
    # This is to make sure that the labels are on top and not overlapping the bars in the combined plot
    coord_cartesian(clip = "off") + 
    
    # Theme adjustments
    theme_bw(base_size = 15) +  # Set base font size
    theme(
      axis.title.x = element_text(size = 18, face = "bold"),  # Increase x-axis title size
      axis.title.y = element_text(size = 18, face = "bold"),  # Increase y-axis title size
      axis.text.x = element_text(size = 18),  # Increase x-axis text size
      axis.text.y = element_text(size = 16),  # Increase y-axis text size
      legend.text = element_text(size = 16),  # Legend text size
      panel.grid.major = element_blank(),  # Remove major grid lines
      panel.grid.minor = element_blank(),  # Remove minor grid lines
      plot.margin = margin(10, 10, 10, 10)  # Add space around the plot
    ) +
    ylim(c(-0.05,0.52)) +
    ggtitle(paste0("",nt.labs[natural_talents]))
  
  
  # Save the plot
  ggsave(paste0("plots/",natural_talents,"_MI.png"), width = 13, height = 6, dpi = 300)



  
} # end of natural_talents loop
