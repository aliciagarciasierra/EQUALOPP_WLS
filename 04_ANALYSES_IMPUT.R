#####################################################################
###################### IMPUTED DATA ANALYSES ########################
#####################################################################


rm(list=ls()) 
set.seed(123)
source("00_MASTER.R")



########################## SETUP ####################################

# Compute for desired outcomes, even only one
outcomes  <- "education"   # OUTCOMES is defined in 00_MASTER.R
  
# Bootstrapping:
n_boot <- 100

# Number of imputed datasets:

m <- 10



NT <- "PGI"

########################## RUN ####################################

for (outcome in outcomes) {
    
  # Run for both PGIs and observed abilities:
  lapply(NT, function(natural_talents) {
    
    # ------- check
      print(paste0("which talent: ",natural_talents))
      print(paste0("n bootstraps: ",n_boot))
      print(paste0("m: ",m))
      
      
      # ------- read data
      data_list <-readRDS(paste0("data/siblings_",outcome,"_MI.rds"))
      
      # ------- scale numeric variables
      data_list <- lapply(data_list, function(df) {
        df %>% mutate_if(is.numeric, scale)
      })
      
      # ------- subset all data for testing (change m for less imputed datasets)
      data_list <- data_list[1:m]
      
      
      
      ########################## MODELS ESTIMATION ####################################
      
      
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
        # Assign names
        colnames(point_estimates) <- INDICES
        # Add column with difference
        point_estimates$diff <- point_estimates$IORAD-point_estimates$IOLIB
        # Write to file
        saveRDS(point_estimates, paste0("results/all_runs/","Complete_",natural_talents,"_MI.rds"))
        
        
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
          
          # Compute p-value (one-sided: H0: value <= 0, H1: value > 0)
          t_stat <- theta_MI / sqrt(T_var)
          p_value <- 1 - pt(t_stat, df)

          return(data.frame(
            Index    = coef_names[j],
            Outcome  = outcome,
            Estimate = theta_MI,
            SE       = sqrt(T_var),
            Lower    = ci_lower,
            Upper    = ci_upper,
            pval     = p_value,
            N        = nrow(data_list[[1]])
          ))
        })
        
        return(do.call(rbind, mi_boot_results))
      }
      
      
      # ------- Run bootstrapping
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
      
      
  
  
  }
  #, mc.cores=3
  ) # end of natural_talents loop



}






