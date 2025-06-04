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
n_boot <- 1000

# Number of imputed datasets:
m <- 25



########################## MODELS ESTIMATION ####################################


# Check number of observations by gender:
sapply(outcomes, function(outcome) {
  
  paste(outcome)
  
  # read all
  data_list <-readRDS(paste0("data/final_datasets_",outcome,"_MI.rds"))
  
  lapply(c(0,1), function(which_sex) {
    # Set label
    sex_lab <- ifelse(which_sex==0,"Brothers","Sisters")
    # filter by gender
    df <- data_list[[1]] %>%
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
    
    ###### Run for each sex:
    for (which_sex in c(0,1)) {
      
      # ------- set label
      sex_lab <- ifelse(which_sex==0,"Brothers","Sisters")
      
      # ------- check
      print(paste0("which group: ",  sex_lab))
      print(paste0("which talent: ", natural_talents))
      print(paste0("n bootstraps: ", n_boot))
      print(paste0("m: ",m))
      
      
      
      ########################## MODELS ESTIMATION ####################################
      
      # ------- read data
      data_list <-readRDS(paste0("data/final_datasets_",outcome,"_MI.rds"))
      
      # ------- only keep one sex
      data_list <- lapply(data_list, function(df) {
         df %>%
          group_by(familyID) %>%
          filter(all(sex == which_sex)) %>%
          ungroup() %>% 
          select(-sex)
      })
      
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
      all_results_list <- lapply(data_list, 
                                   compute_indexes, 
                                   outcome = outcome,
                                   natural_talents = natural_talents#,
                                   #mc.cores = 4
                                 )
      
      
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
        
        # Point estimates
        coef_names      <- INDICES
        point_estimates <- do.call(rbind, lapply(boot_results, function(boot_obj) boot_obj$t0))
        
        # Boot estimates
        boot_matrices <- lapply(boot_results, function(boot_obj) {
          colnames(boot_obj$t) <- coef_names
          boot_obj$t
        })
        
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
          
          return(data.frame(
            Index    = coef_names[j],
            Outcome  = outcome,
            Estimate = theta_MI,
            Lower    = ci_lower, 
            Upper    = ci_upper
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
      saveWorkbook(wb, paste0("results/by_outcome/full_results_",outcome,"_",natural_talents,"_",sex_lab,"_MI.xlsx"), overwrite = TRUE)
      
    
      
    } # end of sex loop
    
    
  } # end of outcomes loop
  
  
} # end of natural_talents loop








######################### GRAPH BY ABILITY AND SEX ###############################

# Read data
all_ci_summary <- lapply(c(0,1), function(which_sex) {
  # Save sex label
  sex_lab <- ifelse(which_sex==0,"Brothers","Sisters")
  
  # Read by both ability definitions
  all_ci_summary <- lapply(NT, function(natural_talents) {
    data <- readWorkbook(paste0("results/by_outcome/full_results_",outcome,"_",natural_talents,"_",sex_lab,"_MI.xlsx"), sheet = "For plotting") 
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
ci_summary$ability <- factor(ci_summary$ability, 
                             levels = c("PGI", "observed"), 
                             labels = c("PGI", "Observed"))
# Create the bar graph
# Main elements
ggplot(ci_summary, aes(x = Outcome, y = Estimate, fill = Index)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                position = position_dodge(0.9), width = 0.25, alpha = 0.4) +
  facet_grid(sex ~ ability, labeller = label_wrap_gen()) +
  geom_text(aes(label = round(Estimate, 2)), 
            position = position_dodge(width = 1), 
            vjust = -3, hjust = -0.1) +
  
  # Labels
  scale_fill_manual(values = c("Sibcorr" = "#F8766D", 
                               "IOLIB" = "#00BA38", 
                               "IORAD" = "#619CFF"),
                    labels = c("Sibling correlation", "Liberal IOP", "Radical IOP")) +
  
  guides(fill = guide_legend(title = NULL)) +
  coord_cartesian(clip = "off") +
  
  # Theme
  theme_bw(base_size = 15) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(" "),
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    axis.text.y  = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "goldenrod1", color = "grey30", linewidth = 0.8),  # Updated here
    strip.text = element_text(size = 14),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "bottom",    
  ) +
  ylim(c(-0.05, 0.7)) +
  ggtitle(OUTCOMES.labs[outcome])


# Save the plot
ggsave(paste0("plots/by_sex_and_ability_MI.png"), width = 10, height = 8, dpi = 300)



