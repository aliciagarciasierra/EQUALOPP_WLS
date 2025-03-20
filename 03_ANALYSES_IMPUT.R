#####################################################################
###################### IMPUTED DATA ANALYSES ########################
#####################################################################


rm(list=ls()) 
set.seed(123)
source("00_MASTER.R")



########################## SETUP ####################################
# ------- settings
# Set to F if you want to run the code from Rstudio
script <- T

# If script == F, specify manually the arguments:
natural_talents <- "PGI" # or "observed"
n_boot          <- 1

# If script == T, the arguments are overwritten:
if (script) {
  args = commandArgs(trailingOnly=TRUE)
  args <- strsplit(args, ",")
  
  natural_talents <- args[[1]]
  n_boot          <- args[[2]] %>% as.numeric()
} 


# ------- read data
data_list <-readRDS("data/final_datasets.rds")


# ------- scale numeric variables
data_list <- lapply(data_list, function(df) {
  df %>% mutate_if(is.numeric, scale)
})


# ------- test
data_list <- data_list[1:m]

# ------- check
print(paste0("which talent: ",natural_talents))
print(paste0("n bootstraps: ",n_boot))
print(paste0("m: ",m))



# ------- models specifications

m0_vars <- "1"
famID   <- "+ (1 | familyID)"

if(natural_talents == "PGI") {
  m1_vars <- paste0("(", pgi_vars, ")^2")
  m2_vars <- paste0("(", pgi_vars, "+", ascr_vars,")^2")
  
} else if(natural_talents == "observed") {
  m1_vars <- paste0("(", cog_vars, "+", noncog_vars,                ")^2")
  m2_vars <- paste0("(", cog_vars, "+", noncog_vars, "+", ascr_vars,")^2")
}




######################## POINT VALUES ##############################


print("Compute main results over imputed datasets")

# ------- Compute indices for multiple datasets and outcomes
all_results_list <- mclapply(data_list, function(dataset) {
                          mclapply(OUTCOMES, 
                                   compute_indexes, 
                                   data = dataset, 
                                   mc.cores = 4)
                           },mc.cores = 4)

# ------- Flatten the list of lists into a single data frame
final_results <- do.call(rbind.data.frame, unlist(all_results_list, recursive = FALSE))


# ------- Compute the mean across imputed datasets
final_results <- final_results %>%
  group_by(Outcome, Model) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop")




##################### CONFIDENCE INTERVALS ############################


print("compute bootstrapping over imputed datasets")

# ------- Define function for Multiple Imputation + Cluster Bootstrapping
mi_cluster_boot <- function(data_list, outcome, n_boot) {
  
  print(outcome)
  
  # Perform bootstrapping in parallel for imputed datasets
  boot_results <- mclapply(data_list, function(dataset) {

    boot(data = dataset, 
         statistic = est_fun, 
         outcome   = outcome,
         R         = n_boot)
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


# ------- Run  for all the outcomes

system.time({
all_boot_list <- lapply(OUTCOMES, 
                        mi_cluster_boot, 
                        data_list = data_list, 
                        n_boot    = n_boot)
})
boot_results <- do.call(rbind, all_boot_list)



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
saveWorkbook(wb, paste0("results/full_results_",natural_talents,"_MI.xlsx"), overwrite = TRUE)



######################### GRAPHS ###############################

# Read the data 
data_graph <- read_excel(paste0("results/full_results_",natural_talents,"_MI.xlsx"), sheet = "For plotting")

# Set theme 
set_pilot_family("Avenir Next Medium", title_family = "Avenir Next Demi Bold")

# Custom order 
data_graph$Index   <- factor(data_graph$Index,   levels = INDICES)
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
  ylim(c(0,0.5))

# Save the plot
ggsave(paste0("plots/results_plot_",natural_talents,"_MI.png"), width = 13, height = 6, dpi = 300)
pdf(NULL)


################### GRAPHS SEPARATED BY OUTCOME #########################

title_nt <- switch(natural_talents,"PGI"="PGIs", "observed"="observed abilities")
outcome_titles <- c(
  "education" = "Education with",
  "income" = "Household Income with",
  "wealth" = "Household Wealth with",
  "health_pc" = "Health with"
)
outcome_titles <- paste(outcome_titles, title_nt)
names(outcome_titles) <- OUTCOMES

# Ensure outcomes variable matches your dataset's Outcome column
outcomes <- unique(data_graph$Outcome)  # This will grab the unique outcomes from your data

# Create a named list to store plots
plots_list_pgi <- list()

# Loop through each outcome and store the plot in the list
for (outcome in outcomes) {
  
  # Filter data for the current outcome
  data_subset <- subset(data_graph, Outcome == outcome)
  
  # Create the plot for the current outcome
  p <- ggplot(data_subset, aes(x = Outcome, y = Estimate, fill = Index)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                  position = position_dodge(0.9), width = 0.25, alpha = 0.4) +
    labs(title = outcome_titles[outcome], x = " ", y = " ") +  # Set custom title
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

  
  # Store the plot in the list with the outcome name as the key
  plots_list_pgi[[outcome]] <- p
}

# To view the plot for "education":
print(plots_list_pgi[["education"]])

# To save the list
saveRDS(plots_list_pgi, paste0("plots/plots_list_",natural_talents,"_MI.rds"))
