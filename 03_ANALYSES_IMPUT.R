

###################################################################################
###################### ANALYSES WLS   ###########################################
###################################################################################
set.seed(123)
source("00_MASTER.R")

#------------- settings
args = commandArgs(trailingOnly=TRUE)
args <- strsplit(args, ",")

natural_talents <- args[[1]]
n_boot          <- args[[2]] %>% as.numeric()


#------------- read data
final_datasets <-readRDS("data/final_datasets.rds")


#------------- scale numeric variables
final_datasets <- lapply(final_datasets, function(df) {
  df %>% mutate_if(is.numeric, scale)
})


#-------------- models specifications
ascr_vars    <- paste(ASCRIBED,         collapse=" + ")
pgi_vars     <- paste(PGIs,             collapse=" + ")
cog_vars     <- paste(OBSERVED_COG,     collapse=" + ")
noncog_vars  <- paste(OBSERVED_NON_COG, collapse=" + ")

m0_vars <- "1"
famID   <- "+ (1 | familyID)"

if(natural_talents == "PGI") {
  m1_vars <- paste0("(", pgi_vars, ")^2")
  m2_vars <- paste0("(", pgi_vars, "+", ascr_vars,")^2")
  
} else if(natural_talents == "observed") {
  m1_vars <- paste0("(", cog_vars, "+", noncog_vars,                ")^2")
  m2_vars <- paste0("(", cog_vars, "+", noncog_vars, "+", ascr_vars,")^2")
}


#--------------- redefine outcomes for results excluding health individual indices

OUTCOMES <- c("education", "occupation","income", "wealth","health_pc")
OUTCOMES.labs <- c("education" = "Education", "occupation" = "Occupation", 
                   "income_ind" = "Income Ind", "income" = "Income", 
                   "wealth" = "Wealth", "health_pc" = "Health")


#-------------- Function to compute the main indexes 

compute_indexes <- function(outcome_var, siblings) {
  
  # 1) NULL MODEL
  m0 <- lmer(as.formula(paste(outcome_var, "~", m0_vars, famID)), data = siblings)
  
  vcov_m0 <- as.data.frame(VarCorr(m0))
  emptyind <- vcov_m0[2, 4]
  emptyfam <- vcov_m0[1, 4]
  totalvar <- (vcov_m0[1, 4] + vcov_m0[2, 4])
  
  # 2) CONDITIONAL MODEL
  m1 <- lmer(as.formula(paste(outcome_var, "~", m1_vars, famID)), data = siblings)
  
  vcov_m1 <- as.data.frame(VarCorr(m1))
  condind <- vcov_m1[2, 4]
  condfam <- vcov_m1[1, 4]
  
  # 3) COMPLETE MODEL
  m2 <- lmer(as.formula(paste(outcome_var, "~", m2_vars, famID)), data = siblings)
  
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
    Sibcorr = c(sibcorr, NA, NA),
    condcorr = c(condcorr, NA, NA),
    w = c(w, NA, NA),
    v = c(v, NA, NA),
    IOLIB = c(IOLIB, NA, NA),
    IORAD = c(NA, NA, IORAD)
  )
  
  return(result_df)
}

#---------------- Store the results from the main analyses

print("Compute main results over imputed datasets")

# Apply function to each dataset in final_datasets
all_results_list <- mclapply(final_datasets, function(dataset) {
  mclapply(OUTCOMES, compute_indexes, siblings = dataset, mc.cores = 4)
},mc.cores = 4)

# Flatten the list of lists into a single data frame
final_results <- do.call(rbind.data.frame, unlist(all_results_list, recursive = FALSE))

# Compute the mean across imputed datasets
final_results <- final_results %>%
  group_by(Outcome, Model) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")

# Print or return the final averaged results
print(final_results)



# ------- Compute bootstrapping for multiple datasets

print("compute bootstrapping over imputed datasets")

# Function to be bootstrapped - returns all parameters of interest
est_fun <- function(data, indices) {
  # Compute the models and the statistics (similar to compute_indexes function)
  m0 <- lmer(as.formula(paste(outcome, "~", m0_vars, famID)), data = data[indices, ])
  m1 <- lmer(as.formula(paste(outcome, "~", m1_vars, famID)), data = data[indices, ])
  m2 <- lmer(as.formula(paste(outcome, "~", m2_vars, famID)), data = data[indices, ])
  
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
  sibcorr  <- emptyfam / totalvar
  condcorr <- condfam / totalvar
  w <- (condind - completeind) / totalvar
  v <- (emptyind - completeind) / totalvar
  IOLIB <- w + condcorr
  IORAD <- v + sibcorr
  
  # Return a numeric vector with the key statistics (same order as INDICES)
  return(c("Sibcorr"=sibcorr,"IOLIB"=IOLIB, "IORAD"=IORAD))
}





# Define an function for MI + boot following (Schomaker et al, 2018)
mi_boot <- function(data_list, outcome, m, n_boot) {
  
  # Function to be bootstrapped - returns all parameters of interest
  est_fun <- function(data, indices) {
    # Compute the models and the statistics (similar to compute_indexes function)
    m0 <- lmer(as.formula(paste(outcome, "~", m0_vars, famID)), data = data[indices, ])
    m1 <- lmer(as.formula(paste(outcome, "~", m1_vars, famID)), data = data[indices, ])
    m2 <- lmer(as.formula(paste(outcome, "~", m2_vars, famID)), data = data[indices, ])
    
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
    sibcorr  <- emptyfam / totalvar
    condcorr <- condfam / totalvar
    w <- (condind - completeind) / totalvar
    v <- (emptyind - completeind) / totalvar
    IOLIB <- w + condcorr
    IORAD <- v + sibcorr
    
    # Return a numeric vector with the key statistics (same order as INDICES)
    return(c("Sibcorr"=sibcorr,"IOLIB"=IOLIB, "IORAD"=IORAD))
  }
  

  # Step 2: Bootstrap each imputed dataset
  boot_results <- mclapply(1:length(data_list), function(i) {
    
    # Write to a process-specific log file
    log_msg <- paste("Process", Sys.getpid(), "- Data", i, "started at", Sys.time(), "\n")
    cat(log_msg, file = paste0("logs/",natural_talents,"/log_process_", Sys.getpid(), ".txt"), append = TRUE)
    
    # run
    data_i <- data_list[[i]]
    boot_res <- boot(data = data_i, statistic = est_fun, R = n_boot)
    
    # Log completion
    cat(paste("Process", Sys.getpid(), "- Data", i, "completed at", Sys.time(), "\n"), 
        file = paste0("logs/",natural_talents,"/log_process_", Sys.getpid(), ".txt"), append = TRUE)
    
    # return results
    return(boot_res)
  }, mc.cores = 4)
  

  # Get the coefficient names
  coef_names <- INDICES
  
  # Extract original estimates for each coefficient in each imputed dataset 
  point_estimates <- lapply(boot_results, function(boot_obj) boot_obj$t0)
  point_estimates_matrix <- do.call(rbind, point_estimates)
  
  # Extract bootstrapped coefficient matrices 
  boot_matrices <- lapply(boot_results, function(boot_obj) {
    colnames(boot_obj$t) <- coef_names
    boot_obj$t
    })
  
  
  # Step 3: Apply Rubin's rules for each coefficient
  mi_boot_results <- lapply(1:length(coef_names), function(j) {
    coef_name <- coef_names[j]
    
    # Point estimates for this coefficient across imputations
    theta_i <- point_estimates_matrix[, j]
    
    # Calculate bootstrap standard deviations for this coefficient
    boot_sds <- sapply(boot_matrices, function(mat) sd(mat[, j]))
    
    # Point estimate
    theta_MI <- mean(theta_i)
    
    # Within-imputation variance (average bootstrap variance)
    W <- mean(boot_sds^2)
    
    # Between-imputation variance
    B <- var(theta_i)
    
    # Total variance
    T_var <- W + (1 + 1/m) * B
    
    # Degrees of freedom for t-distribution
    df <- (m - 1) * (1 + (W / ((1 + 1/m) * B)))^2
    
    # Confidence interval (95%)
    ci_lower <- theta_MI - qt(0.975, df) * sqrt(T_var)
    ci_upper <- theta_MI + qt(0.975, df) * sqrt(T_var)
    
    return(data.frame(
      Index    = coef_name,
      Outcome  = outcome,
      Estimate = theta_MI,
      Lower    = ci_lower, 
      Upper    = ci_upper
    ))
  })
  
  mi_boot_results_df <- do.call(rbind, mi_boot_results)
  
  return(mi_boot_results_df)
}


# Define a function for MI + boot following (Schomaker et al, 2018)
# With cluster resampling
mi_cluster_boot <- function(data_list, outcome, n_boot) {
  
  # - 1 - Function to be bootstrapped 
  est_fun <- function(data, indices) {
    # Compute the models and the statistics
    m0 <- lmer(as.formula(paste(outcome, "~", m0_vars, famID)), data = data[indices, ])
    m1 <- lmer(as.formula(paste(outcome, "~", m1_vars, famID)), data = data[indices, ])
    m2 <- lmer(as.formula(paste(outcome, "~", m2_vars, famID)), data = data[indices, ])
    
    # Extract variance components
    vcov_m0 <- as.data.frame(VarCorr(m0))
    vcov_m1 <- as.data.frame(VarCorr(m1))
    vcov_m2 <- as.data.frame(VarCorr(m2))
    
    # Perform index computations
    emptyind <- vcov_m0[2, 4]
    emptyfam <- vcov_m0[1, 4]
    totalvar <- emptyfam + emptyind
    condind <- vcov_m1[2, 4]
    condfam <- vcov_m1[1, 4]
    completeind <- vcov_m2[2, 4]
    completefam <- vcov_m2[1, 4]
    sibcorr  <- emptyfam / totalvar
    condcorr <- condfam / totalvar
    w <- (condind - completeind) / totalvar
    v <- (emptyind - completeind) / totalvar
    IOLIB <- w + condcorr
    IORAD <- v + sibcorr
    
    # Return a numeric vector with the key statistics
    return(c("Sibcorr"=sibcorr, "IOLIB"=IOLIB, "IORAD"=IORAD))
  }
  
  # - 2 - Cluster sampling function that respects family structure
  cluster_indices <- function(data) {
    
    # Get unique family IDs
    family_ids <- unique(data[["familyID"]])
    
    # Sample families with replacement
    sampled_families <- sample(family_ids, length(family_ids), replace = TRUE)
    
    # Create indices for all observations in the sampled families
    indices <- numeric(0)
    for (fam in sampled_families) {
      fam_rows <- which(data[["familyID"]] == fam)
      indices <- c(indices, fam_rows)
    }
    
    return(indices)
  }
  
  system.time({
  # - 3 - Bootstrap each imputed dataset
  boot_results <- mclapply(1:length(data_list), function(i) {
    
    # Write to a process-specific log file
    #log_msg <- paste("Process", Sys.getpid(), "- Data", i, "started at", Sys.time(), "\n")
    #cat(log_msg, file = paste0("logs/",natural_talents,"/log_process_", Sys.getpid(), ".txt"), append = TRUE)
    
    # get one dataset
    data_i <- data_list[[i]]
    
    # run bootstrap with cluster sampling
    boot_res <- boot(
      data = data_i, 
      statistic = est_fun, 
      R = n_boot,
      sim = "parametric",
      ran.gen = function(data, p) data[cluster_indices(data), ],
      mle = data_i  # Original data as MLE
    )
    
    # Log completion
    #cat(paste("Process", Sys.getpid(), "- Data", i, "completed at", Sys.time(), "\n"), 
    #    file = paste0("logs/",natural_talents,"/log_process_", Sys.getpid(), ".txt"), append = TRUE)
    
    # return results
    return(boot_res)
  }, mc.cores = 4)
  })
  
  ##### TEST   #####
  #boot_1 <- boot_results[[1]]
  #i <- 2
  #index <- boot_1$t[,i]
  #hist(index, 
  #     main = paste0("Histogram of ", INDICES[i], " - ", n_boot, " iters"))
  
  #####.#####  #####
  
  # Get the coefficient names
  coef_names <- INDICES
  
  # Extract original estimates for each coefficient in each imputed dataset 
  point_estimates <- lapply(boot_results, function(boot_obj) boot_obj$t0)
  point_estimates_matrix <- do.call(rbind, point_estimates)
  
  # Extract bootstrapped coefficient matrices 
  boot_matrices <- lapply(boot_results, function(boot_obj) {
    colnames(boot_obj$t) <- coef_names
    boot_obj$t
  })
  
  # Step 3: Apply Rubin's rules for each coefficient
  mi_boot_results <- lapply(1:length(coef_names), function(j) {
    coef_name <- coef_names[j]
    
    # Point estimates for this coefficient across imputations
    theta_i <- point_estimates_matrix[, j]
    
    # Calculate bootstrap standard deviations for this coefficient
    boot_sds <- sapply(boot_matrices, function(mat) sd(mat[, j]))
    
    # Point estimate
    theta_MI <- mean(theta_i)
    
    # Within-imputation variance (average bootstrap variance)
    W <- mean(boot_sds^2)
    
    # Between-imputation variance
    B <- var(theta_i)
    
    # Total variance
    T_var <- W + (1 + 1/length(data_list)) * B
    
    # Degrees of freedom for t-distribution
    df <- (length(data_list) - 1) * (1 + (W / ((1 + 1/length(data_list)) * B)))^2
    
    # Confidence interval (95%)
    ci_lower <- theta_MI - qt(0.975, df) * sqrt(T_var)
    ci_upper <- theta_MI + qt(0.975, df) * sqrt(T_var)
    
    return(data.frame(
      Index    = coef_name,
      Outcome  = outcome,
      Estimate = theta_MI,
      Lower    = ci_lower, 
      Upper    = ci_upper
    ))
  })
  
  mi_boot_results_df <- do.call(rbind, mi_boot_results)
  
  return(mi_boot_results_df)
}

library(lme4)
library(boot)
library(parallel)

# Define function for Multiple Imputation + Cluster Bootstrapping
mi_cluster_boot <- function(data_list, outcome, n_boot) {
  
  # Function to estimate statistics from resampled data
  est_fun <- function(data, indices) {
    # Fit mixed models
    m0 <- lmer(as.formula(paste(outcome, "~", m0_vars, "+ (1|familyID)")), data = data[indices, ])
    m1 <- lmer(as.formula(paste(outcome, "~", m1_vars, "+ (1|familyID)")), data = data[indices, ])
    m2 <- lmer(as.formula(paste(outcome, "~", m2_vars, "+ (1|familyID)")), data = data[indices, ])
    
    # Extract variance components
    vcov_m0 <- as.data.frame(VarCorr(m0))
    vcov_m1 <- as.data.frame(VarCorr(m1))
    vcov_m2 <- as.data.frame(VarCorr(m2))
    
    # Compute statistics
    emptyind <- vcov_m0[vcov_m0$grp == "Residual", "vcov"]
    emptyfam <- vcov_m0[vcov_m0$grp == "familyID", "vcov"]
    totalvar <- emptyfam + emptyind
    condind <- vcov_m1[vcov_m1$grp == "Residual", "vcov"]
    condfam <- vcov_m1[vcov_m1$grp == "familyID", "vcov"]
    completeind <- vcov_m2[vcov_m2$grp == "Residual", "vcov"]
    completefam <- vcov_m2[vcov_m2$grp == "familyID", "vcov"]
    
    sibcorr <- emptyfam / totalvar
    condcorr <- condfam / totalvar
    w <- (condind - completeind) / totalvar
    v <- (emptyind - completeind) / totalvar
    IOLIB <- w + condcorr
    IORAD <- v + sibcorr
    
    return(c("Sibcorr" = sibcorr, "IOLIB" = IOLIB, "IORAD" = IORAD))
  }
  
  # Function to perform cluster resampling
  cluster_indices <- function(data) {
    sampled_families <- sample(unique(data$familyID), replace = TRUE)
    return(unlist(lapply(sampled_families, function(fam) which(data$familyID == fam))))
  }
  
  # Perform bootstrapping in parallel for each imputed dataset
  boot_results <- mclapply(seq_along(data_list), function(i) {
    boot(data = data_list[[i]], 
         statistic = est_fun, 
         R = n_boot, 
         sim = "ordinary", 
         stype = "i", 
         ran.gen = function(data, p) data[cluster_indices(data), ])
  }, mc.cores = detectCores() - 1)
  
  # Combine results using Rubin's rules
  coef_names <- INDICES
  point_estimates <- do.call(rbind, lapply(boot_results, function(boot_obj) boot_obj$t0))
  
  boot_matrices <- lapply(boot_results, function(boot_obj) {
    colnames(boot_obj$t) <- coef_names
    boot_obj$t
  })
  
  mi_boot_results <- lapply(seq_along(coef_names), function(j) {
    theta_i <- point_estimates[, j]
    boot_sds <- sapply(boot_matrices, function(mat) sd(mat[, j]))
    
    theta_MI <- mean(theta_i)
    W <- mean(boot_sds^2)
    B <- var(theta_i)
    T_var <- W + (1 + 1/length(data_list)) * B
    df <- (length(data_list) - 1) * (1 + (W / ((1 + 1/length(data_list)) * B)))^2
    ci_lower <- theta_MI - qt(0.975, df) * sqrt(T_var)
    ci_upper <- theta_MI + qt(0.975, df) * sqrt(T_var)
    
    return(data.frame(
      Index = coef_names[j],
      Outcome = outcome,
      Estimate = theta_MI,
      Lower = ci_lower, 
      Upper = ci_upper
    ))
  })
  
  return(do.call(rbind, mi_boot_results))
}


# Run for all outcomes  ----------------
data_list <- list(final_datasets[[1]])

all_boot_list <- lapply(OUTCOMES, function(outcome) 
  mi_cluster_boot(data_list = data_list, outcome = outcome, n_boot = n_boot))

boot_results <- do.call(rbind, all_boot_list)



# run for all the outcomes ----------------

system.time({
all_boot_list <- lapply(OUTCOMES, mi_boot, 
                          data_list = final_datasets, 
                          m         = m, 
                          n_boot    = n_boot)
})
boot_results <- do.call(rbind, all_boot_list)


# lapply in data_list:   91.217 
# mclapply in data_list: 132.634
# lapply in outcomes and mc in data_list: 66.497


#----------------Store all results

# Create a new Excel workbook
wb <- createWorkbook()

# Add results sheet
addWorksheet(wb, "Full results")
writeData(wb, "Full results", final_results)

# Add confidence intervals sheet
addWorksheet(wb, "For plotting")
writeData(wb, "For plotting", boot_results)

# Save the workbook to an Excel file
saveWorkbook(wb, paste0("results/full_results_",natural_talents,".xlsx"), overwrite = TRUE)



#---------------- Plot the graph

# Read the data 
data_graph <- read_excel(paste0("results/full_results_",natural_talents,".xlsx"), sheet = "For plotting")

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
  )

# Save the plot
ggsave(paste0("plots/results_plot_",natural_talents,".png"), width = 13, height = 6, dpi = 300)
pdf(NULL)


################### GRAPHS SEPARATED BY OUTCOME #########################

title_nt <- switch(natural_talents,"PGI"="PGIs", "observed"="observed abilities")
outcome_titles <- c(
  "education" = "Education with",
  "occupation" = "Occupation with",
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
saveRDS(plots_list_pgi, paste0("plots/plots_list_",natural_talents,".rds"))
