#####################################################################
###################### MAIN ANALYSES ################################
#####################################################################


rm(list=ls()) 

set.seed(123)
source("00_MASTER.R")


#------------- settings
script <- T

# If script == F, specify manually the arguments:
natural_talents <- "PGI" # or "observed"
n_boot          <- 100
cluster         <- T


# If script == T, the arguments are overwritten:
if (script) {
  args = commandArgs(trailingOnly=TRUE)
  args <- strsplit(args, ",")
  
  natural_talents <- args[[1]]
  n_boot          <- args[[2]] %>% as.numeric()
  cluster         <- args[[3]] %>% as.logical()
} 

cluster_lab <- ifelse(cluster,"_cluster","")
  


#------------- read data
siblings <- readRDS("data/siblings.rds")


#------------- rescale
siblings <- siblings %>%
  mutate_if(is.numeric, scale)


#-------------- models specifications
ascr_vars    <- paste(ASCRIBED,     collapse=" + ")
pgi_vars     <- paste(PGIs, collapse=" + ")
cog_vars     <- paste(OBSERVED_COG, collapse=" + ")
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


#-------------- Function to compute the main indexes 

compute_indexes <- function(outcome_var, siblings) {
  
  # 1) NULL MODEL
  m0 <- lmer(as.formula(paste(outcome_var, "~", m0_vars, famID)), data = siblings)
  
  vcov_m0 <- as.data.frame(VarCorr(m0))
  emptyind <- vcov_m0[vcov_m0$grp == "Residual", "vcov"]
  emptyfam <- vcov_m0[vcov_m0$grp == "familyID", "vcov"]
  totalvar <- emptyfam + emptyind
  
  # 2) CONDITIONAL MODEL
  m1 <- lmer(as.formula(paste(outcome_var, "~", m1_vars, famID)), data = siblings)
  
  vcov_m1 <- as.data.frame(VarCorr(m1))
  condind <- vcov_m1[vcov_m1$grp == "Residual", "vcov"]
  condfam <- vcov_m1[vcov_m1$grp == "familyID", "vcov"]
  
  # 3) COMPLETE MODEL
  m2 <- lmer(as.formula(paste(outcome_var, "~", m2_vars, famID)), data = siblings)
  
  vcov_m2 <- as.data.frame(VarCorr(m2))
  completeind <- vcov_m2[vcov_m2$grp == "Residual", "vcov"]
  completefam <- vcov_m2[vcov_m2$grp == "familyID", "vcov"]
  
  # Index computations
  Sibcorr  <- emptyfam / totalvar
  condcorr <- condfam / totalvar
  w <- (condind - completeind) / totalvar
  v <- (emptyind - completeind) / totalvar
  
  IOLIB <- w + condcorr
  IORAD <- v + Sibcorr
  
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
    Sibcorr = c(Sibcorr, NA, NA),
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
all_results   <- lapply(OUTCOMES, compute_indexes, siblings=siblings)
final_results <- do.call(rbind.data.frame, all_results)



##---------- Function to compute confidence intervals through bootsrapping

compute_indexes_bootstrap <- function(dataset, n_boot, outcome, final_results) {

  # Function to perform cluster resampling
  cluster_indices <- function(data) {
    sampled_families <- sample(unique(data$familyID), replace = TRUE)
    return(unlist(lapply(sampled_families, function(fam) which(data$familyID == fam))))
  }
  
  # Function to bootstrap
  est_fun <- function(bootstrap_data, indices) {
    # Subset the data for this bootstrap sample
    data_sample <- bootstrap_data[indices, ]
    
    # Cluster resampling
    if (cluster) data_sample <- cluster_data_boot(data_sample)
    
    # Compute the models and the statistics (similar to compute_indexes function)
    m0 <- lmer(as.formula(paste(outcome, "~", m0_vars, famID)), data = data_sample)
    m1 <- lmer(as.formula(paste(outcome, "~", m1_vars, famID)), data = data_sample)
    m2 <- lmer(as.formula(paste(outcome, "~", m2_vars, famID)), data = data_sample)
    
    # Extract variance components
    vcov_m0 <- as.data.frame(VarCorr(m0))
    vcov_m1 <- as.data.frame(VarCorr(m1))
    vcov_m2 <- as.data.frame(VarCorr(m2))
    
    # Perform your index computations (same as in compute_indexes)
    emptyind    <- vcov_m0[vcov_m0$grp == "Residual", "vcov"]
    emptyfam    <- vcov_m0[vcov_m0$grp == "familyID", "vcov"]
    totalvar    <- emptyfam + emptyind
    condind     <- vcov_m1[vcov_m1$grp == "Residual", "vcov"]
    condfam     <- vcov_m1[vcov_m1$grp == "familyID", "vcov"]
    completeind <- vcov_m2[vcov_m2$grp == "Residual", "vcov"]
    completefam <- vcov_m2[vcov_m2$grp == "familyID", "vcov"]

    Sibcorr  <- emptyfam / totalvar
    condcorr <- condfam / totalvar
    w <- (condind - completeind) / totalvar
    v <- (emptyind - completeind) / totalvar
    
    IOLIB <- w + condcorr
    IORAD <- v + Sibcorr
    
    # Return a numeric vector with the key statistics
    return(c(Sibcorr, IOLIB, IORAD))
  }
  
  
  # Run bootstrapping
  sim_type <- ifelse(cluster, "parametric","ordinary")
  bootstrap_results <- boot(data      = dataset, 
                            statistic = est_fun, 
                            R         = n_boot, 
                            sim       = sim_type, 
                            ran.gen = function(data, p) data[cluster_indices(data), ]
                            )
  
  # Filter original outcome estimates
  outcome_results <- filter(final_results, Outcome==outcome)
  
  # Calculate SE and confidence intervals
  rows <- lapply(1:length(INDICES), function(i) {
    index <- INDICES[i]
    boots <- bootstrap_results$t[, i]
    
    # confidence intervals
    se    <- sd(boots)
    value <- outcome_results[index][!is.na(outcome_results[index])]  # only non Na value in column
    up  <- value + 1.96 * se
    low <- value - 1.96 * se
    
    # bias
    bias_values <- boots - value
    
    # Now bias_values contains the bias distribution
    bias_mean <- mean(bias_values)
    bias_sd   <- sd(bias_values)
    
    # results
    data.frame("Index"=index, "Outcome"=outcome, "Estimate"=value, "Lower"=low, "Upper"=up,
               "Bias_avg"=bias_mean, "Bias_se"=bias_sd)
  })
  boot_results <- do.call(rbind,rows)
  
  return(boot_results)

}


print("compute bootstrapping")

# Apply function to each dataset in final_datasets
all_boot_list <- mclapply(OUTCOMES, 
                          compute_indexes_bootstrap, 
                          dataset       = siblings, 
                          n_boot        = n_boot, 
                          final_results = final_results,
                          mc.cores = 4)

ci_summary <- do.call(rbind,all_boot_list)



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
saveWorkbook(wb, paste0("results/main/full_results_",natural_talents,cluster_lab,".xlsx"), overwrite = TRUE)




#---------------- Plot the graph

# Read the data 
data_graph <- read_excel(paste0("results/main/full_results_",natural_talents,cluster_lab,".xlsx"), sheet = "For plotting")


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
ggsave(paste0("plots/main/results_plot_",natural_talents,cluster_lab,".png"), width = 13, height = 6, dpi = 300)
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
saveRDS(plots_list_pgi, paste0("plots/main/by_outcome/plots_list_",natural_talents,cluster_lab,".rds"))





################### CHECK BIAS #########################

# Read the data 
rows <- lapply(c(T,F), function(cluster) {
  cluster_lab <- ifelse(cluster,"_cluster","")
  read_excel(paste0("results/main/full_results_",natural_talents,cluster_lab,".xlsx"), sheet = "For plotting") %>%
    select(Index,Outcome,Bias_avg) %>% mutate(sampling=ifelse(cluster,"cluster","simple"))
})

do.call(rbind,rows) %>%
  ggplot(aes(x=Index,y=Bias_avg,color=sampling)) + geom_point(size=4) +
  facet_wrap(~Outcome, nrow=1) +
  labs(y="average bias") +
  theme_bw()
ggsave(paste0("plots/main/bias_",natural_talents,".pdf"), width = 13, height = 6, dpi = 300)

