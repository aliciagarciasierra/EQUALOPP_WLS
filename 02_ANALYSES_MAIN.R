#####################################################################
###################### MAIN ANALYSES ################################
#####################################################################


rm(list=ls()) 
set.seed(123)
source("00_MASTER.R")


########################## SETUP ####################################
# ------- settings
# Set to F if you want to run the code from Rstudio
script <- T

# If script == F, specify manually the arguments:
natural_talents <- "PGI" # or "PGI"
n_boot          <- 10


# If script == T, the arguments are overwritten:
if (script) {
  args = commandArgs(trailingOnly=TRUE)
  args <- strsplit(args, ",")
  
  natural_talents <- args[[1]]
  n_boot          <- args[[2]] %>% as.numeric()
} 

  

# ------- read data
siblings <- readRDS("data/siblings.rds")


# ------- rescale
siblings <- siblings %>%
  mutate_if(is.numeric, scale)



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
  
print("compute main results")

# ------- Compute indices for all outcomes
all_results   <- lapply(OUTCOMES, compute_indexes, data=siblings)
final_results <- do.call(rbind.data.frame, all_results)



##################### CONFIDENCE INTERVALS ############################

# ------- Function to compute Cluster Bootstrapping
compute_indexes_bootstrap <- function(dataset, n_boot, outcome, final_results) {
  
  print(outcome)
  
  # Run bootstrapping
  bootstrap_results <- boot(data      = dataset, 
                            statistic = est_fun, 
                            outcome   = outcome,
                            R         = n_boot
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
    
    # results
    data.frame("Index"=index, "Outcome"=outcome, "Estimate"=value, "Lower"=low, "Upper"=up)
  })
  boot_results <- do.call(rbind,rows)
  
  return(boot_results)

}


print("compute bootstrapping")

# ------- Apply function to each outcome
all_boot_list <- lapply(OUTCOMES, 
                        compute_indexes_bootstrap, 
                        dataset       = siblings, 
                        n_boot        = n_boot, 
                        final_results = final_results
                        #mc.cores = 4
                        )
ci_summary <- do.call(rbind,all_boot_list)




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
saveWorkbook(wb, paste0("results/full_results_",natural_talents,".xlsx"), overwrite = TRUE)





######################### GRAPHS ###############################

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
  ) +
  ylim(c(0,0.5))

# Save the plot
ggsave(paste0("plots/results_plot_",natural_talents,".png"), width = 13, height = 6, dpi = 300)
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
saveRDS(plots_list_pgi, paste0("plots/by_outcome/plots_list_",natural_talents,".rds"))





################### CHECK BIAS #########################

# -- X deprecated X --

## Read the data 
#rows <- lapply(c(T,F), function(cluster) {
#  cluster_lab <- ifelse(cluster,"_cluster","")
#  read_excel(paste0("results/full_results_",natural_talents,cluster_lab,".xlsx"), sheet = "For plotting") %>%
#    select(Index,Outcome,Bias_avg) %>% mutate(sampling=ifelse(cluster,"cluster","simple"))
#})
#
#do.call(rbind,rows) %>%
#  ggplot(aes(x=Index,y=Bias_avg,color=sampling)) + geom_point(size=4) +
#  facet_wrap(~Outcome, nrow=1) +
#  labs(y="average bias") +
#  theme_bw()
#ggsave(paste0("plots/bias_",natural_talents,".pdf"), width = 13, height = 6, dpi = 300)




################### CHECK SINGULARITY #########################
if (!script) {
  # Problem: occupation and observed ability
  outcome_good <- "education"
  data_sample <- siblings
  
  
  # 0 check predictors distribution
  cor_data <- select(data_sample, any_of(OBSERVED_NON_COG))
  cors <- cor(cor_data)
  ggcorrplot::ggcorrplot(cors, lab=T)
  

  
  
  # 2 Compare within-family variance  ---------------------
  # --> "average within var occupation: 0.876"
  W_var <- aggregate(education ~ familyID, data=data_sample, var) %>% summarise(avg_variance = mean(V1)) %>% pull(avg_variance)
  print(paste("average within var  education:",round(W_var,3)))
  # --> "average within var  education: 0.648"
  W_var <- aggregate(income ~ familyID, data=data_sample, var) %>% summarise(avg_variance = mean(V1)) %>% pull(avg_variance)
  print(paste("average within var  income:",round(W_var,3)))
  # --> "average within var  income: 0.648"
  W_var <- aggregate(wealth ~ familyID, data=data_sample, var) %>% summarise(avg_variance = mean(V1)) %>% pull(avg_variance)
  print(paste("average within var  wealth:",round(W_var,3)))
  # --> "average within var  wealth: 0.648"
  
  
  
  # 3 look at ICC  ----------------------
  
  null_model <- lmer(education ~ 1 + (1|familyID), data=data_sample)
  icc <- VarCorr(null_model)$familyID[1] / (VarCorr(null_model)$familyID[1] + attr(VarCorr(null_model), "sc")^2)
  print(paste("ICC education:",round(icc,3)))
  # --> "ICC education: 0.353"
  
  null_model <- lmer(income ~ 1 + (1|familyID), data=data_sample)
  icc <- VarCorr(null_model)$familyID[1] / (VarCorr(null_model)$familyID[1] + attr(VarCorr(null_model), "sc")^2)
  print(paste("ICC income:",round(icc,3)))
  # --> "ICC income: 0.167"
  
  null_model <- lmer(wealth ~ 1 + (1|familyID), data=data_sample)
  icc <- VarCorr(null_model)$familyID[1] / (VarCorr(null_model)$familyID[1] + attr(VarCorr(null_model), "sc")^2)
  print(paste("ICC wealth:",round(icc,3)))
  # --> "ICC wealth: 0.231"
  
  null_model <- lmer(health_pc ~ 1 + (1|familyID), data=data_sample)
  icc <- VarCorr(null_model)$familyID[1] / (VarCorr(null_model)$familyID[1] + attr(VarCorr(null_model), "sc")^2)
  print(paste("ICC health_pc:",round(icc,3)))
  # --> "ICC health_pc: 0.143"
  
  
  
  # 4 variance components ----------------------
  
  m0_vars <- "1"
  famID   <- "+ (1 | familyID)"
  
  # occupation
  # -- observed
  m1_vars <- paste0("(", cog_vars, "+", noncog_vars,                ")^2")
  m2_vars <- paste0("(", cog_vars, "+", noncog_vars, "+", ascr_vars,")^2")
  
  m0 <- lmer(as.formula(paste(outcome_problem, "~", m0_vars, famID)), data = data_sample)
  m1 <- lmer(as.formula(paste(outcome_problem, "~", m1_vars, famID)), data = data_sample)
  m2 <- lmer(as.formula(paste(outcome_problem, "~", m2_vars, famID)), data = data_sample)
  vcov_m0 <- as.data.frame(VarCorr(m0))
  vcov_m1 <- as.data.frame(VarCorr(m1))
  vcov_m2 <- as.data.frame(VarCorr(m2))
  
  # -- PGI
  m1_vars <- paste0("(", pgi_vars, ")^2")
  m2_vars <- paste0("(", pgi_vars, "+", ascr_vars,")^2")
  
  m0 <- lmer(as.formula(paste(outcome_problem, "~", m0_vars, famID)), data = data_sample)
  m1 <- lmer(as.formula(paste(outcome_problem, "~", m1_vars, famID)), data = data_sample)
  m2 <- lmer(as.formula(paste(outcome_problem, "~", m2_vars, famID)), data = data_sample)
  vcov_m0 <- as.data.frame(VarCorr(m0))
  vcov_m1 <- as.data.frame(VarCorr(m1))
  vcov_m2 <- as.data.frame(VarCorr(m2))

}






