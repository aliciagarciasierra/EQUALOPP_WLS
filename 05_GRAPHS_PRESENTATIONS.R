

######## COMBINING THE GRAPHS FOR THE PRESENTATION ###########

# Clean
rm(list = ls())
source("00_MASTER.R")


########################## SETUP ####################################

# Set which outcomes to plot
outcomes <- OUTCOMES

# Which data to read
impute <- F

# Ensure the patchwork package is installed and loaded
if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork")
}
library(patchwork)



########################## SAVE ALL PLOTS ####################################



# imputed data?
impute <- T
impute_lab <- ifelse(impute,"_MI","")


lapply(outcomes, function(outcome) {

  # Open graphs
  plot_pgi <- readRDS(paste0("plots/by_outcome/",outcome,"_PGI",impute_lab,".rds"))
  plot_obs <- readRDS(paste0("plots/by_outcome/",outcome,"_observed",impute_lab,".rds"))

  # combine plots
  plot_pgi + plot_obs +
    plot_layout(guides = "collect") &  # Note the & instead of +
    theme(
      legend.text=element_text(size=15),
      legend.position = "bottom",
      legend.justification = "center",
      legend.box.just = "center",
      aspect.ratio=1
    ) 
  
  # save
  ggsave(paste0("plots/by_outcome/",outcome,impute_lab,".pdf"), width = 13, height = 6, dpi = 300)

})






########################## SHOW SINGLE OUTCOME ####################################

# Set preferred outcome and data
outcome <-"education" 

# lab
impute_lab <- ifelse(impute,"_MI","")

# Open lists with graphs
plots_list_pgi <- readRDS(paste0("plots/by_outcome/plots_list_PGI",impute_lab,".rds"))
plots_list_obs <- readRDS(paste0("plots/by_outcome/plots_list_observed",impute_lab,".rds"))

# Combine the two plots 
combined_plot <- plots_list_pgi[[outcome]] + 
  plots_list_obs[[outcome]] +
  plot_layout(guides = "collect") &  # Note the & instead of +
  theme(
    legend.text=element_text(size=15),
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center",
    aspect.ratio=1
  ) 
# Display the combined plot
print(combined_plot)

