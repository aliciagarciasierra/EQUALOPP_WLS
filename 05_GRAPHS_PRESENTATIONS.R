

######## COMBINING THE GRAPHS FOR THE PRESENTATION ###########

# Clean
rm(list = ls())
source("00_MASTER.R")


# Ensure the patchwork package is installed and loaded
if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork")
}
library(patchwork)

cluster <- T
cluster_lab <- ifelse(cluster,"_cluster","")

# Open lists with graphs
plots_list_pgi <- readRDS(paste0("plots/main/by_outcome/plots_list_PGI",cluster_lab,".rds"))
plots_list_obs <- readRDS(paste0("plots/main/by_outcome/plots_list_observed",cluster_lab,".rds"))



# save all plots in png --------------------

lapply(OUTCOMES, function(outcome) {
  
  # combine plots
  combined_plot_education <- plots_list_pgi[[outcome]] + 
    plots_list_obs[[outcome]] +
    plot_layout(guides = "collect") &  # Note the & instead of +
    theme(
      legend.position = "bottom",
      legend.justification = "center",
      legend.box.just = "center",
      aspect.ratio=1
    ) 
  
  # save
  ggsave(paste0("plots/main/by_outcome/",outcome,cluster_lab,".png"), width = 13, height = 6, dpi = 300)
  
})


# print single plot --------------------

# set preferred outcome
outcome <-"education" 

# Combine the two plots 
combined_plot <- plots_list_pgi[[outcome]] + 
  plots_list_obs[[outcome]] +
  plot_layout(guides = "collect") &  # Note the & instead of +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center",
    aspect.ratio=1
  ) 
# Display the combined plot
print(combined_plot)
