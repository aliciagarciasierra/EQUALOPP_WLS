

######## COMBINING THE GRAPHS FOR THE PRESENTATION ###########

# Clean
rm(list = ls())

# Ensure the patchwork package is installed and loaded
if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork")
}
library(patchwork)

# Open lists with graphs  --------------------

plots_list_pgi <- readRDS("plots/plots_list_PGI.rds")
plots_list_obs <- readRDS("plots/plots_list_observed.rds")

OUTCOMES <- c("education", "occupation","income", "wealth","health_pc")

 

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
  ggsave(paste0("plots/by_outcome/",outcome,".png"), width = 13, height = 6, dpi = 300)
  
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

