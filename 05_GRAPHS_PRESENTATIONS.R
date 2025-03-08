

######## COMBINING THE GRAPHS FOR THE PRESENTATION ###########

# Clean
rm(list = ls())

# Ensure the patchwork package is installed and loaded
if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork")
}
library(patchwork)

# Open lists with graphs
plots_list_pgi <- readRDS("plots/plots_list_PGI.rds")
plots_list_obs <- readRDS("plots/plots_list_observed.rds")

# Combine the two plots for "education"
combined_plot_education <- plots_list_pgi[["education"]] + 
  plots_list_obs[["education"]] +
  plot_layout(guides = "collect") &  # Note the & instead of +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center"
  )
# Display the combined plot
print(combined_plot_education)

# Combine the two plots for "occupation"
combined_plot_occupation <- plots_list_pgi[["occupation"]] + 
  plots_list_obs[["occupation"]] +
  plot_layout(guides = "collect") &  # Note the & instead of +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center"
  )
# Display the combined plot
print(combined_plot_occupation)


# Combine the two plots for "income"
combined_plot_income <- plots_list_pgi[["income"]] + 
  plots_list_obs[["income"]] +
  plot_layout(guides = "collect") &  # Note the & instead of +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center"
  )
# Display the combined plot
print(combined_plot_income)

# Combine the two plots for "wealth"
combined_plot_wealth <- plots_list_pgi[["wealth"]] + 
  plots_list_obs[["wealth"]] +
  plot_layout(guides = "collect") &  # Note the & instead of +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center"
  )
# Display the combined plot
print(combined_plot_wealth)

# Combine the two plots for "health_pc"
combined_plot_health_pc <- plots_list_pgi[["health_pc"]] + 
  plots_list_obs[["health_pc"]] +
  plot_layout(guides = "collect") &  # Note the & instead of +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center"
  )
# Display the combined plot
print(combined_plot_health_pc)
