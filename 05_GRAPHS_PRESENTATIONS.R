

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



################### GRAPH SEPARATED BY OUTCOME #########################

lapply(NT, function(natural_talents) {

  # Create plot title
  title = nt.labs[natural_talents]
  
  # Filter data for the current outcome
  data_subset <- readWorkbook(paste0("results/by_outcome/full_results_",outcome,"_",natural_talents,impute_lab,".xlsx"), sheet = "For plotting")
  
  # Take out difference
  data_subset <- data_subset %>% filter(Index != "diff")
  
  # Sort Indices
  data_subset$Index <- factor(data_subset$Index, levels=INDICES)
  
  # Create the plot for the current outcome
  p <- ggplot(data_subset, aes(x = Outcome, y = Estimate, fill = Index)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                  position = position_dodge(0.9), width = 0.25, alpha = 0.9) +
    labs(title=title, x = " ", y = " ") +  # Set custom title
    geom_text(aes(label = round(Estimate, 2)), 
              position = position_dodge(width = 0.9), vjust = -2.5, size = 9) + 
    
    # Add labels
    scale_x_discrete(labels = OUTCOMES.labs) +
    scale_fill_discrete(labels = INDICES.labs) +
    
    # Customize legend
    guides(fill = guide_legend(title = NULL)) +
    
    # Theme adjustments
    theme_bw(base_size = 40) +  # Set base font size
    theme(
      axis.title.x = element_blank(),  
      axis.text.x = element_blank(),  
      axis.ticks.x = element_blank(),
      panel.grid.minor = element_blank(),  # Remove minor grid lines
      plot.margin = margin(10, 10, 10, 10),  # Add space around the plot
      legend.position = "none",  # Remove the legend
      plot.title = element_text(size=30,hjust = 0.5)) +
    scale_y_continuous(limits = c(0, 0.65)) 
  
  
  
  # To view the plot:
  p
  
  # To save the plot
  saveRDS(p, paste0("plots/by_outcome/",outcome,"_",natural_talents,"_MI.rds"))

})




# Combine 

lapply(outcomes, function(outcome) {

  # Open graphs
  plot_pgi <- readRDS(paste0("plots/by_outcome/",outcome,"_PGI",impute_lab,".rds"))
  plot_obs <- readRDS(paste0("plots/by_outcome/",outcome,"_observed",impute_lab,".rds"))

  # combine plots
  plot_pgi + plot_obs +
    plot_layout(guides = "collect") &  # Note the & instead of +
    theme(
      legend.text=element_text(size=30),
      legend.position = "bottom",
      legend.justification = "center",
      legend.box.just = "center",
      aspect.ratio=1
    ) 
  
  # save
  ggsave(paste0("plots/by_outcome/",outcome,impute_lab,".pdf"), width = 15, height = 8)

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

