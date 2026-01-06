

######## COMBINING THE GRAPHS FOR THE PRESENTATION ###########

# Clean
rm(list = ls())
source("00_MASTER.R")


########################## SETUP ####################################

# Set which outcomes to plot
outcome <- "education"

# Which data to read
impute <- T
impute_lab <- ifelse(impute,"_MI","")





################### GRAPH SEPARATED BY OUTCOME #########################

lapply(NT, function(natural_talents) {
  
  # Filter data for the current outcome
  data_subset <- readWorkbook(paste0("results/by_outcome/full_results_",outcome,"_",natural_talents,impute_lab,".xlsx"), sheet = "For plotting")
  
  # Take out difference
  data_subset <- data_subset %>% filter(Index != "diff")
  
  # Sort Indices
  data_subset$Index   <- factor(data_subset$Index, levels=INDICES)
  data_subset$Ability <- natural_talents
  
  # Create the plot for the current outcome
  p <- ggplot(data_subset, aes(x = Outcome, y = Estimate, fill = Index)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                  position = position_dodge(0.9), width = 0.25, alpha = 0.9) +
    labs(x = " ", y = " ") + 
    geom_text(aes(label = round(Estimate, 2)), 
              position = position_dodge(width = 0.9), vjust = -2.5, size = 9) + 
    
    # Add labels
    scale_x_discrete(labels = OUTCOMES.labs) +
    
    # Grid
    facet_wrap(~ Ability, labeller = labeller(Ability = nt.labs)) +
    
    # Customize legend
    guides(fill = guide_legend(title = NULL)) +
    
    # Theme adjustments
    theme_minimal(base_size = 35) +  # Set base font size
    theme(
      axis.title.x = element_blank(),  
      axis.text.x = element_blank(),  
      axis.ticks.x = element_blank(),
      panel.grid.minor = element_blank(),  # Remove minor grid lines
      panel.grid.major.x = element_blank(),
      legend.position = "none",  # Remove the legend
      legend.key.size = unit(1,"cm")
    ) +
    
    scale_y_continuous(limits = c(0, 0.65)) +
    
    # Color scale
    scale_fill_manual(labels = INDICES.labs, values=c("#FDE725FF", "#5DC863FF", "#5287EB"))
    
  
  # To view the plot:
  p
  
  # To save the plot
  saveRDS(p, paste0("plots/by_outcome/",outcome,"_",natural_talents,"_MI.rds"))

})


# Combine 

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
ggsave(paste0("plots/",outcome,impute_lab,".jpeg"), width = 15, height = 8)








