

######## COMBINING THE GRAPHS FOR THE PRESENTATION ###########

# Clean
rm(list = ls())
source("00_MASTER.R")


########################## SETUP ####################################

# Set which outcomes to plot
outcome <- "education"
natural_talents <- "PGI"

# Which data to read
impute <- F
impute_lab <- ifelse(impute,"_MI","")









################### GRAPH  #########################

# Read all
data <- readRDS(paste0("results/results_",outcome,"_",natural_talents,impute_lab,".rds"))

# Take out difference
data <- data %>% filter(Index != "diff")

# Round for plotting
data <- data %>%
  mutate_if(is.numeric, round, 2)

# Sort Indices
data$Index   <- factor(data$Index, levels=INDICES)
data$Dataset <- factor(data$Dataset, levels = c("WLS","MoBa"))



# Create the plot for the current outcome
ggplot(data, aes(x = Outcome, y = Estimate, fill = Index)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                position = position_dodge(0.7), 
                width = 0.25, alpha = 0.9) +
  labs(x = " ", y = "Inequality of Opportunity in Education\n") + 
  geom_text(aes(label = Estimate, y=Upper), 
            position  = position_dodge(width = 0.7), 
            vjust = -1, size = 7) + 
  scale_x_discrete(labels = OUTCOMES.labs) +
  facet_grid(~Dataset) +
  guides(fill = guide_legend(nrow = 1, byrow = F, title = NULL,
                             keywidth=1.2, keyheight=1.2,
                             default.unit="cm")) +
  theme_bw(base_size = 25) +  # Set base font size
  theme(
    text = element_text(size=20),
    axis.title.x = element_blank(),  
    axis.ticks.x = element_blank(), 
    axis.text.x = element_blank(),
    panel.grid.minor = element_blank(),  
    panel.grid.major.x = element_blank(),
    legend.position = "top",  
    strip.background = element_rect(fill="white"),
    strip.text = element_text(size=20),
    legend.text=element_text(size=20),
    legend.spacing.y = unit(0.1, "pt")
  ) +
  scale_y_continuous(limits = c(0, 0.7), expand = expansion(mult = c(0, 0))) +
  scale_fill_manual(labels = INDICES.labs, values=c("#F0B70F", "#7ABA3A", "#E83B3F")) 


# Save
ggsave(paste0("plots/",outcome,impute_lab,"_parents.png"), width = 10, height = 10)







