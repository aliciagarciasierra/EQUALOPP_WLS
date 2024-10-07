
###################################################################################
###################### DESCRIPTIVES  ###########################################
###################################################################################

####################################################################
################## FIGURE 1: HEXBIN CHART ##############################
####################################################################

# Reshape to wide at the family level
wide<-reshape(as.data.frame(siblings),direction="wide", idvar="familyID", timevar="withinID")

# PGI Variables
pgis <- c("pgi_education", "pgi_cognitive", "pgi_math_exam", "pgi_math_ability",  
          "pgi_depression", "pgi_well_being", "pgi_neuroticism")

# Loop to generate the plots
sapply(pgis, function(pgi){
  # Dynamically create the column names for sibling 1 and sibling 2
  x_var <- paste0(pgi, ".1")
  y_var <- paste0(pgi, ".2")
  
  # Create the hexbin plot for each PGI variable
  hexbin <- ggplot(wide, aes_string(x=x_var, y=y_var)) +
    geom_hex(bins = 80) +
    geom_abline(color="darkgoldenrod1", linetype="dashed", size=0.3) +
    theme_bw() +
    scale_x_continuous(breaks=c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4)) +
    scale_y_continuous(breaks=c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4)) +
    scale_fill_continuous(type = "viridis") +
    xlab("Sibling 1") +
    ylab("Sibling 2") +
    theme(axis.title.y = element_text(size = 11, angle = 90)) +
    theme(axis.title.x = element_text(size = 11, angle = 00)) +
    theme(legend.title = element_text(color = "black", size = 8),
          legend.text = element_text(color = "black", size = 7)) +
    ggtitle(paste("Hexbin chart: differences in the", gsub("pgi_", "", pgi), "PGI between siblings")) +
    theme(plot.title = element_text(size=12, face="bold"))
  
  # Display the plot with the correct fill label
  print(hexbin + labs(fill = "Count"))
})



####################################################################
################## FIGURE 2: PGI vs OBSERVED #######################
####################################################################


datas <- lapply(ABILITY_DEFS, function(ability) {
  name <- switch(ability, "polygenic indices"="", "observed ability"=paste0("_",OBSERVED_COG))
  data <- read_excel(paste0("results/full_results",name,".xlsx"), sheet = "For plotting")
  data %>% mutate(Ability = ability)
})
data_graph <- do.call(rbind, datas)

# Custom order 
data_graph$Index   <- factor(data_graph$Index, levels = INDICES)
data_graph$Outcome <- factor(data_graph$Outcome, levels = OUTCOMES)
data_graph$Ability <- factor(data_graph$Ability, levels = ABILITY_DEFS)


# - version 1

# Define grouping for splitting outcomes and create two plots
groups <- cut(1:length(OUTCOMES), 2, labels = FALSE)

# Create the bar graph
lapply(1:2, function(group) {
  
  # filter data based on outcome
  data <- filter(data_graph, Outcome %in% OUTCOMES[groups == group])
  
  # Main elements
  ggplot(data, aes(x = Index, y = Estimate, fill = Index)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                  position = position_dodge(0.9), width = 0.25, alpha = 0.4) +
    labs(title = " ", x = " ", y = " ") +
    geom_text(aes(label = round(Estimate, 2)), position = position_dodge(width = 1), vjust =-0.8 ,hjust=0.3) + 
    
    # Theme
    theme_bw() + 
    theme(axis.text.x = element_blank() ) +
    
    # Add labels
    #scale_x_discrete(labels = INDICES.labs) +
    scale_fill_discrete(labels = INDICES.labs) +
    guides(fill = guide_legend(title = NULL)) +
    
    # Other
    facet_grid(Outcome ~ Ability, labeller = labeller(Outcome = OUTCOMES.labs)) +
    ylim(-0.09,0.6)
  
  ggsave(paste0("plots/comparison_",group,".png"), width = 6, height = 8, dpi = 300)
  
})


# - version 2

ggplot(data_graph, aes(x = Outcome, y = Estimate, fill = Index)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                position = position_dodge(0.9), width = 0.25, alpha = 0.4) +
  labs(title = " ", x = " ", y = " ") +
  geom_text(aes(label = round(Estimate, 2)), position = position_dodge(width = 1), vjust =-1.5 ,hjust=0.3) + 
  
  # Add labels
  scale_x_discrete(labels = OUTCOMES.labs) +
  scale_fill_discrete(labels = INDICES.labs) +
  # Theme
  guides(fill = guide_legend(title = NULL)) +
  facet_wrap(~ Ability, nrow = 2) +
  ylim(-0.08,0.5) +
  theme_bw()

ggsave(paste0("plots/comparison_rows.pdf"), width = 11, height = 8, dpi = 300)



