
###################################################################################
###################### DESCRIPTIVES  ###########################################
###################################################################################

source("00_MASTER.R")
siblings <- readRDS("data/siblings.rds")

siblings <- siblings %>%
  mutate_if(is.numeric, scale)



####################################################################
################## TABLE DESCRIPTIVES ##############################
####################################################################

# Extract variables from the globals
variables_of_interest <- c(
  get("OUTCOMES"),
  get("ASCRIBED"),
  get("OBSERVED_NON_COG"),
  get("OBSERVED_COG"),
  get("PGI_COG"),
  get("PGI_NON_COG")
)

# Check the resulting list
print(variables_of_interest)

# Filter the dataset to include only the variables of interest
descriptive_stats <- siblings %>%
  select(all_of(variables_of_interest)) %>%
  summarise(across(everything(), list(
    mean = ~ mean(.x, na.rm = TRUE),
    min = ~ min(.x, na.rm = TRUE),
    max = ~ max(.x, na.rm = TRUE),
    sd = ~ sd(.x, na.rm = TRUE)
  )))

# Reshape for better readability
descriptive_stats_long <- descriptive_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Statistic"),
    names_pattern = "^(.*)_(mean|min|max|sd)$",  # Match suffixes without splitting variable names
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = Statistic,
    values_from = Value
  )
# View the result
print(descriptive_stats_long)

write_xlsx(descriptive_stats_long, path = "descriptives.xlsx")

####################################################################
################## FIGURE 1: HEXBIN CHART ##############################
####################################################################

siblings <- readRDS("siblings.rds")

# Reshape to wide at the family level
wide<-reshape(as.data.frame(siblings),direction="wide", idvar="familyID", timevar="withinID")


##############
#### PGIs ####
##############

# Loop to generate the plots
sapply(PGIs, function(pgi){
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



##############
##### PCs ####
##############

# Loop to generate the plots
sapply(PC_COG, function(pc){
  # Dynamically create the column names for sibling 1 and sibling 2
  x_var <- paste0(pc, ".1")
  y_var <- paste0(pc, ".2")
  
  # Create the hexbin plot for each PGI variable
  hexbin <- ggplot(wide, aes_string(x=x_var, y=y_var)) +
    geom_hex(bins = 80) +
    geom_abline(color="darkgoldenrod1", linetype="dashed", linewidth=0.3) +
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
    ggtitle(paste("Hexbin chart: differences in the", gsub("pc_", "", pc), "PC between siblings")) +
    theme(plot.title = element_text(size=12, face="bold"))
  
  # Display the plot with the correct fill label
  print(hexbin + labs(fill = "Count"))
})




####################################################################
################## FIGURE 2: PGI vs OBSERVED #######################
####################################################################


datas <- lapply(ABILITY_DEFS, function(ability) {
  name <- switch(ability, "polygenic indices"="", "observed ability"=paste0("_",OBSERVED_COG))
  data <- read_excel(paste0("full_results",name,".xlsx"), sheet = "For plotting")
  data %>% mutate(Ability = ability)
})
data_graph <- do.call(rbind, datas)

# Only outcomes of interest
data_graph <- data_graph %>% filter(Outcome %in% OUTCOMES)

# Custom order 
data_graph <- data_graph %>% mutate(
  Index   = factor(Index,   levels = INDICES),
  Outcome = factor(Outcome, levels = OUTCOMES),
  Ability = factor(Ability, levels = ABILITY_DEFS)
)

n <- nrow(siblings)

# labels with sample size
custom_labels <- c("polygenic indices" = paste0("polygenic indices (n=", n, ")"), "observed ability" = paste0("observed ability (n=", n, ")"))


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
  facet_wrap(~ Ability, nrow = 2, labeller = labeller(Ability = custom_labels)) +
  ylim(-0.1,0.5) +
  theme_bw()

ggsave(paste0("plots/comparison_rows.pdf"), width = 11, height = 8, dpi = 300)
   

####################################################################
###################### SIBLING SIMILARITY ##########################
####################################################################

# Reshape to wide at the family level
wide<-reshape(as.data.frame(siblings),direction="wide", idvar="familyID", timevar="withinID")

# compute mean outcome differences
mean_diff <- lapply(OUTCOMES, function(var) {
  x_var <- paste0(var, ".1")
  y_var <- paste0(var, ".2")
  
  all_diff <- wide %>% reframe(diff = abs(get(x_var, wide) - get(y_var, wide))) %>% unlist()
  m <- mean(all_diff) %>% round(2) # 1.851
  list("outcome" = var, "mean_sib_diff" = m)
})

# save
diff_tab <- do.call(rbind.data.frame, mean_diff) %>% cbind("unit" = c("years", "Duncan SEI score", "Dollars", "Dollars", "PC"))
write_csv(diff_tab, "results/siblings_diff_outcomes.csv")



# compute mean differences

mean_diff <- lapply(PGIs, function(var) {
  
  x_var <- paste0(var, ".1")
  y_var <- paste0(var, ".2")
  
  all_diff <- wide %>% 
    reframe(diff = abs(get(x_var, wide) - get(y_var, wide))) %>% unlist()
  m <- mean(all_diff) %>% round(2) # 1.851
  list("PGI" = var, "mean_sib_diff" = m)
})

# save
diff_tab <- do.call(rbind.data.frame, mean_diff) 
write_csv(diff_tab, "results/siblings_diff_PGIs.csv")







