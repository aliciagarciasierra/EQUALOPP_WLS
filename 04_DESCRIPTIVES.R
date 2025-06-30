
###################################################################################
###################### DESCRIPTIVES  ###########################################
###################################################################################

source("00_MASTER.R")

outcome <- "education"
data_list <- readRDS(paste0("data/final_datasets_",outcome,".rds"))

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
######################## TABLES OF RESULTS #########################
####################################################################

# Select outcome
outcome <- "education"

# Loop over ability definition
results_nt <- lapply(NT, function(natural_talents) {

  # Full sample results
  results_full <- readWorkbook(paste0("results/by_outcome/full_results_",outcome,"_",natural_talents,"_MI.xlsx"), 
                          sheet = "For plotting")
  # Add index
  results_full <- results_full %>% mutate(Sample = "Complete")
  
  # Gender results
  results_sex <- lapply(c(0,1), function(which_sex) {
    # Gender label
    sex_lab <- ifelse(which_sex==0,"Brothers","Sisters")
    # Read
    results <- readWorkbook(paste0("results/by_outcome/full_results_",outcome,"_",natural_talents,"_",sex_lab,"_MI.xlsx"), sheet = "For plotting") 
    # Add index
    results <- results %>% mutate(Sample = sex_lab)
  })
  
  bind_rows(results_full, results_sex) %>% mutate(Natural_Talents = natural_talents)

})

# Combine results
results <- bind_rows(results_nt) 

# Convert "Sample" to factor and sort
results <- results %>% mutate(Sample = factor(Sample,levels=c("Complete","Sisters","Brothers")))

# Compute SE from CIs
results <- results %>% 
  mutate(SE = (Upper - Lower) / (2*1.96)) %>% 
  select(-Upper, -Lower) 



####################################################################
# -- Table 1: all indices

# Extract difference and p-values
differences <- results %>% 
  filter(Index=="diff") %>% 
  select(Natural_Talents, Sample, diff=Estimate, pval)

# Reshape to wide format
wide_df <- results %>%
  filter(Index!="diff") %>%
  select(-pval,-Outcome) %>%
  pivot_wider(names_from = Index, values_from = c(Estimate, SE), names_sep = "_")

# Add difference between IORAD and IOLIB
wide_df <- merge(wide_df, differences)

# Recompute difference from rounded values
wide_df <- wide_df %>% mutate(diff=round(Estimate_IORAD,2)-round(Estimate_IOLIB,2))

# Reorder variables
wide_df_1 <- wide_df %>% 
  arrange(Natural_Talents, Sample) %>%
  select(Natural_Talents,  Sample, 
         Estimate_Sibcorr, SE_Sibcorr, 
         Estimate_IOLIB,   SE_IOLIB,
         Estimate_IORAD,   SE_IORAD,
         diff, pval) 


####################################################################
# -- Table 2: difference between brothers and sisters

# Open all runs to get correlation between PGI and observed on same sample
point_estimates <- lapply(c("Brothers","Sisters"), function(sex_lab) {
  est_nt <- lapply(NT, function(natural_talents) {
    # Read
    est <- readRDS(paste0("results/all_runs/",sex_lab,"_",natural_talents,".rds"))
    # New names with natural talent indicator
    new_lib <- paste0("IOLIB_",natural_talents)
    new_rad <- paste0("IORAD_",natural_talents)
    # Rename
    est %>% select(!!new_lib := IOLIB, !!new_rad := IORAD)
  })
  # Combine  
  est <- bind_cols(est_nt)
  # Compute correlation
  lib_cor <- cor(est$IOLIB_PGI, est$IOLIB_observed)
  rad_cor <- cor(est$IORAD_PGI, est$IORAD_observed)
  # Output
  list(Index  = c("IOLIB", "IORAD"), 
       Sample = c(sex_lab, sex_lab), 
       Cor    = c(lib_cor, rad_cor))
})

correlations <- bind_rows(point_estimates)


# Reshape to wide format
wide_df <- results %>% 
  filter(Index %in% c("IOLIB","IORAD"),
         Sample != "Complete") %>%
  select(Index, Estimate, SE, Sample, Natural_Talents) %>%
  pivot_wider(names_from = Natural_Talents, values_from = c(Estimate, SE), names_sep = "_")

# Add correlations
wide_df <- merge(wide_df, correlations)

# Add difference between PGI and observed and test (corrected for correlation)
wide_df <- wide_df %>%
  mutate(
    diff    = abs(Estimate_PGI - Estimate_observed),
    se_diff = sqrt(SE_PGI^2 + SE_observed^2 - 2 * Cor * SE_PGI * SE_observed),
    t_stat  = diff / se_diff,
    pval = 2 * pnorm(abs(t_stat), lower.tail = FALSE)
  ) %>%
  # Recompute difference for display purposes
  mutate(diff = abs(round(Estimate_PGI,2) - round(Estimate_observed,2))) %>%
  select(-t_stat) 

# Sort
wide_df <- wide_df %>% arrange(Index, Sample) 

# Add rows with differences between brothers and sisters
differences <- wide_df %>%
  select(Index, Sample, diff, se_diff) %>%
  pivot_wider(names_from = Sample, values_from = c(diff, se_diff)) %>%
  mutate(
    Sample            = NA_character_,
    Estimate_PGI      = NA_real_,
    Estimate_observed = NA_real_,
    SE_PGI            = NA_real_,
    SE_observed       = NA_real_,
    diff    = diff_Sisters - diff_Brothers,
    se_diff = sqrt(se_diff_Sisters^2 + se_diff_Brothers^2),
    pval    = 2 * pnorm(abs(diff / se_diff), lower.tail = FALSE)
  ) %>%
  # Recompute difference for display purposes
  mutate(diff = round(diff_Sisters,2)-round(diff_Brothers,2)) %>%
  select(Index, Sample, Estimate_PGI, SE_PGI, Estimate_observed, SE_observed, diff, se_diff, pval) %>%
  mutate(Sample = as.factor(Sample))

# Combine with original data
wide_df_2 <- bind_rows(wide_df, differences) %>%
  arrange(Index, is.na(Sample))


####################################################################
# --- Combine tables and correct p-values

# Add index and bind tables
wide_df_1$id <- 1:nrow(wide_df_1)
wide_df_2$id <- (nrow(wide_df_1)+1):(nrow(wide_df_1)+nrow(wide_df_2))
all <- bind_rows(select(wide_df_1,pval,id), select(wide_df_2,pval,id))

# Correct p-values and add stars
all <- all %>% 
  mutate(corr_pval = p.adjust(pval, method = "holm"),
         stars     = add_stars(corr_pval)) %>% select(-pval)

# Merge again with original tables
wide_df_1 <- merge(wide_df_1, all, by="id")
wide_df_2 <- merge(wide_df_2, all, by="id")


####################################################################
# --- Print in Latex format

# Table 1
latex_table <- wide_df_1 %>%
  mutate(
    row_text = glue(
      "& {Sample} & ${round(Estimate_Sibcorr, 2)}$ $({round(SE_Sibcorr, 2)})$ & ${round(Estimate_IOLIB, 2)}$ $({round(SE_IOLIB, 2)})$ & ${round(Estimate_IORAD, 2)}$ $({round(SE_IORAD, 2)})$ & ${round(diff, 2)}^{stars}$ \\\\"
    )) %>% pull(row_text)
latex_table

# Table 2
latex_table <- wide_df_2 %>%
  mutate(
    row_text = glue(
      "& {Sample} & ${round(Estimate_PGI, 2)}$ $({round(SE_PGI, 2)})$ & ${round(Estimate_observed, 2)}$ $({round(SE_observed, 2)})$ & ${round(diff, 2)}^{stars}$ \\\\"
    )) %>% pull(row_text)
latex_table





####################################################################
################## FIGURE 1: HEXBIN CHART ##############################
####################################################################

outcome <- "education"
siblings <- readRDS(paste0("data/siblings_",outcome,".rds"))

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







