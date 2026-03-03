
################################################################################
################################## DESCRIPTIVES  ###############################
################################################################################



source("00_MASTER.R")

outcome <- "education"
data    <- readRDS(paste0("data/siblings_",outcome,".rds"))

table(data$familyID) %>% table()


#################################################################
###################### TABLE DESCRIPTIVES #######################
#################################################################


# Filter the dataset to include only the variables of interest
descriptive_stats <- data %>%
  select(all_of(OUTCOMES), all_of(ASCRIBED), -contains("pc")) %>%
  mutate(sex=as.numeric(as.character(sex))) %>%
  summarise(across(everything(), list(
    mean = ~ mean(.x, na.rm = TRUE),
    min = ~ min(.x, na.rm = TRUE),
    max = ~ max(.x, na.rm = TRUE),
    sd  = ~ sd(.x, na.rm = TRUE)
  )))

# Reshape for better readability
descriptive_stats_long <- descriptive_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Statistic"),
    names_pattern = "^(.*)_(mean|min|max|sd)$",  # Match suffixes without splitting variable names
    values_to = "Value"
  ) %>% pivot_wider(
    names_from = Statistic,
    values_from = Value
  ) %>% mutate_if(is.numeric, round, 2)

# View the result
print(descriptive_stats_long)

write_xlsx(descriptive_stats_long, path = "descriptives.xlsx")




####################################################################
######################## TABLES OF RESULTS #########################
####################################################################

# Set
outcome         <- "education"
natural_talents <- "PGI"

# -- Complete sample results
results_all <- readRDS(paste0("results/results_",outcome,"_",natural_talents,".rds")) %>% 
  mutate(Sample = "Complete", Natural_Talents = natural_talents)

# -- Gender results
results_sex <- readRDS(paste0("results/results_",outcome,"_",natural_talents,"_gender.rds")) %>%
  rename(Sample = sex, Natural_Talents = ability)

# Combine results
results <- bind_rows(results_all, results_sex) 

# Sort
results <- results %>% 
  mutate(Sample = factor(Sample,levels=c("Complete","Sisters","Brothers")))

# Compute SE from CIs
results <- results %>% 
  mutate(SE = (Upper - Lower) / (2*1.96)) %>% 
  select(-Upper, -Lower) 


# Extract p-values and adjust
pvalues <- results %>% 
  filter(Index=="diff") %>% 
  select(Dataset, Natural_Talents, Sample, pval) %>%
  mutate(pval.adj = p.adjust(pval, method="BH"),
         stars    = add_stars(pval.adj))

# Reshape to wide format
wide_df <- results %>%
  select(-pval,-Outcome) %>%
  pivot_wider(names_from = Index, values_from = c(Estimate, SE), names_sep = "_")

# Add back pvalues
wide_df <- merge(wide_df, pvalues)


# Reorder variables
wide_df %>% 
  arrange(Dataset, Sample, Natural_Talents) %>%
  select(Dataset, Sample, N, Natural_Talents, 
         Estimate_Sibcorr, SE_Sibcorr, 
         Estimate_IOLIB,   SE_IOLIB,
         Estimate_IORAD,   SE_IORAD,
         Estimate_diff, pval) %>%
  mutate_if(is.numeric, round, 2)



