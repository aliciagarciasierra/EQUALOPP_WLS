
################################################################################
################################## DESCRIPTIVES  ###############################
################################################################################


setwd("~/Library/CloudStorage/OneDrive-UniversitédeLausanne/UNIL/projects/GenesSkills/EQUALOPP_WLS")
source("00_MASTER.R")

outcome      <- "education"
moba_sample  <- "parents"   # "parents" or "children"
data         <- readRDS(paste0("data/siblings_",outcome,".rds"))



#################################################################
###################### TABLE DESCRIPTIVES #######################
#################################################################


# Filter the dataset to include only the variables of interest
descriptive_stats <- data %>%
  select(all_of(OUTCOMES), all_of(ASCRIBED), -contains("pc"), all_of(PGIs)) %>%
  mutate_at(PGIs, ~ as.numeric(scale(.))) %>%
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
  ) #%>% mutate_if(is.numeric, round, 2)

# View the result
print(descriptive_stats_long)





####################################################################
######################## TABLES OF RESULTS #########################
####################################################################

# Set
outcome         <- "education"
natural_talents <- "PGI"

# -- Complete sample results
results_all <- readRDS(paste0("results/results_",outcome,"_",natural_talents,"_",moba_sample,".rds")) %>%
  mutate(Sample = "Complete", Natural_Talents = natural_talents)

# -- Gender results
results_sex <- readRDS(paste0("results/results_",outcome,"_",natural_talents,"_gender_",moba_sample,".rds")) %>%
  rename(Natural_Talents = ability)

# Combine results
results <- bind_rows(results_all, results_sex) 

# Sort
results <- results %>% 
  mutate(Sample = factor(Sample,levels=c("Complete","Sisters","Brothers")))

results <- results %>% select(-Upper, -Lower)


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
results_table <- wide_df %>% 
  arrange(Dataset, Sample, Natural_Talents) %>%
  select(Dataset, Sample, N, Natural_Talents, 
         Estimate_Sibcorr, SE_Sibcorr, 
         Estimate_IOLIB,   SE_IOLIB,
         Estimate_IORAD,   SE_IORAD,
         Estimate_diff, pval) %>%
  mutate_if(is.numeric, round, 2)



################################################################################
############### VARIANCE COMPONENT COMPARISON: FULL vs. NO INTERACTIONS #######
################################################################################

# Reads the "Full results" sheet from both specs for each sample and stacks
# them into a single table so the two specifications can be compared directly.

files <- list(
  full = c(
    Brothers = "results/by_outcome/full_results_education_PGI_Brothers.xlsx",
    Sisters  = "results/by_outcome/full_results_education_PGI_Sisters.xlsx",
    Pooled   = "results/by_outcome/full_results_education_PGI.xlsx"
  ),
  noint = c(
    Brothers = "results/by_outcome/full_results_education_PGI_Brothers_nointeractions.xlsx",
    Sisters  = "results/by_outcome/full_results_education_PGI_Sisters_nointeractions.xlsx",
    Pooled   = "results/by_outcome/full_results_education_PGI_nointeractions.xlsx"
  )
)

read_components <- function(path) {
  d  <- read.xlsx(path, sheet = "Full results")
  r1 <- d[d$Model == "NULL MODEL",     ]
  r3 <- d[d$Model == "COMPLETE MODEL", ]
  data.frame(
    Sibcorr      = r1$Sibcorr,
    delta_within = r1$v - r1$w,
    w            = r1$w,
    condcorr     = r1$condcorr,
    IOLIB        = r1$IOLIB,
    IORAD        = r3$IORAD
  )
}

comp <- lapply(names(files), function(spec) {
  lapply(names(files[[spec]]), function(samp) {
    row <- read_components(files[[spec]][[samp]])
    data.frame(Sample = samp, Spec = spec, row)
  }) |> do.call(what = rbind)
}) |> do.call(what = rbind)

comp <- comp |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  arrange(Sample, Spec)

print(comp, row.names = FALSE)


################################################################################
############### PLOT: FULL vs. NO-INTERACTION RESULTS BY SAMPLE ################
################################################################################

make_sample_plot <- function(spec, title) {
  plot_data <- lapply(names(files[[spec]]), function(samp) {
    d <- read.xlsx(files[[spec]][[samp]], sheet = "For plotting")
    d$Sample <- samp
    d
  }) |> do.call(what = rbind)

  plot_data <- plot_data |>
    filter(Index != "diff") |>
    mutate(
      Index    = factor(Index, levels = INDICES),
      Sample   = factor(Sample, levels = c("Pooled", "Sisters", "Brothers")),
      Estimate = round(Estimate, 2),
      Upper    = round(Upper,    2)
    )

  ggplot(plot_data, aes(x = Sample, y = Estimate, fill = Index)) +
    geom_col(position = "dodge", width = 0.7) +
    geom_errorbar(aes(ymin = Lower, ymax = Upper),
                  position = position_dodge(0.7),
                  width = 0.25, alpha = 0.9) +
    geom_text(aes(label = Estimate, y = Upper),
              position = position_dodge(width = 0.7),
              vjust = -1, size = 4) +
    labs(title = title, y = "Inequality of Opportunity in Education\n") +
    guides(fill = guide_legend(nrow = 1, byrow = FALSE, title = NULL,
                               keywidth = 1.2, keyheight = 1.2,
                               default.unit = "cm")) +
    theme_bw(base_size = 18) +
    theme(
      axis.title.x       = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title         = element_text(size = 16, hjust = 0.5)
    ) +
    scale_y_continuous(limits = c(0, 0.6), expand = expansion(mult = c(0, 0))) +
    scale_fill_manual(labels = INDICES.labs, values = c("#F0B70F", "#7ABA3A", "#E83B3F"))
}

p_full  <- make_sample_plot("full",  "With interactions")
p_noint <- make_sample_plot("noint", "Main effects only")

p_full + p_noint + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("plots/by_sample_full_vs_noint.png", width = 14, height = 8)










