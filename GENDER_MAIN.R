#####################################################################
###################### IMPUTED DATA ANALYSES ########################
#####################################################################


rm(list=ls()) 
set.seed(123)
source("00_MASTER.R")


########################## SETUP ####################################

# Compute for desired outcome and natural talents
outcome         <- "education"
natural_talents <- "PGI"

# Bootstrapping:
n_boot <- 1000



########################## MODELS ESTIMATION ####################################



# Check number of observations by gender:

siblings = readRDS(paste0("data/siblings_",outcome,".rds"))

lapply(c(0,1), function(which_sex) {
  # Set label
  sex_lab <- ifelse(which_sex==0,"Brothers","Sisters")
  # Filter by gender
  siblings <- siblings %>%
    group_by(familyID) %>%
    filter(all(sex == which_sex)) %>%
    ungroup() %>% 
    select(-sex) 
  # Print sample size
  paste0(sex_lab, " N:", nrow(siblings))
  
})




######  Run for PGIs


###### Run for each gender:
mclapply(c(0,1), function(which_sex) {
  
  # ------- set label
  sex_lab <- ifelse(which_sex==0,"Brothers","Sisters")
  
  # ------- check
  print(paste0("which group: ", sex_lab))
  print(paste0("which talent: ",natural_talents))
  print(paste0("n bootstraps: ",n_boot))
  
  
  ########################## MODELS ESTIMATION ####################################
  
  # ------- read data
  siblings <- readRDS(paste0("data/siblings_",outcome,".rds"))
  
  # ------- only keep one sex
  siblings <- siblings %>%
      group_by(familyID) %>%
      filter(all(sex == which_sex)) %>%
      ungroup() %>% 
      select(-sex)
  
  
  # ------- rescale
  siblings <- siblings %>%
    mutate_if(is.numeric, scale)
  
  
  
  ######################## FULL RESULTS ##############################
  
  # ------- Compute indices for all outcomes
  print("compute main results")
  final_results = compute_indexes(outcome, siblings, natural_talents)
  
  
  
  ##################### CONFIDENCE INTERVALS ############################

  # ------- Run bootstrapping
  print("compute bootstrapping")
  boot_results = compute_indexes_bootstrap(siblings, n_boot, outcome)
  
  
  
  ##################### SAVE RESULTS ############################
  
  # Create a new Excel workbook
  wb <- createWorkbook()
  
  # Add results sheet
  addWorksheet(wb, "Full results")
  writeData(wb, "Full results", final_results)
  
  # Add confidence intervals sheet
  addWorksheet(wb, "For plotting")
  writeData(wb, "For plotting", boot_results)
  
  # Save the workbook to an Excel file
  saveWorkbook(wb, paste0("results/by_outcome/full_results_",outcome,"_",natural_talents,"_",sex_lab,".xlsx"), overwrite = TRUE)
  
  
  
}, mc.cores=4) # end of sex loop

cat("Finished computation")








######################### GRAPH BY DATASET AND SEX ###############################

# ==== MoBa

# Children
ci_summaryMoBa <- read.delim(text="Index	Outcome	Estimate	Lower	Upper	pval	N	ability	Sample
Sibcorr	education	0.40608267	0.36471313	0.45414984	0.000000e+00	1760	PGI	Brothers
IOLIB	education	0.34051696	0.30390035	0.40380466	0.000000e+00	1760	PGI	Brothers
IORAD	education	0.41903337	0.37568150	0.48054908	0.000000e+00	1760	PGI	Brothers
diff	education	0.07851641	0.05280824	0.10419221	7.687090e-09	1760	PGI	Brothers
Sibcorr	education	0.34817288	0.28557667	0.39070146	0.000000e+00	1601	PGI	Sisters
IOLIB	education	0.30974408	0.25652222	0.35671824	0.000000e+00	1601	PGI	Sisters
IORAD	education	0.37402385	0.32345435	0.42815731	0.000000e+00	1601	PGI	Sisters
diff	education	0.06427977	0.03985909	0.09054253	5.165231e-07	1601	PGI	Sisters", sep = "\t")

# Parents
ci_summaryMoBa <- read.delim(text="Index	Outcome	Estimate	Lower	Upper	pval	N	ability	Sample
Sibcorr	education	0.43215919	0.40378979	0.46675070	0	3246	PGI	Brothers
IOLIB	education	0.34634678	0.30888033	0.37862215	0	3246	PGI	Brothers
IORAD	education	0.44314968	0.41325078	0.47729431	0	3246	PGI	Brothers
diff	education	0.09680290	0.07919004	0.11656514	0	3246	PGI	Brothers
Sibcorr	education	0.36735349	0.34270131	0.38682179	0	7034	PGI	Sisters
IOLIB	education	0.31447880	0.29176119	0.33403230	0	7034	PGI	Sisters
IORAD	education	0.39131654	0.36664315	0.41233420	0	7034	PGI	Sisters
diff	education	0.07683774	0.06626177	0.08810007	0	7034	PGI	Sisters", sep = "\t")


# ==== WLS 
ci_summaryWLS <- map_df(c("Brothers","Sisters"), function(sex_lab) {
  data <- readWorkbook(paste0("results/by_outcome/full_results_",outcome,"_",natural_talents,"_",sex_lab,".xlsx"), sheet = "For plotting") 
  data %>% mutate(ability = natural_talents, Sample = sex_lab)
})

# Combine 
data <- bind_rows(mutate(ci_summaryMoBa,Dataset="MoBa"), 
                  mutate(ci_summaryWLS, Dataset="WLS"))

# Save all
saveRDS(data, paste0("results/results_",outcome,"_",natural_talents,"_gender.rds"))






# ===== Organize =====

# Take out difference
data <- data %>% filter(Index != "diff")

# Custom order 
data$Index   <- factor(data$Index,   levels = INDICES)
data$Sample  <- factor(data$Sample,  
                       levels = c("Sisters","Brothers"), labels = c("Women","Men"))
data$Dataset <- factor(data$Dataset, levels = c("WLS","MoBa"))

# Round for plotting
data <- data %>% mutate_if(is.numeric, round, 2)



# ===== Plot =====

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
  facet_grid(Dataset ~ Sample) +
  guides(fill = guide_legend(nrow = 1, byrow = F, title = NULL,
                             keywidth=1.2, keyheight=1.2,
                             default.unit="cm")) +
  theme_bw(base_size = 25) + 
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
    legend.text = element_text(size=20),
    legend.spacing.y = unit(0.1, "pt")
  ) +
  scale_y_continuous(limits = c(0, 0.7), expand = expansion(mult = c(0, 0))) +
  scale_fill_manual(labels = INDICES.labs, values=c("#F0B70F", "#7ABA3A", "#E83B3F")) 


# Save the plot
ggsave(paste0("plots/by_gender.png"), width = 10, height = 11, dpi = 300)


