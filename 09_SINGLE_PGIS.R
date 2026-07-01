#####################################################################
###################### ONE PGI ANALYSES #############################
#####################################################################


rm(list=ls()) 

set.seed(123)
source("00_MASTER.R")


########################## SETUP ####################################

# Compute for desired outcomes and natural talents, even only one
outcome         <- "education"
natural_talents <- "PGI"
dataset <- "WLS"

# If dataset = WLS
n_boot  <- 500
compute <- T



########################## READ DATA ####################################

# ------- log
print(outcome)
print(natural_talents)

# ------- read data
siblings <- readRDS(paste0("data/siblings_",outcome,".rds"))

# ------- re-scale
siblings <- siblings %>%
  mutate_if(is.numeric, scale)



if (compute) {
  
  
  ## == For each PGI:
  mclapply(PGIs, function(pgi) {
      
    
    ##################### CONFIDENCE INTERVALS ############################
    
    # ------- Run bootstrapping
    print("compute bootstrapping")
    ci_summary = compute_indexes_bootstrap(siblings, n_boot, outcome, pgi)
    
    ## Add PGI column
    ci_summary <- ci_summary %>% mutate(PGI = pgi)
    
    
    ##################### SAVE RESULTS ############################
    
    # Create a new Excel workbook
    wb <- createWorkbook()
    
    # Add confidence intervals sheet
    addWorksheet(wb, "For plotting")
    writeData(wb, "For plotting", ci_summary)
    
    # Save the workbook to an Excel file
    saveWorkbook(wb, paste0("results/by_outcome/full_results_",outcome,"_",natural_talents,"_",pgi,".xlsx"), overwrite = T)
  
    
  }, mc.cores = 3)


}


# ================= PLOT ================= #


# read

if (dataset == "WLS") {
  data <- map_df(PGIs, function(pgi) {
    readWorkbook(paste0("results/by_outcome/full_results_",outcome,"_",natural_talents,"_",pgi,".xlsx"), sheet = "For plotting")
  })
} else if (dataset == "MOBA") {
  source("00a_MOBA_RESULTS.R")
  data <- moba_single_pgis
}

# Take out difference
data <- data %>% filter(Index != "diff")

# Custom order
data$Index   <- factor(data$Index,   levels = INDICES)
data$PGI  <- factor(data$PGI, levels = PGIs, labels = PGIs.labs)
#data$Dataset <- factor(data$Dataset, levels = c("WLS","MoBa"))

# Round for plotting
data <- data %>% mutate_if(is.numeric, round, 2)

# Create the plot for the current outcome
ggplot(data, aes(x = Outcome, y = Estimate, fill = Index)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper),
                position = position_dodge(0.7),
                width = 0.25, alpha = 0.9) +
  labs(x = " ", y = "Inequality of Opportunity in Education\n") +
  geom_text(aes(label = Estimate, y=Upper),
            position  = position_dodge(width = 0.7),
            vjust = -1, size = 5) +
  scale_x_discrete(labels = OUTCOMES.labs) +
  facet_wrap(~ PGI, nrow=2) +
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
    strip.text = element_text(size=15),
    legend.text = element_text(size=20),
    legend.spacing.y = unit(0.1, "pt")
  ) +
  scale_y_continuous(limits = c(0, 0.7), expand = expansion(mult = c(0, 0))) +
  scale_fill_manual(labels = INDICES.labs, values=c("#F0B70F", "#7ABA3A", "#E83B3F"))

# Save the plot
ggsave(glue("plots/by_PGI_{dataset}.pdf"), width = 12, height = 11)





















