
########################## COMBINE WITH MOBA ####################################

setwd("~/Library/CloudStorage/OneDrive-UniversitédeLausanne/UNIL/projects/GenesSkills/EQUALOPP_WLS")
source("00_MASTER.R")


setwd("~/Library/CloudStorage/OneDrive-UniversitédeLausanne/UNIL/projects/GenesSkills/EQUALOPP_WLS")
rm(list = ls())
set.seed(123)
source("00_MASTER.R")
source("00a_MOBA_RESULTS.R")

outcome <- "education"
natural_talents <- "PGI"


# ===================== MAIN RESULTS =====================

# === WLS
ci_summaryWLS <- readWorkbook(glue("results/by_outcome/full_results_{outcome}_{natural_talents}.xlsx"),
                              sheet = "For plotting")


# Save one combined file per MoBa sample:

# - parents
all_parents <- bind_rows(mutate(moba_parents,  Dataset="MoBa"), mutate(ci_summaryWLS, Dataset="WLS"))
saveRDS(all_parents, paste0("results/results_",outcome,"_",natural_talents,"_parents.rds"))

# - children
all_children <- bind_rows(mutate(moba_children,  Dataset="MoBa"), mutate(ci_summaryWLS, Dataset="WLS"))
saveRDS(all_children, paste0("results/results_",outcome,"_",natural_talents,"_children.rds"))





# ===================== GENDER RESULTS =====================


# ==== WLS
ci_summaryWLS <- map_df(c("Brothers","Sisters"), function(sex_lab) {
  data <- readWorkbook(glue("results/by_outcome/full_results_{outcome}_{natural_talents}_{sex_lab}.xlsx"), sheet = "For plotting")
  data %>% mutate(ability = natural_talents, Sample = sex_lab)
})


# Save one combined file per MoBa sample:

# - parents
all_parents <- bind_rows(mutate(moba_parents_gender,  Dataset="MoBa"), mutate(ci_summaryWLS, Dataset="WLS"))
saveRDS(all_parents, paste0("results/results_",outcome,"_",natural_talents,"_gender_parents.rds"))

# - children
all_children <- bind_rows(mutate(moba_children_gender,  Dataset="MoBa"), mutate(ci_summaryWLS, Dataset="WLS"))
saveRDS(all_children, paste0("results/results_",outcome,"_",natural_talents,"_gender_children.rds"))
