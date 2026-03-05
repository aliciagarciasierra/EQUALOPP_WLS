
source("00_MASTER.R")

outcome         <- "education"
natural_talents <- "PGI"

########################## COMBINE WITH MOBA ####################################


########## MAIN RESULTS ##########

# === MoBa (both samples)
moba_main <- list(
  parents = read.delim(text="Index	Outcome	Estimate	SE	Lower	Upper	pval	N
Sibcorr	education	0.37420625	0.007972544	0.36016743	0.3898473	0	19514
IOLIB	education	0.32063291	0.007599699	0.30463092	0.3349338	0	19514
IORAD	education	0.39904685	0.007596536	0.38562276	0.4130919	0	19514
diff	education	0.07841393	0.003532571	0.07238116	0.0847985	0	19514", sep = "\t"),
  children = read.delim(text="Index	Outcome	Estimate	Lower	Upper	pval	N
Sibcorr	education	0.39806344	0.37070421	0.41979626	0	6740
IOLIB	education	0.34577913	0.32002460	0.36793881	0	6740
IORAD	education	0.43095272	0.40635397	0.45467043	0	6740
diff	education	0.08517359	0.07476835	0.09619981	0	6740", sep = "\t")
)

# === WLS
ci_summaryWLS <- readWorkbook(paste0("results/by_outcome/full_results_",outcome,"_",natural_talents,".xlsx"),
                              sheet = "For plotting")


# Save one combined file per MoBa sample
for (moba_sample in names(moba_main)) {
  moba_data <- moba_main[[moba_sample]]
  if (!"SE" %in% names(moba_data)) moba_data$SE <- (moba_data$Upper - moba_data$Lower) / (2 * 1.96)
  data <- bind_rows(mutate(moba_data, Dataset="MoBa"),
                    mutate(ci_summaryWLS, Dataset="WLS"))
  saveRDS(data, paste0("results/results_",outcome,"_",natural_talents,"_",moba_sample,".rds"))
}






########## GENDER RESULTS ##########


# ==== MoBa (both samples)
moba_gender <- list(
  parents = read.delim(text="Index	Outcome	Estimate	SE	Lower	Upper	pval	N	ability	Sample
Sibcorr	education	0.43215919	0.017211916	0.40378979	0.46675070	0	3246	PGI	Brothers
IOLIB	education	0.34634678	0.018923673	0.30888033	0.37862215	0	3246	PGI	Brothers
IORAD	education	0.44314968	0.016528084	0.41325078	0.47729431	0	3246	PGI	Brothers
diff	education	0.09680290	0.009140143	0.07919004	0.11656514	0	3246	PGI	Brothers
Sibcorr	education	0.36735349	0.011906612	0.34270131	0.38682179	0	7034	PGI	Sisters
IOLIB	education	0.31447880	0.012086897	0.29176119	0.33403230	0	7034	PGI	Sisters
IORAD	education	0.39131654	0.012157220	0.36664315	0.41233420	0	7034	PGI	Sisters
diff	education	0.07683774	0.005581235	0.06626177	0.08810007	0	7034	PGI	Sisters", sep = "\t"),
  children = read.delim(text="Index	Outcome	Estimate	Lower	Upper	pval	N	ability	Sample
Sibcorr	education	0.40608267	0.36471313	0.45414984	0.000000e+00	1760	PGI	Brothers
IOLIB	education	0.34051696	0.30390035	0.40380466	0.000000e+00	1760	PGI	Brothers
IORAD	education	0.41903337	0.37568150	0.48054908	0.000000e+00	1760	PGI	Brothers
diff	education	0.07851641	0.05280824	0.10419221	7.687090e-09	1760	PGI	Brothers
Sibcorr	education	0.34817288	0.28557667	0.39070146	0.000000e+00	1601	PGI	Sisters
IOLIB	education	0.30974408	0.25652222	0.35671824	0.000000e+00	1601	PGI	Sisters
IORAD	education	0.37402385	0.32345435	0.42815731	0.000000e+00	1601	PGI	Sisters
diff	education	0.06427977	0.03985909	0.09054253	5.165231e-07	1601	PGI	Sisters", sep = "\t")
)

# ==== WLS
ci_summaryWLS <- map_df(c("Brothers","Sisters"), function(sex_lab) {
  data <- readWorkbook(paste0("results/by_outcome/full_results_",outcome,"_",natural_talents,"_",sex_lab,".xlsx"), sheet = "For plotting")
  data %>% mutate(ability = natural_talents, Sample = sex_lab)
})

# Save one combined file per MoBa sample
for (moba_sample in names(moba_gender)) {
  moba_data <- moba_gender[[moba_sample]]
  if (!"SE" %in% names(moba_data)) moba_data$SE <- (moba_data$Upper - moba_data$Lower) / (2 * 1.96)
  data <- bind_rows(mutate(moba_data, Dataset="MoBa"),
                    mutate(ci_summaryWLS, Dataset="WLS"))
  saveRDS(data, paste0("results/results_",outcome,"_",natural_talents,"_gender_",moba_sample,".rds"))
}


