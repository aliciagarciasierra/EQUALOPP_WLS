
#####################################################################
################## REPLICATION: HERD ET AL. 2019 ###################
#####################################################################

# Replicates Figure 4 from:
# Herd, P., Freese, J., Sicinski, K., Domingue, B.W., Harris, K.M.,
# Wei, C., & Hauser, R.M. (2019). Genes, Gender Inequality, and
# Educational Attainment. American Sociological Review, 84(6), 1069-1098.


rm(list = ls())
set.seed(123)
setwd("~/Library/CloudStorage/OneDrive-UniversitédeLausanne/UNIL/projects/GenesSkills/EQUALOPP_WLS")
source("00_MASTER.R")

library(lmtest)
library(sandwich)


## CONFIG ---------------------------------------------------------------

PGI_VARS <- c(
  "pgi_education" = "PGI Education",
  "pgi_cognitive" = "PGI Cognitive Ability"
)

EDU_WAVES <- c(
  "Age ~25" = "education_0",   # edyr64,      R02 (graduates only)
  "Age ~36" = "education_1",   # z_edeqyr,    R03
  "Age ~54" = "education_2",   # z_rb004red,  R04
  "Age ~65" = "education_3"    # z_gb103red,  R05
)

SEX_LABS <- c("0" = "Men", "1" = "Women")
SEX_COLS <- c("Men" = "#4472C4", "Women" = "#E83B3F")

## ---------------------------------------------------------------------


## READ AND PREPARE DATA -----------------------------------------------

data_raw <- readRDS("data/data.rds")

# IDs and demographics
data_raw <- data_raw %>%
  mutate(
    pgiID      = paste(idpub, rtype, sep = "_"),
    sex        = case_when(z_sexrsp == 1 ~ 0L, z_sexrsp == 2 ~ 1L, TRUE ~ NA_integer_),
    birth_year = ifelse(z_brdxdy < 0, NA, z_brdxdy)
  )

# Wave-specific education (negative values = missing in WLS)
data_raw <- data_raw %>%
  rename(
    education_0 = edyr64,
    education_1 = z_edeqyr,
    education_2 = z_rb004red,
    education_3 = z_gb103red
  ) %>%
  mutate(across(c(education_0, education_1, education_2, education_3), ~ ifelse(. < 0, NA, .)))

# PGI + PCs: read repository file, strip _PGI_shuffled suffix
pgi_raw <- read_dta("data/PGIrepo_v1.1_idpub_shuffled.dta") %>%
  mutate(pgiID = paste(idpub, rtype, sep = "_")) %>%
  select(pgiID, pgi_education = pgi_easingle, pgi_cognitive = pgi_cpsingle, matches("^pc[0-9]+_PGI_shuffled$"))

names(pgi_raw) <- str_replace(names(pgi_raw), "_PGI_shuffled", "")

# Merge and restrict to individuals with valid PGI and sex.
# all.y = TRUE keeps only those present in the Repository file, which already
# restricts to European-ancestry individuals (Becker et al. 2021, Methods).
df <- merge(data_raw, pgi_raw, by = "pgiID", all.x = FALSE, all.y = TRUE) %>%
  filter(!is.na(pgi_education), !is.na(sex)) %>%
  mutate(
    pgi_education = as.numeric(scale(pgi_education)),
    pgi_cognitive = as.numeric(scale(pgi_cognitive)),
    sex_lab       = factor(SEX_LABS[as.character(sex)], levels = c("Men", "Women"))
  )

cat("Analytic sample N =", nrow(df), "\n")
cat("Sex distribution:\n"); print(table(df$sex_lab))
cat("Education N by wave:\n")
print(sapply(unname(EDU_WAVES), function(v) sum(!is.na(df[[v]]))))


## MODELS ---------------------------------------------------------------
# OLS: education_wave ~ pgi_education + birth_year + pc1:pc20, stratified by sex
# Robust standard errors (HC3), following Herd et al. 2019

run_ols <- function(wave_var, sex_label, pgi, d) {
  sub <- d %>% filter(sex_lab == sex_label, !is.na(.data[[wave_var]]))
  fml <- as.formula(paste(wave_var, "~", pgi, "+ birth_year +", paste(PC, collapse = " + ")))
  fit <- lm(fml, data = sub)
  ct  <- coeftest(fit, vcov = vcovHC(fit, type = "HC3"))
  data.frame(
    wave  = wave_var,
    sex   = sex_label,
    pgi   = pgi,
    coef  = ct[pgi, "Estimate"],
    se    = ct[pgi, "Std. Error"],
    ci_lo = ct[pgi, "Estimate"] - 1.96 * ct[pgi, "Std. Error"],
    ci_hi = ct[pgi, "Estimate"] + 1.96 * ct[pgi, "Std. Error"],
    n     = nrow(sub)
  )
}

results <- map_df(
  cross2(names(EDU_WAVES), cross2(c("Men", "Women"), names(PGI_VARS))),
  function(x) {
    run_ols(EDU_WAVES[[x[[1]]]], x[[2]][[1]], x[[2]][[2]], df) %>%
      mutate(wave_label = x[[1]])
  }
)

results <- results %>%
  mutate(
    wave_label = factor(wave_label, levels = names(EDU_WAVES)),
    sex        = factor(sex, levels = c("Men", "Women")),
    pgi_label  = factor(PGI_VARS[pgi], levels = PGI_VARS)
  )

print(results %>% select(pgi_label, wave_label, sex, coef, ci_lo, ci_hi, n))


## SAVE -----------------------------------------------------------------
saveRDS(results, "results/herd_replication_fig4.rds")


## PLOT -----------------------------------------------------------------
ggplot(results, aes(x = wave_label, y = coef, fill = sex)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(
    aes(ymin = ci_lo, ymax = ci_hi),
    position = position_dodge(width = 0.7),
    width = 0.2, linewidth = 0.7
  ) +
  facet_wrap(~ pgi_label, ncol = 1) +
  labs(
    x       = NULL,
    y       = "Marginal effect",
    fill    = NULL,
  ) +
  scale_fill_manual(values = SEX_COLS) +
  theme_bw(base_size = 16) +
  theme(
    legend.position    = "top",
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background   = element_rect(fill = "white"),
    strip.text         = element_text(size = 14),
    axis.text.x        = element_text(size = 12)
  )

ggsave("plots/herd_replication_fig4.pdf", width = 7, height = 7)
