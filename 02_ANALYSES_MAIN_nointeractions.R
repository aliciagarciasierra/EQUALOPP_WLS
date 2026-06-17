#####################################################################
###################### MAIN ANALYSES — NO INTERACTIONS ##############
#####################################################################

# Replicates 02_ANALYSES_MAIN.R but fits main effects only (no pairwise
# interactions in m1 or m2). Used to check whether the interaction terms
# materially change the variance component estimates.

rm(list=ls())

set.seed(123)
source("00_MASTER.R")


########################## SETUP ####################################

outcome         <- "education"
natural_talents <- "PGI"
n_boot          <- 500
result_flag     <- "nointeractions"
samples         <- c("Brothers", "Sisters", "Pooled")


########################## READ DATA ####################################

siblings <- readRDS(paste0("data/siblings_", outcome, ".rds"))
siblings <- siblings %>% mutate_if(is.numeric, scale)


########################## OVERRIDE MODEL FUNCTIONS ####################

# Shadow compute_indexes and est_fun from 00_MASTER.R with main-effects-only
# versions: replace (vars)^2 with vars (no pairwise interactions).

compute_indexes <- function(outcome, data, natural_talents, pgis = PGIs) {

  m0_vars   <- "1"
  famID     <- "+ (1 | familyID)"
  # Exclude constant variables — model.matrix fails on zero-variance predictors
  active    <- names(data)[sapply(data, function(x) length(unique(x)) > 1)]
  ascr_vars <- paste(ASCRIBED[ASCRIBED %in% active], collapse = " + ")
  pgi_vars  <- paste(pgis[pgis %in% active], collapse = " + ")
  obs_vars  <- paste(OBSERVED, collapse = " + ")

  if (natural_talents == "PGI") {
    m1_vars <- pgi_vars
    m2_vars <- paste(pgi_vars, "+", ascr_vars)
  } else if (natural_talents == "observed") {
    m1_vars <- obs_vars
    m2_vars <- paste(obs_vars, "+", ascr_vars)
  } else stop("select a valid definition of natural talents")

  m0 <- lmer(as.formula(paste(outcome, "~", m0_vars, famID)), data = data)
  m1 <- lmer(as.formula(paste(outcome, "~", m1_vars, famID)), data = data)
  m2 <- lmer(as.formula(paste(outcome, "~", m2_vars, famID)), data = data)

  vcov_m0 <- as.data.frame(VarCorr(m0))
  vcov_m1 <- as.data.frame(VarCorr(m1))
  vcov_m2 <- as.data.frame(VarCorr(m2))

  emptyind    <- vcov_m0[vcov_m0$grp == "Residual", "vcov"]
  emptyfam    <- vcov_m0[vcov_m0$grp == "familyID", "vcov"]
  totalvar    <- emptyfam + emptyind
  condind     <- vcov_m1[vcov_m1$grp == "Residual", "vcov"]
  condfam     <- vcov_m1[vcov_m1$grp == "familyID", "vcov"]
  completeind <- vcov_m2[vcov_m2$grp == "Residual", "vcov"]
  completefam <- vcov_m2[vcov_m2$grp == "familyID", "vcov"]

  Sibcorr  <- emptyfam / totalvar
  condcorr <- condfam  / totalvar
  w        <- (condind  - completeind) / totalvar
  v        <- (emptyind - completeind) / totalvar
  IOLIB    <- w + condcorr
  IORAD    <- v + Sibcorr

  data.frame(
    Outcome = outcome,
    Model   = c("NULL MODEL", "CONDITIONAL MODEL", "COMPLETE MODEL"),
    emptyind    = c(emptyind,    NA, NA),
    emptyfam    = c(emptyfam,    NA, NA),
    totalvar    = c(totalvar,    NA, NA),
    condind     = c(NA, condind,     NA),
    condfam     = c(NA, condfam,     NA),
    completeind = c(NA, NA, completeind),
    completefam = c(NA, NA, completefam),
    Sibcorr  = c(Sibcorr,  NA, NA),
    condcorr = c(condcorr, NA, NA),
    w        = c(w,        NA, NA),
    v        = c(v,        NA, NA),
    IOLIB    = c(IOLIB,    NA, NA),
    IORAD    = c(NA,       NA, IORAD)
  )
}

est_fun <- function(family_ids, indices, outcome, natural_talents, pgis, full_data) {

  sampled_families <- family_ids[indices]
  data_sample      <- full_data[full_data$familyID %in% sampled_families, ]

  famID     <- "+ (1 | familyID)"
  active    <- names(data_sample)[sapply(data_sample, function(x) length(unique(x)) > 1)]
  ascr_vars <- paste(ASCRIBED[ASCRIBED %in% active], collapse = " + ")
  pgi_vars  <- paste(pgis[pgis %in% active], collapse = " + ")

  m0_vars <- "1"
  m1_vars <- pgi_vars
  m2_vars <- paste(pgi_vars, "+", ascr_vars)

  m0 <- lmer(as.formula(paste(outcome, "~", m0_vars, famID)), data = data_sample)
  m1 <- lmer(as.formula(paste(outcome, "~", m1_vars, famID)), data = data_sample)
  m2 <- lmer(as.formula(paste(outcome, "~", m2_vars, famID)), data = data_sample)

  vcov_m0 <- as.data.frame(VarCorr(m0))
  vcov_m1 <- as.data.frame(VarCorr(m1))
  vcov_m2 <- as.data.frame(VarCorr(m2))

  emptyind    <- vcov_m0[vcov_m0$grp == "Residual", "vcov"]
  emptyfam    <- vcov_m0[vcov_m0$grp == "familyID", "vcov"]
  totalvar    <- emptyfam + emptyind
  condind     <- vcov_m1[vcov_m1$grp == "Residual", "vcov"]
  condfam     <- vcov_m1[vcov_m1$grp == "familyID", "vcov"]
  completeind <- vcov_m2[vcov_m2$grp == "Residual", "vcov"]
  completefam <- vcov_m2[vcov_m2$grp == "familyID", "vcov"]

  Sibcorr  <- emptyfam / totalvar
  condcorr <- condfam  / totalvar
  w        <- (condind  - completeind) / totalvar
  v        <- (emptyind - completeind) / totalvar
  IOLIB    <- w + condcorr
  IORAD    <- v + Sibcorr

  return(c(Sibcorr, IOLIB, IORAD))
}


######################## LOOP OVER SAMPLES ##############################

for (sample_tag in samples) {

  cat(glue("\n===== {sample_tag} =====\n"))

  # Filter siblings by sample
  sample_data <- switch(sample_tag,
    Brothers = siblings |> group_by(familyID) |> filter(all(sex == 0)) |> ungroup(),
    Sisters  = siblings |> group_by(familyID) |> filter(all(sex == 1)) |> ungroup(),
    Pooled   = siblings
  )

  # Output filename: Brothers/Sisters get a suffix; Pooled does not
  sample_suffix <- switch(sample_tag, Brothers = "_Brothers", Sisters = "_Sisters", Pooled = "")
  out_path <- paste0("results/by_outcome/full_results_", outcome, "_",
                     natural_talents, sample_suffix, "_", result_flag, ".xlsx")

  # Compute
  print("compute main results — no interactions")
  final_results <- compute_indexes(outcome, sample_data, natural_talents)

  print("compute bootstrapping — no interactions")
  ci_summary <- compute_indexes_bootstrap(sample_data, n_boot, outcome)

  # Save
  wb <- createWorkbook()
  addWorksheet(wb, "Full results")
  writeData(wb, "Full results", final_results)
  addWorksheet(wb, "For plotting")
  writeData(wb, "For plotting", ci_summary)
  saveWorkbook(wb, out_path, overwrite = TRUE)
  cat("Saved:", out_path, "\n")
}




################### FIGURE: NO-INTERACTION RESULTS BY SAMPLE ###################

plot_data <- lapply(samples, function(samp) {
  samp_suffix <- switch(samp, Brothers = "_Brothers", Sisters = "_Sisters", Pooled = "")
  path <- paste0("results/by_outcome/full_results_", outcome, "_",
                 natural_talents, samp_suffix, "_", result_flag, ".xlsx")
  d <- read.xlsx(path, sheet = "For plotting")
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
            vjust = -1, size = 5) +
  labs(y = "Inequality of Opportunity in Education\n") +
  guides(fill = guide_legend(nrow = 1, byrow = FALSE, title = NULL,
                             keywidth = 1.2, keyheight = 1.2,
                             default.unit = "cm")) +
  theme_bw(base_size = 20) +
  theme(
    axis.title.x       = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position    = "top",
    strip.background   = element_rect(fill = "white"),
    strip.text         = element_text(size = 16),
    legend.text        = element_text(size = 16),
    legend.spacing.y   = unit(0.1, "pt")
  ) +
  scale_y_continuous(limits = c(0, 0.6), expand = expansion(mult = c(0, 0))) +
  scale_fill_manual(labels = INDICES.labs, values = c("#F0B70F", "#7ABA3A", "#E83B3F"))

ggsave(paste0("plots/by_sample_", result_flag, ".pdf"), width = 10, height = 8)


