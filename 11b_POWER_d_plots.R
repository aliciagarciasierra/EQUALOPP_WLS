##############################################################
######## POWER ANALYSIS — delta_within: combined plot ########
##############################################################

setwd("~/Library/CloudStorage/OneDrive-UniversitédeLausanne/UNIL/projects/GenesSkills/EQUALOPP_WLS")
rm(list = ls())
source("00_MASTER.R")


###### ------ CONFIG ----- #######

dataset      <- "MOBA"            # "WLS" or "MOBA"
concentrated <- T
n_sim        <- 500
samples      <- c("Pooled", "Brothers", "Sisters")

sample_labels <- c(Pooled = "Pooled", Brothers = "Men", Sisters = "Women")


######################################################
##########  DATASET-SPECIFIC SETUP  #################
######################################################

if (dataset == "WLS") {

  xlsx_lookup <- c(
    Brothers = "results/nointeractions/full_results_education_PGI_Brothers.xlsx",
    Sisters  = "results/nointeractions/full_results_education_PGI_Sisters.xlsx",
    Pooled   = "results/nointeractions/full_results_education_PGI.xlsx"
  )

  get_params <- function(sample_tag) {
    results <- read_xlsx(xlsx_lookup[sample_tag], sheet = "Full results")
    null    <- results[results$Model == "NULL MODEL",        ]
    cond    <- results[results$Model == "CONDITIONAL MODEL", ]
    list(icc          = null$emptyfam / null$totalvar,
         delta_within = (null$emptyind - cond$condind) / null$totalvar)
  }

} else if (dataset == "MOBA") {

  source("00a_MOBA_RESULTS.R")

  vc_lookup <- list(
    Pooled   = moba_parents_full,
    Brothers = moba_parents_brothers_full,
    Sisters  = moba_parents_sisters_full
  )

  get_params <- function(sample_tag) {
    vc_df <- vc_lookup[[sample_tag]]
    null  <- vc_df[vc_df$Model == "NULL MODEL",        ]
    cond  <- vc_df[vc_df$Model == "CONDITIONAL MODEL", ]
    list(icc          = null$emptyfam / null$totalvar,
         delta_within = (null$emptyind - cond$condind) / null$totalvar)
  }

} else {
  stop(glue("Unknown dataset: '{dataset}'. Must be 'WLS' or 'MOBA'."))
}


######################################################
##########  LOAD CURVES & COMPUTE ANNOTATIONS  ######
######################################################

conc_tag <- ifelse(concentrated, "concentrated", "equal")

# Load power curves for all samples
curves_df <- do.call(rbind, lapply(samples, function(s) {
  tag <- glue("{dataset}_{conc_tag}_{s}_{n_sim}")
  pc  <- readRDS(glue("PowerAnalysis/results/power_d_{tag}.rds"))
  pc  <- pc[order(pc$delta), ]
  pc$sample <- s
  pc
}))

curves_df$sample <- factor(curves_df$sample, levels = samples, labels = sample_labels[samples])

# Per-sample annotation: observed delta + detectable delta
ann_df <- do.call(rbind, lapply(samples, function(s) {
  params         <- get_params(s)
  delta_observed <- params$delta_within

  pc <- curves_df[curves_df$sample == sample_labels[s], ]

  idx_above <- which(!is.na(pc$power) & pc$power >= 0.80)
  det_delta <- if (length(idx_above) == 0) NA_real_ else {
    fa <- min(idx_above)
    if (fa == 1) pc$delta[fa] else {
      d_lo <- pc$delta[fa - 1]; d_hi <- pc$delta[fa]
      p_lo <- pc$power[fa - 1]; p_hi <- pc$power[fa]
      d_lo + (0.80 - p_lo) / (p_hi - p_lo) * (d_hi - d_lo)
    }
  }

  x_max <- max(pc$delta, na.rm = TRUE)

  data.frame(
    sample         = sample_labels[s],
    delta_observed = delta_observed,
    label_observed = glue("observed = {round(delta_observed, 3)}"),
    det_delta      = det_delta,
    label_det      = if (!is.na(det_delta)) glue("detectable = {round(det_delta, 3)}") else NA_character_,
    x_max          = x_max,
    stringsAsFactors = FALSE
  )
}))

ann_df$sample <- factor(ann_df$sample, levels = sample_labels[samples])


######################################################
##########  PLOT  ####################################
######################################################

p <- ggplot(curves_df, aes(x = delta, y = power)) +
  geom_line(linewidth = 1.2, color = "steelblue") +
  geom_point(size = 2.5, color = "steelblue") +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "grey40") +
  geom_text(data = ann_df,
            aes(x = x_max * 0.98, y = 0.85, label = "80% power"),
            hjust = 1, size = 3, color = "grey40") +
  # Observed delta
  geom_vline(data = ann_df,
             aes(xintercept = delta_observed), linetype = "dotted", color = "red") +
  geom_text(data = ann_df,
            aes(x = delta_observed + (x_max * 0.02), y = 0.05, label = label_observed),
            hjust = 0, size = 3, color = "red") +
  # Detectable delta
  geom_vline(data = ann_df[!is.na(ann_df$det_delta), ],
             aes(xintercept = det_delta), linetype = "dotted", color = "steelblue") +
  geom_text(data = ann_df[!is.na(ann_df$det_delta), ],
            aes(x = det_delta + (x_max * 0.02), y = 0.15, label = label_det),
            hjust = 0, size = 3, color = "steelblue") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(
    x = "Variance explained by PGIs within family",
    y = "Power (one-sided, alpha = 0.05)"
  ) +
  facet_wrap(~sample, ncol = 1) +
  theme_bw(base_size = 11) +
  theme(
    strip.background = element_rect(fill="grey90")
  )

print(p)


out_tag <- glue("{dataset}_{conc_tag}_{n_sim}")
ggsave(glue("PowerAnalysis/plots/power_d_{out_tag}_combined.pdf"), p, width = 7, height = 10)
cat(glue("Plot saved: PowerAnalysis/plots/power_d_{out_tag}_combined.pdf\n"))
