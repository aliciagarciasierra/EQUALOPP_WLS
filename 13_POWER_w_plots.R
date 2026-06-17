##############################################################
######## POWER PLOTS — w (within-family ascribed IOP) ########
##############################################################

# Reads saved power curve .rds files from 13_POWER_w_simulation.R and
# produces one two-panel figure per sample: full interactions (left)
# vs. main effects only (right).

rm(list = ls())
setwd("~/Library/CloudStorage/OneDrive-UniversitédeLausanne/UNIL/projects/GenesSkills/EQUALOPP_WLS")
source("00_MASTER.R")
library(patchwork)


###### ------ CONFIG ----- #######

n_sim        <- 500
concentrated <- TRUE
samples      <- c("Brothers", "Sisters")

conc_tag     <- ifelse(concentrated, "concentrated", "equal")

n_fam_lookup <- c(Brothers = 473, Sisters = 612, Pooled = 2167)

xlsx_lookup <- list(
  full = c(
    Brothers = "results/by_outcome/full_results_education_PGI_Brothers.xlsx",
    Sisters  = "results/by_outcome/full_results_education_PGI_Sisters.xlsx"
  ),
  noint = c(
    Brothers = "results/by_outcome/full_results_education_PGI_Brothers_nointeractions.xlsx",
    Sisters  = "results/by_outcome/full_results_education_PGI_Sisters_nointeractions.xlsx"
  )
)


###### ------ HELPERS ----- #######

read_w_obs <- function(path) {
  d <- read.xlsx(path, sheet = "Full results")
  d[d$Model == "NULL MODEL", "w"]
}

interpolate_mde <- function(power_curve, threshold = 0.80) {
  idx <- which(!is.na(power_curve$power_w) & power_curve$power_w >= threshold)
  if (length(idx) == 0) return(NA_real_)
  fa   <- min(idx)
  if (fa == 1) return(power_curve$w_true[fa])
  w_lo <- power_curve$w_true[fa - 1]; w_hi <- power_curve$w_true[fa]
  p_lo <- power_curve$power_w[fa - 1]; p_hi <- power_curve$power_w[fa]
  w_lo + (0.80 - p_lo) / (p_hi - p_lo) * (w_hi - w_lo)
}


###### ------ PLOT FUNCTION ----- #######

make_w_plot <- function(sample_tag, int_tag, w_obs) {
  tag         <- glue("{conc_tag}_{sample_tag}_{int_tag}_{n_sim}")
  power_curve <- readRDS(glue("PowerAnalysis/results/power_w_{tag}.rds"))
  power_curve <- power_curve[order(power_curve$w_true), ]

  det_w       <- interpolate_mde(power_curve)
  spec_label  <- if (int_tag == "fullint") "With interactions" else "Main effects only"
  x_max       <- max(power_curve$w_true)

  p <- ggplot(power_curve, aes(x = w_true, y = power_w)) +
    geom_line(linewidth = 1.2, color = "steelblue") +
    geom_point(size = 2.5, color = "steelblue") +
    geom_hline(yintercept = 0.80, linetype = "dashed", color = "grey40") +
    annotate("text", x = x_max * 0.98, y = 0.83,
             label = "80% power", hjust = 1, size = 3, color = "grey40") +
    geom_vline(xintercept = w_obs, linetype = "dotted", color = "red") +
    annotate("text", x = w_obs + x_max * 0.01, y = 0.05,
             label = glue("observed = {round(w_obs, 3)}"),
             hjust = 0, size = 3, color = "red") +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    labs(
      title = spec_label,
      x     = "True w = (condind - completeind) / totalvar",
      y     = "Power (one-sided, alpha = 0.05)"
    ) +
    theme_bw(base_size = 11) +
    theme(plot.title = element_text(hjust = 0.5))

  if (!is.na(det_w))
    p <- p +
      geom_vline(xintercept = det_w, linetype = "dotted", color = "steelblue") +
      annotate("text", x = det_w + x_max * 0.01, y = 0.15,
               label = glue("detectable = {round(det_w, 3)}"),
               hjust = 0, size = 3, color = "steelblue")
  p
}


###### ------ LOOP OVER SAMPLES ----- #######

for (sample_tag in samples) {

  n_families <- n_fam_lookup[sample_tag]
  w_full     <- read_w_obs(xlsx_lookup$full[sample_tag])
  w_noint    <- read_w_obs(xlsx_lookup$noint[sample_tag])

  p_full  <- make_w_plot(sample_tag, "fullint", w_full)
  p_noint <- make_w_plot(sample_tag, "noint",   w_noint)

  fig <- p_full / p_noint +
    plot_annotation(
      title = glue("Power to detect w - {sample_tag} (N families = {n_families})"),
      theme = theme(plot.title = element_text(size = 13, hjust = 0.5))
    )

  out_path <- glue("PowerAnalysis/plots/power_w_{conc_tag}_{sample_tag}_comparison_{n_sim}.png")
  ggsave(out_path, fig, width = 8, height = 8)
  cat(glue("Saved: {out_path}\n\n"))
}
