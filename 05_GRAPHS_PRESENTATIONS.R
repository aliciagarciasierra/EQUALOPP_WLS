

######## COMBINING THE GRAPHS FOR THE PRESENTATION ###########

library(magick)
library(pdftools)

# Income #
# Import the saved plots
plot_pgis <- image_read_pdf("plots/results_plot_pgis_income_hh.pdf")
plot_observed <- image_read_pdf("plots/results_plot_observed_income_hh.pdf")

# Combine them side by side
combined_plot <- image_append(c(plot_pgis, plot_observed), stack=TRUE)

# Save the combined image
image_write(combined_plot, path = "plots/combined_plot_income.pdf", format = "pdf")

# Education #
# Import the saved plots
plot_pgis <- image_read_pdf("plots/results_plot_pgis_education.pdf")
plot_observed <- image_read_pdf("plots/results_plot_observed_education.pdf")

# Combine them side by side
combined_plot <- image_append(c(plot_pgis, plot_observed), stack=TRUE)

# Save the combined image
image_write(combined_plot, path = "plots/combined_plot_education.pdf", format = "pdf")

# Occupation #
# Import the saved plots
plot_pgis <- image_read_pdf("plots/results_plot_pgis_occupation.pdf")
plot_observed <- image_read_pdf("plots/results_plot_observed_occupation.pdf")

# Combine them side by side
combined_plot <- image_append(c(plot_pgis, plot_observed), stack=TRUE)

# Save the combined image
image_write(combined_plot, path = "plots/combined_plot_occupation.pdf", format = "pdf")

# Wealth #
# Import the saved plots
plot_pgis <- image_read_pdf("plots/results_plot_pgis_wealth.pdf")
plot_observed <- image_read_pdf("plots/results_plot_observed_wealth.pdf")

# Combine them side by side
combined_plot <- image_append(c(plot_pgis, plot_observed), stack=TRUE)

# Save the combined image
image_write(combined_plot, path = "plots/combined_plot_wealth.pdf", format = "pdf")




