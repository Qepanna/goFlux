#==============================================================================#
#                                                                              #
#  goFlux - Quality check                                                      #
#                                                                              #
#  This scripts is used to plots the results and save it as pdf                #
#                                                                              #
#  https://qepanna.quarto.pub/goflux/flux2pdf.html                             #
#                                                                              #
#==============================================================================#

# Before going through this workflow, make sure that your working directory
# is set to the folder containing the scripts.
setwd("your_directory/divided workflow")

# Source 000_required_package.R to install and load required packages
source("000_required_packages.R")

# Load RData
manID.UGGA <- list.files(path = "RData", pattern = "manID.RData", full.names = TRUE) %>%
  map_df(~ get(load(.x)))

load(file = "results/CO2_result.RData")
load(file = "results/CH4_result.RData")

# Plot results -----------------------------------------------------------------

plot.legend = c("MAE", "RMSE", "AICc", "k.ratio", "g.factor")
plot.display = c("MDF", "prec", "nb.obs", "flux.term")
quality.check = TRUE

CO2_plots <- flux.plot(CO2_best, manID.UGGA, "CO2dry_ppm", shoulder=20,
                       plot.legend, plot.display, quality.check)
CH4_plots <- flux.plot(CH4_best, manID.UGGA, "CH4dry_ppb", shoulder=20,
                       plot.legend, plot.display, quality.check)

# Combine plot lists into one list
plot.list <- c(CO2_plots, CH4_plots)

# Save plots to pdf
flux2pdf(plot.list, outfile = "results/UGGA.results.pdf")

