#==============================================================================#
#                                                                              #
#  goFlux - Simple workflow                                                    #
#                                                                              #
#  This scripts is used to import raw data from a LGR instrument, and          #
#  follow all the subsequent steps of the workflow in the goFlux package.      #
#                                                                              #
#  https://qepanna.quarto.pub/goflux/                                          #
#                                                                              #
#==============================================================================#

# Before going through this workflow, make sure that your working directory
# is set to the folder containing the scripts.
setwd("your_directory/simple workflow")

# Source 000_required_package.R to install and load required packages
source("000_required_packages.R")

# Import raw data --------------------------------------------------------------

# Raw data, directly downloaded from your instrument, should be stored in a
# folder "raw data". If you are using multiple instruments, create a separate
# folder for each instrument.

# Import raw data in your environment
imp.UGGA <- import.UGGA(inputfile = "raw data/UGGA.txt")

# Manual identification of start and end time ----------------------------------

# In this example, the instrument UGGA requires an auxiliary file to identify
# the start and end times of the measurements using the function click.peak2
# Load auxiliary file
auxfile <- read_excel("aux_UGGA.xlsx")

# Define a window of observation for each measurement using the function obs.win
# By default, the gastype used to identify measurements is CO2dry_ppm
ow.UGGA <- obs.win(inputfile = imp.UGGA, auxfile = auxfile, obs.length = 180)

# Click on a scatter plot to identify start and end times using click.peak2
# Tip: create a folder to store results
manID.UGGA <- click.peak2(ow.list = ow.UGGA,
                          save.plots = "results/click.peak.UGGA")

# Flux calculation -------------------------------------------------------------

# Calculate fluxes for each gas using the function goFlux
CO2_flux <- goFlux(manID.UGGA, "CO2dry_ppm")
CH4_flux <- goFlux(manID.UGGA, "CH4dry_ppb")

# Automatically select the best flux estimate (LM vs HM)
criteria = c("MAE", "AICc", "g.factor", "MDF")

CO2_best <- best.flux(CO2_flux, criteria)
CH4_best <- best.flux(CH4_flux, criteria)

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

# Save results -----------------------------------------------------------------

# Save plots to pdf
flux2pdf(plot.list, outfile = "results/UGGA.results.pdf")

# Save flux results as RData
save(CO2_best, file = "results/CO2_result.RData")
save(CH4_best, file = "results/CH4_result.RData")

# Save flux results to Excel
write.xlsx(CO2_best, file = "results/CO2_result.xlsx")
write.xlsx(CH4_best, file = "results/CH4_result.xlsx")


