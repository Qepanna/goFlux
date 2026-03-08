#==============================================================================#
#                                                                              #
#  goFlux - Flux calculation and automatic selection of best flux estimate     #
#                                                                              #
#  This scripts is used to calculate GHG fluxes and select the best flux       #
#  estimate between LM and HM using the function best.flux                     #
#                                                                              #
#  https://qepanna.quarto.pub/goflux/goFlux.html                               #
#  https://qepanna.quarto.pub/goflux/bestflux.html                             #
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

# Flux calculation -------------------------------------------------------------

# Calculate fluxes for each gas using the function goFlux
CO2_flux <- goFlux(manID.UGGA, "CO2dry_ppm")
CH4_flux <- goFlux(manID.UGGA, "CH4dry_ppb")

# Automatically select the best flux estimate (LM vs HM)
criteria = c("MAE", "AICc", "g.factor", "MDF")

CO2_best <- best.flux(CO2_flux, criteria)
CH4_best <- best.flux(CH4_flux, criteria)

# Save results -----------------------------------------------------------------

# Save flux results as RData
save(CO2_best, file = "results/CO2_result.RData")
save(CH4_best, file = "results/CH4_result.RData")

# Save flux results to Excel
write.xlsx(CO2_best, file = "results/CO2_result.xlsx")
write.xlsx(CH4_best, file = "results/CH4_result.xlsx")


