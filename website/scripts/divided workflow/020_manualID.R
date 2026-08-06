#==============================================================================#
#                                                                              #
#  goFlux - Identify start and end of measurements                             #
#                                                                              #
#  This scripts is used to manually identify start and end of measurements     #
#                                                                              #
#  https://qepanna.quarto.pub/goflux/manualID.html                             #
#                                                                              #
#==============================================================================#

# Before going through this workflow, make sure that your working directory
# is set to the folder containing the scripts.
setwd("your_directory/divided workflow")

# Source 000_required_package.R to install and load required packages
source("000_required_packages.R")

# Load RData
# Tip: use the function list.files to retrieve all files within the RData folder
# with the pattern "imp.RData". Then use the function map_df to load all files
# and combine them into one data frame.
imp.UGGA <- list.files(path = "RData", pattern = "imp.RData", full.names = TRUE) %>%
  map_df(~ get(load(.x)))

# Manual identification of start and end time ----------------------------------

# In this example, the instrument UGGA requires an auxiliary file to identify
# the start and end times of the measurements using the function click.peak2
# Load auxiliary file
auxfile <- read_excel("aux_UGGA.xlsx")

# Define a window of observation for each measurement using the function obs.win
# By default, the gastype used to identify measurements is CO2dry_ppm
ow.UGGA <- obs.win(inputfile = imp.UGGA, auxfile = auxfile, obs.length = 180)

# Click on a scatter plot to identify start and end times using click.peak2
# Warning: for more than 20 measurements, it is highly recommended to reduce
# the number of measurements per loop in click.peak2 using the argument seq
# Tip: create a folder to store results
manID.UGGA1 <- click.peak2(ow.list = ow.UGGA, seq = seq(1,3),
                           save.plots = "results/click.peak.UGGA1")

manID.UGGA2 <- click.peak2(ow.list = ow.UGGA, seq = seq(4,6),
                           save.plots = "results/click.peak.UGGA2")

manID.UGGA <- rbind(manID.UGGA1, manID.UGGA2)

# Save manID.UGGA as RData
# Tip: include a meaningful pattern to be able to list these files,
# like it was shown on lines 19-23
save(manID.UGGA, file = "RData/manID.UGGA_manID.RData")
