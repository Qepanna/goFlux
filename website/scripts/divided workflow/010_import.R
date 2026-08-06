#==============================================================================#
#                                                                              #
#  goFlux - Import raw data                                                    #
#                                                                              #
#  This scripts is used to import raw data from a LGR instrument               #
#                                                                              #
#  https://qepanna.quarto.pub/goflux/import.html                               #
#                                                                              #
#==============================================================================#

# Before going through this workflow, make sure that your working directory
# is set to the folder containing the scripts.
setwd("your_directory/divided workflow")

# Source 000_required_package.R to install and load required packages
source("000_required_packages.R")

# Import raw data --------------------------------------------------------------

# Raw data, directly downloaded from your instrument, should be stored in a
# folder "raw data". If you are using multiple instruments, create a separate
# folder for each instrument.

# Save raw data as RData
import2RData(path = "raw data", instrument = "UGGA", date.format = "dmy",
             keep_all = FALSE, prec = c(0.2, 1.4, 50))

