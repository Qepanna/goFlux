# =========================================================================== #
#                              Import Libraries                               #
# =========================================================================== #
library(testthat)   # For testing
library(tidyverse)  # For piping and data manipulation
library(conflicted) # For forcing conflict to be errors

# =========================================================================== #
#                        Source the function to test                          #
# =========================================================================== #
source("./R/import.DX4015.R")

# =========================================================================== #
#                         Create input for the tests                          #
# =========================================================================== #
# Test input file:
inputfile <- "./inst/extdata/DX4015/DX4015.TXT"

# Create a temporary input file with no data
no.data.inputfile <- tempfile(fileext = ".TXT")

# Create a temporary input file with a different date format based on the
# test input file:
faulty.date.inputfile <- tempfile(fileext = ".TXT")
read.delim(inputfile, colClasses = "character") %>%
  mutate(
    Date = {
      splt <- stringr::str_split(Date, "-")
      d <- character(length = length(splt))
      m <- character(length = length(splt))
      y <- character(length = length(splt))
      for (i in seq_along(splt)) {
        d[i] <- splt[[c(i, 3)]]
        m[i] <- splt[[c(i, 2)]]
        y[i] <- splt[[c(i, 1)]]
      }
      paste(m, d, y, sep = "-")
    }
  ) %>%
  write.table(file = faulty.inputfile)

# =========================================================================== #
#                                Run the tests                                #
# =========================================================================== #

test_that(
  desc = "Test that import.DX4015() works correctly",
  code = {
    # Test that error handling works empty data file
    expect_warning(import.DX4015(no.data.inputfile))

    # Test that error handling works with wrong date format
    expect_warning(import.DX4015(faulty.date.inputfile))
  }
)
