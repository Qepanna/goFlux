library(testthat)
library(tidyverse)
library(conflicted)

source("./R/import.DX4015.R")

# Test input file:
inputfile <- "./inst/extdata/DX4015/DX4015.TXT"

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

test_that(
  desc = "import.DX4015() works correctly",
  code = {
    # Test that error handling works with wrong filename
    expect_warning(import.DX4015("hat"))

    # Test that error handling works with wrong date format
    expect_warning(import.DX4015(faulty.date.inputfile))
  }
)
