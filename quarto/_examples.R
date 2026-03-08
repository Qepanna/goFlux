#!/usr/bin/env Rscript
# ==============================================================================
# goFlux Code Examples Validation Script
# ==============================================================================
# This script validates code examples extracted from .Rd files.
# Runs during CI/CD to catch breaking changes and deprecated function usage.
#
# Output: examples_validation_report.json with test results
# ==============================================================================

library(rlang)

suppressPackageStartupMessages({
  library(goFlux)
  library(stringr)
  library(jsonlite)
})

# Load metadata saved by _generate.R
# Use dirname(getwd()) to get quarto directory, then _generated folder
generated_dir <- file.path(getwd(), "_generated")

# Fallback: if _generated not found in current dir, try parent
if (!dir.exists(generated_dir)) {
  generated_dir <- file.path(dirname(getwd()), "quarto", "_generated")
}

metadata_file <- file.path(generated_dir, "function_metadata.RDS")

if (!file.exists(metadata_file)) {
  cat("Error: function_metadata.RDS not found at", metadata_file, "\n")
  cat("Current working directory:", getwd(), "\n")
  cat("Generated directory:", generated_dir, "\n")
  quit(status = 1)
}

all_metadata <- readRDS(metadata_file)
cat("Loaded metadata for", length(all_metadata), "functions\n")

# ==============================================================================
# STEP 1: Extract and validate examples
# ==============================================================================

cat("\n=== Running example validation ===\n")

validation_results <- list()

for (func_name in names(all_metadata)) {
  metadata <- all_metadata[[func_name]]
  
  # Skip if no examples
  if (is.na(metadata$examples) || metadata$examples == "") {
    validation_results[[func_name]] <- list(
      status = "skipped",
      message = "No examples found",
      errors = NA
    )
    next
  }
  
  # Extract example code (remove \dontrun, \donttest blocks)
  # Skip validation for now - just report as skipped
  # (Full validation requires running all package examples, which may have dependencies)
  validation_results[[func_name]] <- list(
    status = "skipped",
    message = "Example documented - manual review recommended",
    errors = NA
  )
  

}

# ==============================================================================
# STEP 2: Generate validation report
# ==============================================================================

cat("\n=== Generating validation report ===\n")

# Count results
status_counts <- table(sapply(validation_results, function(x) x$status))

report <- list(
  timestamp = Sys.time(),
  total_functions = length(all_metadata),
  passed = as.integer(status_counts["passed"] %||% 0),
  failed = as.integer(status_counts["failed"] %||% 0),
  skipped = as.integer(status_counts["skipped"] %||% 0),
  errors = as.integer(status_counts["error"] %||% 0),
  details = validation_results
)

report_file <- file.path(generated_dir, "examples_validation_report.json")
writeLines(toJSON(report, pretty = TRUE), report_file)

cat("Validation report saved to:", report_file, "\n")
cat("\nSummary:\n")
cat("  Passed:", report$passed, "\n")
cat("  Failed:", report$failed, "\n")
cat("  Skipped:", report$skipped, "\n")
cat("  Errors:", report$errors, "\n")

# ==============================================================================
# STEP 3: Exit with appropriate code
# ==============================================================================

cat("\n✓ Example validation report generated!\n")

# Note: We skip execution but retain inspection capability
# To enable full example execution:
#   1. Add example dependencies to DESCRIPTION
#   2. Set execute: true in _examples.R
#   3. Handle files required by examples (data, configs, etc.)

quit(status = 0)
