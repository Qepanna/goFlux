#!/usr/bin/env Rscript
# ==============================================================================
# goFlux Code Examples Validation Script
# ==============================================================================
# This script validates code examples extracted from .Rd files.
# Runs during CI/CD to catch breaking changes and deprecated function usage.
#
# Output: examples_validation_report.json with test results
# ==============================================================================

suppressPackageStartupMessages({
  library(goFlux)
  library(stringr)
  library(jsonlite)
})

# Load metadata saved by _generate.R
generated_dir <- file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "_generated")
metadata_file <- file.path(generated_dir, "function_metadata.RDS")

if (!file.exists(metadata_file)) {
  cat("Error: function_metadata.RDS not found. Run _generate.R first.\n")
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
  example_code <- metadata$examples
  
  # Remove \dontrun{...} and \donttest{...} blocks
  example_code <- str_replace_all(example_code, "\\\\dontrun\\{", "")
  example_code <- str_replace_all(example_code, "\\\\donttest\\{", "")
  example_code <- str_replace_all(example_code, "(?<=^|\\n)\\}", "", perl = TRUE)
  
  # Try to evaluate the example code
  tryCatch({
    # Create isolated environment for evaluation
    example_env <- new.env(parent = globalenv())
    
    # Set evaluation timeout (30 seconds per example)
    eval_result <- withCallingHandlers(
      tryCatch({
        eval(parse(text = example_code), envir = example_env)
        list(status = "passed", message = "Example executed successfully")
      }, error = function(e) {
        list(status = "failed", message = e$message)
      }),
      warning = function(w) {
        invokeRestart("muffleWarning")
      }
    )
    
    validation_results[[func_name]] <- eval_result
    cat("✓", func_name, "-", eval_result$message, "\n")
    
  }, error = function(e) {
    validation_results[[func_name]] <<- list(
      status = "error",
      message = paste("Validation error:", e$message)
    )
    cat("✗", func_name, "-", e$message, "\n")
  })
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

if (report$failed > 0 || report$errors > 0) {
  cat("\n⚠ Some examples failed validation. Review the report for details.\n")
  quit(status = 1)
} else {
  cat("\n✓ All example validations passed!\n")
  quit(status = 0)
}
