#!/usr/bin/env Rscript
# ==============================================================================
# goFlux Quarto Documentation Generator
# ==============================================================================
# This script extracts metadata from installed goFlux package .Rd files and
# generates dynamic Quarto markdown includes for API documentation.
#
# Run this during Quarto render to automatically synchronize documentation
# with R package changes.
# ==============================================================================

# Load required packages
suppressPackageStartupMessages({
  library(goFlux)
  library(tools)
  library(stringr)
})

# Create output directory for generated files
output_dir <- file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "_generated")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Created output directory:", output_dir, "\n")
}

# ==============================================================================
# STEP 1: Extract function metadata from installed package
# ==============================================================================

# Get list of all exported functions from NAMESPACE
namespace_file <- system.file("NAMESPACE", package = "goFlux")
namespace_content <- readLines(namespace_file)
exports <- grep("^export\\(", namespace_content, value = TRUE)
exported_functions <- gsub("^export\\(|\\)$", "", exports)

cat("Found", length(exported_functions), "exported functions\n")

# Get help database
help_db <- tools::Rd_db("goFlux")

# Function to parse individual .Rd file
parse_rd_file <- function(rd_path) {
  tryCatch({
    # Parse the Rd file
    rd_parsed <- tools::parse_Rd(rd_path)
    
    # Extract components
    result <- list(
      name = NA,
      title = NA,
      description = NA,
      usage = NA,
      arguments = list(),
      details = NA,
      value = NA,
      examples = NA,
      seealso = NA,
      details_raw = ""
    )
    
    # Process different sections
    for (item in rd_parsed) {
      tag <- attr(item, "Rd_tag")
      
      if (tag == "\\name") {
        result$name <- as.character(item)
      } else if (tag == "\\title") {
        result$title <- paste(as.character(item), collapse = " ")
      } else if (tag == "\\description") {
        result$description <- paste(as.character(item), collapse = " ")
      } else if (tag == "\\usage") {
        result$usage <- paste(as.character(item), collapse = "\n")
      } else if (tag == "\\arguments") {
        # Parse arguments section
        args_parsed <- parse_arguments_section(item)
        result$arguments <- args_parsed
      } else if (tag == "\\details") {
        result$details <- paste(as.character(item), collapse = " ")
        result$details_raw <- paste(as.character(item), collapse = "\n")
      } else if (tag == "\\value") {
        result$value <- paste(as.character(item), collapse = " ")
      } else if (tag == "\\examples") {
        result$examples <- paste(as.character(item), collapse = "\n")
      } else if (tag == "\\seealso") {
        result$seealso <- paste(as.character(item), collapse = " ")
      }
    }
    
    return(result)
  }, error = function(e) {
    cat("Error parsing", rd_path, ":", e$message, "\n")
    return(NULL)
  })
}

# Helper function to parse arguments section
parse_arguments_section <- function(args_item) {
  args_list <- list()
  
  if (is.list(args_item)) {
    for (i in seq_along(args_item)) {
      item <- args_item[[i]]
      tag <- attr(item, "Rd_tag")
      
      if (tag == "\\item") {
        # Extract argument name and description
        item_content <- as.character(item)
        if (length(item_content) >= 2) {
          arg_name <- trimws(item_content[1])
          arg_desc <- trimws(paste(item_content[-1], collapse = " "))
          args_list[[arg_name]] <- arg_desc
        }
      }
    }
  }
  
  return(args_list)
}

# ==============================================================================
# STEP 2: Categorize functions
# ==============================================================================

categorize_functions <- function(func_names) {
  # Import functions: start with "import."
  imports <- func_names[str_detect(func_names, "^import\\.")]
  
  # Core flux calculation: goFlux, best.flux, HM.flux, LM.flux, flux.plot, flux2pdf
  core <- func_names[func_names %in% c(
    "goFlux", "best.flux", "flux.plot", "flux2pdf",
    "HM.flux", "LM.flux", "k.max", "g.factor", "MDF", "flux.term"
  )]
  
  # Measurement identification: autoID, obs.win, click.peak2
  analysis <- func_names[func_names %in% c(
    "autoID", "obs.win", "click.peak2", "iso.comp",
    "crop.meas", "auto.deadband", "align"
  )]
  
  # Data import wrapper
  wrapper <- func_names[func_names == "import2RData"]
  
  return(list(
    imports = imports,
    core = core,
    analysis = analysis,
    wrapper = wrapper
  ))
}

categories <- categorize_functions(exported_functions)

cat("Categorized functions:\n")
cat("  Import functions:", length(categories$imports), "\n")
cat("  Core functions:", length(categories$core), "\n")
cat("  Analysis functions:", length(categories$analysis), "\n")
cat("  Wrapper functions:", length(categories$wrapper), "\n")

# ==============================================================================
# STEP 3: Extract metadata for all functions
# ==============================================================================

all_metadata <- list()

for (func_name in exported_functions) {
  if (func_name %in% names(help_db)) {
    rd_file <- help_db[[func_name]]
    metadata <- parse_rd_file(rd_file)
    if (!is.null(metadata)) {
      all_metadata[[func_name]] <- metadata
      cat("✓", func_name, "\n")
    }
  }
}

cat("\nSuccessfully extracted metadata for", length(all_metadata), "functions\n")

# ==============================================================================
# STEP 4: Generate individual function reference pages
# ==============================================================================

# Create function reference template
generate_function_reference <- function(metadata, func_category) {
  if (is.null(metadata)) return("")
  
  lines <- c()
  lines <- c(lines, "---")
  lines <- c(lines, paste0('title: "`', metadata$name, '`"'))
  lines <- c(lines, paste0('code-block-bg: true'))
  lines <- c(lines, "---")
  lines <- c(lines, "")
  
  # Description
  if (!is.na(metadata$description)) {
    lines <- c(lines, metadata$description)
    lines <- c(lines, "")
  }
  
  # Title/Short description
  if (!is.na(metadata$title)) {
    lines <- c(lines, "## Description")
    lines <- c(lines, "")
    lines <- c(lines, metadata$title)
    lines <- c(lines, "")
  }
  
  # Usage
  lines <- c(lines, "## Usage")
  lines <- c(lines, "")
  lines <- c(lines, "```r")
  if (!is.na(metadata$usage)) {
    lines <- c(lines, metadata$usage)
  }
  lines <- c(lines, "```")
  lines <- c(lines, "")
  
  # Arguments
  if (length(metadata$arguments) > 0) {
    lines <- c(lines, "## Arguments")
    lines <- c(lines, "")
    
    for (arg_name in names(metadata$arguments)) {
      arg_desc <- metadata$arguments[[arg_name]]
      # Clean HTML entities
      arg_desc <- gsub("<[^>]+>", "", arg_desc)  # Remove HTML tags
      arg_desc <- gsub("&lt;", "<", arg_desc)
      arg_desc <- gsub("&gt;", ">", arg_desc)
      arg_desc <- gsub("&amp;", "&", arg_desc)
      
      lines <- c(lines, paste0("**`", arg_name, "`**"))
      lines <- c(lines, "")
      lines <- c(lines, paste0(": ", arg_desc))
      lines <- c(lines, "")
    }
  }
  
  # Value
  if (!is.na(metadata$value)) {
    lines <- c(lines, "## Value")
    lines <- c(lines, "")
    value_text <- gsub("<[^>]+>", "", metadata$value)
    lines <- c(lines, value_text)
    lines <- c(lines, "")
  }
  
  # Details
  if (!is.na(metadata$details)) {
    lines <- c(lines, "## Details")
    lines <- c(lines, "")
    details_text <- gsub("<[^>]+>", "", metadata$details)
    lines <- c(lines, details_text)
    lines <- c(lines, "")
  }
  
  # Examples
  if (!is.na(metadata$examples) && metadata$examples != "") {
    lines <- c(lines, "## Examples")
    lines <- c(lines, "")
    lines <- c(lines, "```r")
    lines <- c(lines, metadata$examples)
    lines <- c(lines, "```")
    lines <- c(lines, "")
  }
  
  # See Also
  if (!is.na(metadata$seealso)) {
    lines <- c(lines, "## See Also")
    lines <- c(lines, "")
    seealso_text <- gsub("<[^>]+>", "", metadata$seealso)
    lines <- c(lines, seealso_text)
    lines <- c(lines, "")
  }
  
  return(paste(lines, collapse = "\n"))
}

# Generate reference pages for import functions
cat("\n=== Generating import function references ===\n")

import_refs <- list()
for (func in categories$imports) {
  if (func %in% names(all_metadata)) {
    filename <- file.path(output_dir, paste0("ref_", func, ".qmd"))
    content <- generate_function_reference(all_metadata[[func]], "import")
    writeLines(content, filename)
    cat("Generated:", filename, "\n")
    import_refs[[func]] <- all_metadata[[func]]
  }
}

# ==============================================================================
# STEP 5: Generate instrument comparison matrix
# ==============================================================================

cat("\n=== Generating instrument comparison matrix ===\n")

# Extract instrument information from import function names and descriptions
instruments <- list()
import_mapping <- list(
  "import.DX4015" = list(name = "Gasmet DX4015", manufacturer = "Gasmet", type = "portable"),
  "import.EGM5" = list(name = "PP-Systems EGM-5", manufacturer = "PP-Systems", type = "portable"),
  "import.eosMX12" = list(name = "Aeris EOS MX12", manufacturer = "Aeris", type = "portable"),
  "import.G2201i" = list(name = "Picarro G2201-i", manufacturer = "Picarro", type = "isotopic"),
  "import.G2508" = list(name = "Picarro G2508", manufacturer = "Picarro", type = "concentration"),
  "import.G4301" = list(name = "Picarro G4301", manufacturer = "Picarro", type = "mobile"),
  "import.GAIA" = list(name = "GAIA2TECH ECOFlux", manufacturer = "GAIA2TECH", type = "chamber"),
  "import.GasmetPD" = list(name = "Gasmet PD (Custom)", manufacturer = "Gasmet", type = "custom"),
  "import.GT5000" = list(name = "Gasmet GT5000", manufacturer = "Gasmet", type = "portable"),
  "import.HT8850" = list(name = "Healthy Photon HT8850", manufacturer = "Healthy Photon", type = "portable"),
  "import.LI6400" = list(name = "LI-COR LI-6400", manufacturer = "LI-COR", type = "portable"),
  "import.LI7810" = list(name = "LI-COR LI-7810", manufacturer = "LI-COR", type = "portable"),
  "import.LI7820" = list(name = "LI-COR LI-7820", manufacturer = "LI-COR", type = "portable"),
  "import.LI8100" = list(name = "LI-COR LI-8100", manufacturer = "LI-COR", type = "automated"),
  "import.LI8150" = list(name = "LI-COR LI-8150", manufacturer = "LI-COR", type = "automated"),
  "import.LI8200" = list(name = "LI-COR LI-8200", manufacturer = "LI-COR", type = "chamber"),
  "import.LI8250" = list(name = "LI-COR LI-8250", manufacturer = "LI-COR", type = "multiplexer"),
  "import.N2Oi2" = list(name = "Los Gatos N2Oi2", manufacturer = "Los Gatos", type = "isotopic"),
  "import.N2OM1" = list(name = "Los Gatos N2OM1", manufacturer = "Los Gatos", type = "portable"),
  "import.skyline" = list(name = "Skyline2D", manufacturer = "EarthBound Scientific", type = "chamber"),
  "import.uCH4" = list(name = "Los Gatos uCH4", manufacturer = "Los Gatos", type = "portable"),
  "import.UGGA" = list(name = "Los Gatos UGGA", manufacturer = "Los Gatos", type = "portable"),
  "import.uN2O" = list(name = "Los Gatos uN2O", manufacturer = "Los Gatos", type = "portable")
)

# Generate comparison matrix
comparison_lines <- c(
  "# Supported Instruments",
  "",
  "The goFlux package supports import functions for 22+ instruments from multiple manufacturers.",
  "",
  "| Function | Instrument | Manufacturer | Type | Import Method |",
  "|----------|-----------|--------------|------|----------------|"
)

for (func in sort(categories$imports)) {
  if (func %in% names(import_mapping)) {
    info <- import_mapping[[func]]
    comparison_lines <- c(
      comparison_lines,
      paste0(
        "| [`", func, "`](#", tolower(func), ") | ",
        info$name, " | ",
        info$manufacturer, " | ",
        info$type, " | Single or batch |"
      )
    )
  }
}

comparison_lines <- c(
  comparison_lines,
  "",
  "See individual function references for detailed documentation on each instrument."
)

writeLines(comparison_lines, file.path(output_dir, "instruments_matrix.qmd"))
cat("Generated: instruments_matrix.qmd\n")

# ==============================================================================
# STEP 6: Generate function index
# ==============================================================================

cat("\n=== Generating function index ===\n")

index_lines <- c(
  "# Function Reference",
  "",
  "## Import Functions",
  "",
  "Functions to import raw data from gas analyzers:"
)

for (func in sort(categories$imports)) {
  if (func %in% names(all_metadata)) {
    meta <- all_metadata[[func]]
    index_lines <- c(
      index_lines,
      paste0("- [`", func, "()`](#", tolower(func), ") - ", 
             if (!is.na(meta$title)) meta$title else "Import function")
    )
  }
}

index_lines <- c(
  index_lines,
  "",
  "## Core Functions",
  "",
  "Functions for flux calculation and analysis:"
)

for (func in sort(categories$core)) {
  if (func %in% names(all_metadata)) {
    meta <- all_metadata[[func]]
    index_lines <- c(
      index_lines,
      paste0("- [`", func, "()`](#", tolower(func), ") - ",
             if (!is.na(meta$title)) meta$title else "Core function")
    )
  }
}

index_lines <- c(
  index_lines,
  "",
  "## Analysis & Measurement Functions",
  "",
  "Functions for measurement identification and analysis:"
)

for (func in sort(categories$analysis)) {
  if (func %in% names(all_metadata)) {
    meta <- all_metadata[[func]]
    index_lines <- c(
      index_lines,
      paste0("- [`", func, "()`](#", tolower(func), ") - ",
             if (!is.na(meta$title)) meta$title else "Analysis function")
    )
  }
}

writeLines(index_lines, file.path(output_dir, "function_index.qmd"))
cat("Generated: function_index.qmd\n")

# ==============================================================================
# STEP 7: Save metadata for examples validation script
# ==============================================================================

cat("\n=== Saving metadata for validation ===\n")

# Save metadata as RData for use by _examples.R
saveRDS(all_metadata, file.path(output_dir, "function_metadata.RDS"))
cat("Saved function metadata to function_metadata.RDS\n")

# ==============================================================================
# COMPLETION
# ==============================================================================

cat("\n✓ Documentation generation complete!\n")
cat("Generated files in:", output_dir, "\n")
cat("Files created:\n")
cat("  -", length(categories$imports), "import function references\n")
cat("  - function_index.qmd\n")
cat("  - instruments_matrix.qmd\n")
cat("  - function_metadata.RDS\n")
