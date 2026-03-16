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
# Try to determine context (RStudio vs. command line)
output_dir <- tryCatch({
  rstudioapi::getActiveDocumentContext()$path %>% dirname() %>% file.path("_generated")
}, error = function(e) {
  # Fallback for non-interactive/CI environment
  file.path(getwd(), "_generated")
})

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

# Function to parse individual .Rd file or parsed Rd object
parse_rd_file <- function(rd_input) {
  tryCatch({
    # Parse the Rd file if it's a character path, otherwise assume it's already parsed
    if (is.character(rd_input)) {
      rd_parsed <- tools::parse_Rd(rd_input)
    } else {
      rd_parsed <- rd_input
    }
    
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
      details_raw = "",
      category = NA,
      instrument_metadata = NA
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
      } else if (tag == "\\category") {
        # Extract function category (import, core, analysis, wrapper)
        result$category <- trimws(as.character(item))
      } else if (tag == "\\instrument") {
        # Extract instrument metadata in format: manufacturer|name|type
        instrument_str <- trimws(as.character(item))
        result$instrument_metadata <- instrument_str
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
# STEP 2: Categorize functions (with metadata-based extraction + fallbacks)
# ==============================================================================

# Fallback mapping for functions without @category tags
fallback_categories <- list(
  core = c("goFlux", "best.flux", "flux.plot", "flux2pdf", "HM.flux", "LM.flux", "k.max", "g.factor", "MDF", "flux.term"),
  analysis = c("autoID", "obs.win", "click.peak2", "iso.comp", "crop.meas", "auto.deadband", "align"),
  wrapper = c("import2RData")
)

# Fallback mapping for instrument metadata
fallback_instruments <- list(
  "import.DX4015" = list(manufacturer = "Gasmet", name = "Gasmet DX4015"),
  "import.EGM5" = list(manufacturer = "PP-Systems", name = "PP-Systems EGM-5"),
  "import.eosMX12" = list(manufacturer = "Eosense", name = "Portable Recirculating Multiplexer"),
  "import.G2201i" = list(manufacturer = "Picarro", name = "Picarro G2201-i"),
  "import.G2508" = list(manufacturer = "Picarro", name = "Picarro G2508"),
  "import.G4301" = list(manufacturer = "Picarro", name = "Picarro G4301"),
  "import.GAIA" = list(manufacturer = "GAIA2TECH", name = "GAIA2TECH ECOFlux"),
  "import.GasmetPD" = list(manufacturer = "Gasmet", name = "Gasmet PD (Custom)"),
  "import.GT5000" = list(manufacturer = "Gasmet", name = "Gasmet GT5000"),
  "import.HT8850" = list(manufacturer = "Healthy Photon", name = "Healthy Photon HT8850"),
  "import.LI6400" = list(manufacturer = "LI-COR", name = "LI-COR LI-6400"),
  "import.LI7810" = list(manufacturer = "LI-COR", name = "LI-COR LI-7810"),
  "import.LI7820" = list(manufacturer = "LI-COR", name = "LI-COR LI-7820"),
  "import.LI8100" = list(manufacturer = "LI-COR", name = "LI-COR LI-8100"),
  "import.LI8150" = list(manufacturer = "LI-COR", name = "LI-COR LI-8150"),
  "import.LI8200" = list(manufacturer = "LI-COR", name = "LI-COR LI-8200"),
  "import.LI8250" = list(manufacturer = "LI-COR", name = "LI-COR LI-8250"),
  "import.N2Oi2" = list(manufacturer = "Los Gatos", name = "Los Gatos N2Oi2"),
  "import.N2OM1" = list(manufacturer = "Los Gatos", name = "Los Gatos N2OM1"),
  "import.skyline" = list(manufacturer = "EarthBound Scientific", name = "Skyline2D"),
  "import.uCH4" = list(manufacturer = "Los Gatos", name = "Los Gatos uCH4"),
  "import.UGGA" = list(manufacturer = "Los Gatos", name = "Los Gatos UGGA"),
  "import.uN2O" = list(manufacturer = "Los Gatos", name = "Los Gatos uN2O")
)

# Function to get instrument metadata from extracted or fallback data
get_instrument_metadata <- function(func_name, metadata) {
  # If extracted from roxygen tags, parse the format: manufacturer|code|name|link
  if (!is.na(metadata$instrument_metadata)) {
    parts <- strsplit(metadata$instrument_metadata, "\\|", fixed = FALSE)[[1]]
    if (length(parts) >= 4) {
      return(list(
        manufacturer = trimws(parts[1]),
        code = trimws(parts[2]),
        name = trimws(parts[3]),
        link = trimws(parts[4])
      ))
    } else if (length(parts) >= 2) {
      # Fallback for 2-part format (manufacturer|name)
      return(list(
        manufacturer = trimws(parts[1]),
        name = trimws(parts[2]),
        link = NA
      ))
    }
  }
  # Fall back to hardcoded mapping
  if (func_name %in% names(fallback_instruments)) {
    return(fallback_instruments[[func_name]])
  }
  return(NULL)
}

# Function to get category from extracted or fallback data
get_function_category <- function(func_name, metadata) {
  # If extracted from roxygen tags
  if (!is.na(metadata$category)) {
    return(tolower(trimws(metadata$category)))
  }
  # Fall back to pattern-based detection
  if (str_detect(func_name, "^import\\.")) return("import")
  if (func_name %in% fallback_categories$core) return("core")
  if (func_name %in% fallback_categories$analysis) return("analysis")
  if (func_name %in% fallback_categories$wrapper) return("wrapper")
  return("other")
}

# ==============================================================================
# STEP 3: Extract metadata for all functions (BEFORE categorization)
# ==============================================================================

all_metadata <- list()

# The help_db uses .Rd filenames as keys, which may differ from function names
# We need to map function names to the correct help entries
for (func_name in exported_functions) {
  # Try to find the help entry for this function
  rd_file <- NULL
  
  # First try exact match
  if (func_name %in% names(help_db)) {
    rd_file <- help_db[[func_name]]
  } else {
    # Try various matching strategies for the help database keys
    possible_keys <- c(
      func_name,                                    # exact match
      paste0(func_name, ".Rd"),                    # with .Rd extension
      gsub("\\.", "_", func_name),                 # dots to underscores
      gsub("\\.", "-", func_name)                  # dots to hyphens
    )
    
    # Look for any of these keys
    for (key in possible_keys) {
      if (key %in% names(help_db)) {
        rd_file <- help_db[[key]]
        break
      }
    }
    
    # If still not found, try pattern matching
    if (is.null(rd_file)) {
      # Create a regex pattern that's more flexible
      pattern <- gsub("\\.", "[\\.\\-_]", func_name)
      matching_keys <- grep(pattern, names(help_db), value = TRUE)
      if (length(matching_keys) > 0) {
        rd_file <- help_db[[matching_keys[1]]]
      }
    }
  }
  
  if (!is.null(rd_file)) {
    metadata <- parse_rd_file(rd_file)
    if (!is.null(metadata)) {
      all_metadata[[func_name]] <- metadata
      cat("✓", func_name, "\n")
    } else {
      cat("✗ Failed to parse metadata for", func_name, "\n")
    }
  } else {
    cat("✗ No help found for", func_name, "\n")
  }
}

cat("\nSuccessfully extracted metadata for", length(all_metadata), "functions\n")

categorize_functions <- function(func_names) {
  imports <- c()
  core <- c()
  analysis <- c()
  wrapper <- c()
  
  # Categorize based on extracted metadata
  for (func_name in func_names) {
    category <- if (func_name %in% names(all_metadata)) {
      get_function_category(func_name, all_metadata[[func_name]])
    } else {
      get_function_category(func_name, list(category = NA))
    }
    
    if (category == "import") imports <- c(imports, func_name)
    else if (category == "core") core <- c(core, func_name)
    else if (category == "analysis") analysis <- c(analysis, func_name)
    else if (category == "wrapper") wrapper <- c(wrapper, func_name)
  }
  
  return(list(
    imports = imports,
    core = core,
    analysis = analysis,
    wrapper = wrapper
  ))
}

# ==============================================================================
# STEP 3.5: Validate instrument metadata (for developers)
# ==============================================================================

# List of instruments already documented in quarto/import.qmd
# These should NOT be validated/flagged as missing - they're ground truth
instruments_documented_in_qmd <- c(
  # LI-COR
  "import.LI6400", "import.LI7810", "import.LI7820", 
  "import.LI8100", "import.LI8150", "import.LI8200", "import.LI8250",
  # Los Gatos Research (LGR)
  "import.UGGA", "import.N2OM1", "import.N2Oi2",
  # GAIA2TECH
  "import.GAIA",
  # EarthBound Scientific
  "import.skyline",
  # Gasmet
  "import.DX4015", "import.GT5000", "import.GasmetPD",
  # Picarro
  "import.G2201i", "import.G2508", "import.G4301",
  # PP-Systems
  "import.EGM5",
  # Aeris Technologies
  "import.uCH4", "import.uN2O",
  # Healthy Photon
  "import.HT8850"
)


validate_instrument_metadata <- function(all_metadata, exported_functions) {
  import_funcs <- grep("^import\\.", exported_functions, value = TRUE)
  import_funcs <- setdiff(import_funcs, c("import2RData", "import2file"))
  
  # Only check instruments NOT already documented in import.qmd
  undocumented_funcs <- setdiff(import_funcs, instruments_documented_in_qmd)
  
  missing_instruments <- c()
  format_errors <- c()
  
  for (func_name in undocumented_funcs) {
    if (func_name %in% names(all_metadata)) {
      metadata <- all_metadata[[func_name]]
      
      if (is.na(metadata$instrument_metadata)) {
        missing_instruments <- c(missing_instruments, func_name)
      } else {
        parts <- strsplit(metadata$instrument_metadata, "\\|")[[1]]
        if (length(parts) != 2) {
          format_errors <- c(format_errors, 
            paste0(func_name, " (", length(parts), " parts, expected 2)"))
        }
      }
    }
  }
  
  # Print summary to console for developers
  cat("\n", strrep("=", 60), "\n")
  cat("DEVELOPER SUMMARY: Instrument Metadata Validation\n")
  cat(strrep("=", 60), "\n")
  cat("Status: Checking", length(undocumented_funcs), "instruments NOT yet in import.qmd\n")
  cat("(", length(instruments_documented_in_qmd), "instruments already documented are skipped)\n\n")
  
  if (length(missing_instruments) == 0 && length(format_errors) == 0) {
    if (length(undocumented_funcs) == 0) {
      cat("✓ All documented instruments are ground truth in import.qmd\n")
    } else {
      cat("✓ All undocumented instruments have valid metadata\n")
    }
  } else {
    if (length(missing_instruments) > 0) {
      cat("\n⚠ Missing @instrument tags (needs ground truth link in import.qmd or @instrument tag):\n")
      for (func in missing_instruments) cat("  -", func, "\n")
    }
    if (length(format_errors) > 0) {
      cat("\n⚠ Invalid @instrument format (expected: Manufacturer|InstrumentName):\n")
      for (err in format_errors) cat("  -", err, "\n")
    }
  }
  cat(strrep("=", 60), "\n\n")
}


categories <- categorize_functions(exported_functions)

# Validate instrument metadata
validate_instrument_metadata(all_metadata, exported_functions)

cat("Categorized functions:\n")
cat("  Import functions:", length(categories$imports), "\n")
cat("  Core functions:", length(categories$core), "\n")
cat("  Analysis functions:", length(categories$analysis), "\n")
cat("  Wrapper functions:", length(categories$wrapper), "\n")

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
  
  # Arguments (markdown table with 1/3 for name, 2/3 for description)
  if (length(metadata$arguments) > 0) {
    lines <- c(lines, "## Arguments")
    lines <- c(lines, "")
    lines <- c(lines, "| Argument | Description |")
    lines <- c(lines, "|:---------|:------------|")
    
    for (arg_name in names(metadata$arguments)) {
      arg_desc <- metadata$arguments[[arg_name]]
      # Clean HTML entities and line breaks
      arg_desc <- gsub("<[^>]+>", "", arg_desc)
      arg_desc <- gsub("&lt;", "<", arg_desc)
      arg_desc <- gsub("&gt;", ">", arg_desc)
      arg_desc <- gsub("&amp;", "&", arg_desc)
      arg_desc <- gsub("\n", " ", arg_desc)  # Remove newlines for table
      # Truncate very long descriptions
      if (nchar(arg_desc) > 150) {
        arg_desc <- paste0(substr(arg_desc, 1, 150), "...")
      }
      
      lines <- c(lines, paste0("| `", arg_name, "` | ", arg_desc, " |"))
    }
    lines <- c(lines, "")
  }
  
  # Value (skip for import functions)
  if (!is.na(metadata$value) && func_category != "import") {
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
  
  # See Also (external links open in new tab, internal links don't)
  if (!is.na(metadata$seealso)) {
    lines <- c(lines, "## See Also")
    lines <- c(lines, "")
    seealso_text <- gsub("<[^>]+>", "", metadata$seealso)
    # Add target="_blank" to external links only
    seealso_text <- gsub(
      '(https?://[^\\s\\)\\]]+)',
      '[\\1](\\1){target="_blank"}',
      seealso_text
    )
    lines <- c(lines, seealso_text)
    lines <- c(lines, "")
  }
  
  return(paste(lines, collapse = "\n"))
}

# Generate reference pages for ALL functions (import, core, analysis, wrapper)
cat("\n=== Generating function reference pages ===\n")

import_refs <- list()

# Helper to generate minimal reference if metadata unavailable
generate_minimal_reference <- function(func_name) {
  lines <- c()
  lines <- c(lines, "---")
  lines <- c(lines, paste0('title: "`', func_name, '`"'))
  lines <- c(lines, paste0('code-block-bg: true'))
  lines <- c(lines, "---")
  lines <- c(lines, "")
  lines <- c(lines, "See [All Functions](function_index.qmd) for complete documentation.")
  lines <- c(lines, "")
  return(paste(lines, collapse = "\n"))
}

# Generate pages for all categorized functions
all_functions_to_document <- c(
  categories$imports,
  categories$core,
  categories$analysis,
  categories$wrapper
)

for (func in all_functions_to_document) {
  filename <- file.path(output_dir, paste0("ref_", func, ".qmd"))
  
  # Use full metadata if available, otherwise generate minimal stub
  if (func %in% names(all_metadata)) {
    func_category <- get_function_category(func, all_metadata[[func]])
    content <- generate_function_reference(all_metadata[[func]], func_category)
  } else {
    content <- generate_minimal_reference(func)
  }
  
  writeLines(content, filename)
  cat("✓", func, "\n")
  
  if (func %in% categories$imports) {
    import_refs[[func]] <- all_metadata[[func]]
  }
}

cat("Generated", length(all_functions_to_document), "function reference pages\n")

# ==============================================================================
# STEP 5: Generate instrument comparison matrix
# ==============================================================================

cat("\n=== Generating instrument comparison matrix ===\n")

# Generate comparison matrix using extracted or fallback metadata
comparison_lines <- c(
  "# Supported Instruments",
  "",
  "The goFlux package supports import functions for 22+ instruments from multiple manufacturers.",
  "",
  "| Function | Instrument | Manufacturer | Type | Import Method |",
  "|----------|-----------|--------------|------|----------------|"
)

for (func in sort(categories$imports)) {
  if (func %in% names(all_metadata)) {
    metadata <- all_metadata[[func]]
    info <- get_instrument_metadata(func, metadata)
    
    if (!is.null(info)) {
      comparison_lines <- c(
        comparison_lines,
        paste0(
          "| [`", func, "`](#", tolower(func), ") | ",
          info$name, " | ",
          info$manufacturer, " | Single or batch |"
        )
      )
    }
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
# STEP 6b: Generate dynamic sidebar navigation
# ==============================================================================

cat("\n=== Generating sidebar navigation components ===\n")

# Create navigation sections for each category
nav_sections <- list(
  imports = list(
    text = "Instrument Imports",
    functions = categories$imports
  ),
  core = list(
    text = "Core Functions",
    functions = categories$core
  ),
  analysis = list(
    text = "Analysis Functions",
    functions = categories$analysis
  )
)

# Create a YAML snippet for sidebar that could be included
nav_yaml_lines <- c(
  "# Dynamic Sidebar Navigation (Generated)",
  "# Add these sections to quarto/_quarto.yml under website.sidebar.contents",
  "# as replacement for hardcoded sections",
  ""
)

for (category_name in names(nav_sections)) {
  section <- nav_sections[[category_name]]
  nav_yaml_lines <- c(
    nav_yaml_lines,
    "# ---",
    paste0("# - section: \"", section$text, "\""),
    paste0("#   text: \"", section$text, "\""),
    "#   contents:"
  )
  
  for (func in sort(section$functions)) {
    nav_yaml_lines <- c(
      nav_yaml_lines,
      paste0("#     - file: _generated/ref_", func, ".qmd")
    )
  }
  nav_yaml_lines <- c(nav_yaml_lines, "")
}

nav_yaml_lines <- c(
  nav_yaml_lines,
  "# ---",
  "",
  "# Note: Featured sections above are commented examples.",
  "# Uncomment and customize the sections you want to include in your sidebar."
)

writeLines(nav_yaml_lines, file.path(output_dir, "sidebar_navigation.yml"))
cat("Generated: sidebar_navigation.yml (example navigation structure)\n")

# ==============================================================================
# STEP 6c: Auto-link manual pages to API documentation
# ==============================================================================

cat("\n=== Auto-linking manual pages to API docs ===\n")

# Find all manual .qmd files (exclude _generated/)
manual_qmd_files <- list.files(
  dirname(output_dir),
  pattern = "^[^_].+\\.qmd$",
  full.names = FALSE
)

# Create reference map: function name -> reference file
function_refs <- list()
for (func in exported_functions) {
  function_refs[[func]] <- paste0("[`", func, "()`](/_generated/ref_", func, ".qmd)")
}

cross_refs_report <- list()

# For each manual page, find mentions of functions and create a reference file
for (qmd_file in manual_qmd_files) {
  qmd_path <- file.path(dirname(output_dir), qmd_file)
  if (!file.exists(qmd_path)) next
  
  content <- readLines(qmd_path, warn = FALSE)
  content_text <- paste(content, collapse = " ")
  
  # Find which functions are mentioned in this file
  mentioned_functions <- c()
  for (func in exported_functions) {
    # Look for function name patterns (as code or in text)
    # Match: `function_name` or `function_name()` or function_name in text
    if (grepl(paste0("\\b", func, "\\b"), content_text)) {
      mentioned_functions <- c(mentioned_functions, func)
    }
  }
  
  if (length(mentioned_functions) > 0) {
    cross_refs_report[[qmd_file]] <- mentioned_functions
    cat("✓", qmd_file, "mentions", length(mentioned_functions), "functions\n")
  }
}

# Save cross-reference report for documentation
cross_ref_json <- jsonlite::toJSON(cross_refs_report, pretty = TRUE)
writeLines(cross_ref_json, file.path(output_dir, "cross_references.json"))
cat("Generated: cross_references.json (auto-detected function mentions in manual pages)\n")

cat("\nNote: Functions mentioned in manual pages are documented in cross_references.json\n")
cat("Manual page authors can use this to add relevant API references.\n")

# ==============================================================================
# STEP 7: Save metadata for examples validation script
# ==============================================================================

cat("\n=== Saving metadata for validation ===\n")

# Save metadata as RData for use by _examples.R
saveRDS(all_metadata, file.path(output_dir, "function_metadata.RDS"))
cat("Saved function metadata to function_metadata.RDS\n")

# ==============================================================================
# STEP 8: Extract and consolidate references from R source files
# ==============================================================================

cat("\n=== Extracting references from R source files ===\n")

# Get the R source directory
r_source_dir <- file.path(dirname(dirname(output_dir)), "R")

if (!dir.exists(r_source_dir)) {
  cat("Warning: R source directory not found at", r_source_dir, "\n")
} else {
  # Read all R files
  r_files <- list.files(r_source_dir, pattern = "\\.R$", full.names = TRUE)
  
  all_references <- list()
  
  for (r_file in r_files) {
    content <- readLines(r_file, warn = FALSE)
    content_text <- paste(content, collapse = "\n")
    
    # Extract @references sections from roxygen comments
    # Pattern: @references followed by text until next @ tag or end
    ref_pattern <- "@references\\s+(.+?)(?=\\n\\s*@|$)"
    matches <- gregexpr(ref_pattern, content_text, perl = TRUE)
    
    if (matches[[1]][1] != -1) {
      refs_text <- regmatches(content_text, matches)[[1]]
      
      for (ref in refs_text) {
        # Clean up the reference text
        ref_clean <- gsub("@references\\s+", "", ref)
        ref_clean <- trimws(ref_clean)
        
        # Associate with function name (extract from nearby @export)
        func_match <- gregexpr("@export\\s+([\\w\\.]+)", content_text, perl = TRUE)
        func_names <- c()
        
        if (func_match[[1]][1] != -1) {
          func_names_raw <- regmatches(content_text, gregexpr("@export\\s+([\\w\\.]+)", content_text, perl = TRUE))[[1]]
          func_names <- gsub("@export\\s+", "", func_names_raw)
        }
        
        for (func in func_names) {
          if (!func %in% names(all_references)) {
            all_references[[func]] <- c()
          }
          all_references[[func]] <- c(all_references[[func]], ref_clean)
        }
      }
    }
  }
  
  cat("Extracted references for", length(all_references), "functions\n")
  
  # ==============================================================================
  # STEP 9: Consolidate references and update metadata
  # ==============================================================================
  
  cat("\n=== Consolidating references across sources ===\n")
  
  references_report <- list()
  
  for (func in names(all_references)) {
    refs <- unique(all_references[[func]])  # Remove duplicates
    
    if (length(refs) > 0) {
      # Check if these references already exist in the website
      # (would need to scan existing .qmd files)
      references_report[[func]] <- list(
        source = "R_files",
        count = length(refs),
        references = refs
      )
      
      cat("✓", func, "-", length(refs), "references found\n")
    }
  }
  
  # Save references report
  writeLines(
    jsonlite::toJSON(references_report, pretty = TRUE),
    file.path(output_dir, "extracted_references.json")
  )
  
  cat("Generated: extracted_references.json\n")
  
  # ==============================================================================
  # STEP 10: Generate references section for website
  # ==============================================================================
  
  cat("\n=== Generating consolidated references page ===\n")
  
  refs_page_lines <- c(
    "# References",
    "",
    "This page consolidates all references mentioned in the goFlux package documentation.",
    "",
    "## References by Function",
    ""
  )
  
  for (func in sort(names(all_references))) {
    refs <- unique(all_references[[func]])
    if (length(refs) > 0) {
      refs_page_lines <- c(
        refs_page_lines,
        paste0("### `", func, "`"),
        ""
      )
      
      for (ref in refs) {
        refs_page_lines <- c(refs_page_lines, paste0("- ", ref))
      }
      
      refs_page_lines <- c(refs_page_lines, "")
    }
  }
  
  writeLines(refs_page_lines, file.path(output_dir, "all_references.qmd"))
  cat("Generated: all_references.qmd\n")
}

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
