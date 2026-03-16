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

# Developer debug (uncomment to see export extraction)
# cat("Found", length(exported_functions), "exported functions\n")

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
      } else if (tag == "\\instrumentlink") {
        # Extract instrument metadata in format: manufacturer|name|type
        instrument_str <- trimws(as.character(item))
        result$instrument_metadata <- instrument_str
      }
    }
    
    return(result)
  }, error = function(e) {
    cat("Error parsing Rd content:", e$message, "\n")
    return(NULL)
  })
}

# Helper function to recursively flatten Rd objects to text
flatten_rd_to_text <- function(obj) {
  if (is.null(obj)) return("")
  if (is.character(obj)) return(paste(obj, collapse = " "))
  if (is.list(obj)) {
    # Recursively flatten list elements
    parts <- sapply(obj, flatten_rd_to_text)
    return(paste(parts, collapse = " "))
  }
  # For other types, convert to string
  return(as.character(obj))
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
        # Structure: \item is a list with 2 elements: name and description
        if (is.list(item) && length(item) >= 2) {
          # item[[1]] = name (can be char or list)
          arg_name <- flatten_rd_to_text(item[[1]])
          
          # item[[2]] = description (can be char, list, or complex Rd structure)
          arg_desc <- flatten_rd_to_text(item[[2]])
          
          if (arg_name != "" && arg_desc != "") {
            # Clean up the description: remove extra whitespace, newlines
            arg_desc <- gsub("\\n+", " ", arg_desc)  # Replace newlines with spaces
            arg_desc <- gsub("\\s+", " ", arg_desc)   # Replace multiple spaces with single space
            arg_desc <- trimws(arg_desc)
            
            args_list[[arg_name]] <- arg_desc
          }
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

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (is.character(x) && identical(trimws(x), ""))) y else x

is_url_token <- function(token) {
  grepl("(https?://|www\\.)", trimws(token), ignore.case = TRUE)
}

extract_first_url <- function(text) {
  if (is.null(text) || is.na(text)) return(NA_character_)
  match <- regexpr("(https?://[^\\s|]+|www\\.[^\\s|]+)", text, perl = TRUE, ignore.case = TRUE)
  if (match[1] == -1) return(NA_character_)
  regmatches(text, match)[1]
}

extract_instrument_keyvalues <- function(tokens) {
  key_map <- list(
    manufacturer = c("manufacturer", "mfg", "brand", "company", "maker"),
    code = c("code", "model", "instrument", "id", "short", "shortname"),
    name = c("name", "instrument_name", "title", "device", "instrumentname"),
    link = c("url", "link", "website", "web", "site")
  )

  parsed <- list()

  for (token in tokens) {
    token_clean <- trimws(token)
    if (!grepl("[:=]", token_clean)) next

    match <- regexec("^([^:=]+?)\\s*[:=]\\s*(.+)$", token_clean, perl = TRUE)
    matched <- regmatches(token_clean, match)[[1]]
    if (length(matched) < 3) next

    key <- tolower(trimws(matched[2]))
    value <- trimws(matched[3])
    if (value == "") next

    for (target_key in names(key_map)) {
      if (key %in% key_map[[target_key]]) {
        parsed[[target_key]] <- value
      }
    }
  }

  parsed
}

normalize_instrument_info <- function(instrument_info) {
  if (is.null(instrument_info)) return(NULL)

  manufacturer <- instrument_info$manufacturer %||% NA
  code <- instrument_info$code %||% NA
  name <- instrument_info$name %||% NA
  link <- instrument_info$link %||% NA

  if (!is.na(link) && !is_url_token(link)) {
    if (grepl("https?://", link, ignore.case = TRUE)) {
      link <- sub("^.*?(https?://.*)$", "\\1", link)
    } else {
      link <- NA
    }
  }

  if (is.na(name) && !is.na(code)) name <- code

  if (is.na(manufacturer) && is.na(name) && is.na(link) && is.na(code)) {
    return(NULL)
  }

  list(
    manufacturer = manufacturer,
    code = code,
    name = name,
    link = link
  )
}

parse_instrument_metadata <- function(raw_instrument) {
  if (is.null(raw_instrument) || is.na(raw_instrument)) return(NULL)

  raw_clean <- trimws(raw_instrument)
  if (raw_clean == "") return(NULL)

  tokens <- trimws(unlist(strsplit(raw_clean, "\\|", fixed = FALSE)))
  tokens <- tokens[tokens != ""]
  if (length(tokens) == 0) return(NULL)

  keyvalues <- extract_instrument_keyvalues(tokens)

  token_urls <- sapply(tokens, extract_first_url)
  has_url <- !is.na(token_urls)
  url_tokens <- token_urls[has_url]
  non_url_tokens <- tokens[!has_url]

  manufacturer <- keyvalues$manufacturer %||% NA
  code <- keyvalues$code %||% NA
  name <- keyvalues$name %||% NA
  link <- keyvalues$link %||% (if (length(url_tokens) > 0) url_tokens[1] else NA)

  # Flexible positional fallback if key-value tags are absent or partial.
  if (length(keyvalues) == 0) {
    if (is.na(manufacturer) && length(non_url_tokens) >= 1) {
      manufacturer <- non_url_tokens[1]
    }

    if (is.na(name)) {
      if (length(non_url_tokens) == 2) {
        name <- non_url_tokens[2]
      } else if (length(non_url_tokens) == 3) {
        name <- non_url_tokens[2]
      } else if (length(non_url_tokens) >= 4) {
        name <- non_url_tokens[3]
      }
    }

    if (is.na(code)) {
      if (length(non_url_tokens) == 3) {
        code <- non_url_tokens[3]
      } else if (length(non_url_tokens) >= 4) {
        code <- non_url_tokens[2]
      }
    }
  } else {
    if (is.na(manufacturer) && length(non_url_tokens) >= 1) {
      manufacturer <- non_url_tokens[1]
    }
    if (is.na(name) && length(non_url_tokens) >= 2) {
      name <- non_url_tokens[length(non_url_tokens)]
    }
    if (is.na(code) && length(non_url_tokens) >= 3) {
      code <- non_url_tokens[2]
    }
  }

  normalize_instrument_info(list(
    manufacturer = manufacturer,
    code = code,
    name = name,
    link = link
  ))
}

extract_instrument_tags_from_sources <- function(r_source_dir) {
  tags <- list()

  if (!dir.exists(r_source_dir)) return(tags)

  r_files <- list.files(r_source_dir, pattern = "^import\\..+\\.R$", full.names = TRUE)

  for (r_file in r_files) {
    lines <- readLines(r_file, warn = FALSE)
    tag_line <- grep("^#'\\s*@instrumentlink\\b", lines, value = TRUE)
    if (length(tag_line) == 0) next

    raw_value <- sub("^#'\\s*@instrumentlink\\s*", "", tag_line[1])
    func_name <- sub("\\.R$", "", basename(r_file))

    tags[[func_name]] <- trimws(raw_value)
  }

  tags
}

# Function to get instrument metadata from extracted or fallback data
get_instrument_metadata <- function(func_name, metadata) {
  # 1) Priority: source-level @instrumentlink extraction from R/import.*.R
  if (exists("source_instrument_metadata") && func_name %in% names(source_instrument_metadata)) {
    source_info <- normalize_instrument_info(source_instrument_metadata[[func_name]])
    if (!is.null(source_info)) return(source_info)
  }

  # 2) Parsed metadata string (when available)
  if (!is.null(metadata) && !is.null(metadata$instrument_metadata) && !is.na(metadata$instrument_metadata)) {
    parsed_info <- parse_instrument_metadata(metadata$instrument_metadata)
    if (!is.null(parsed_info)) return(parsed_info)
  }

  # Fall back to hardcoded mapping
  if (func_name %in% names(fallback_instruments)) {
    fallback_info <- fallback_instruments[[func_name]]
    return(normalize_instrument_info(fallback_info))
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

# Developer debug: Successfully extracted metadata
# cat("\nSuccessfully extracted metadata for", length(all_metadata), "functions\n")

# Extract @instrumentlink tags directly from R source (priority over fallback mappings)
r_source_dir <- file.path(dirname(dirname(output_dir)), "R")
source_instrument_tags <- extract_instrument_tags_from_sources(r_source_dir)
source_instrument_metadata <- lapply(source_instrument_tags, parse_instrument_metadata)

# Merge raw source tags into metadata store when available
for (func_name in names(source_instrument_tags)) {
  raw_tag <- source_instrument_tags[[func_name]]

  if (!func_name %in% names(all_metadata)) {
    all_metadata[[func_name]] <- list(
      name = func_name,
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
      instrument_metadata = if (raw_tag == "") NA else raw_tag
    )
  } else if (is.na(all_metadata[[func_name]]$instrument_metadata) || trimws(all_metadata[[func_name]]$instrument_metadata) == "") {
    all_metadata[[func_name]]$instrument_metadata <- if (raw_tag == "") NA else raw_tag
  }
}

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
    metadata <- if (func_name %in% names(all_metadata)) all_metadata[[func_name]] else list(instrument_metadata = NA)
    instrument_info <- get_instrument_metadata(func_name, metadata)

    if (is.null(instrument_info)) {
      missing_instruments <- c(missing_instruments, func_name)
      next
    }

    if ((is.null(instrument_info$manufacturer) || is.na(instrument_info$manufacturer) || trimws(instrument_info$manufacturer) == "") &&
        (is.null(instrument_info$name) || is.na(instrument_info$name) || trimws(instrument_info$name) == "")) {
      format_errors <- c(format_errors, paste0(func_name, " (unable to infer manufacturer/name)"))
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
      cat("\n⚠ Missing @instrumentlink tags (needs ground truth link in import.qmd or @instrumentlink tag):\n")
      for (func in missing_instruments) cat("  -", func, "\n")
    }
    if (length(format_errors) > 0) {
      cat("\n⚠ Invalid @instrumentlink format (unable to infer required fields):\n")
      for (err in format_errors) cat("  -", err, "\n")
    }
  }
  cat(strrep("=", 60), "\n\n")
}


categories <- categorize_functions(exported_functions)

# Validate instrument metadata (developer feedback only)
if (Sys.getenv("GOFLUX_DEBUG") == "1") {
  validate_instrument_metadata(all_metadata, exported_functions)
  cat("Categorized functions:\n")
  cat("  Import functions:", length(categories$imports), "\n")
  cat("  Core functions:", length(categories$core), "\n")
  cat("  Analysis functions:", length(categories$analysis), "\n")
  cat("  Wrapper functions:", length(categories$wrapper), "\n")
}

# ==============================================================================
# STEP 3.5+: Intelligent Import Documentation Generation (NEW)
# ==============================================================================
# These functions extract prose patterns from existing documentation and
# generate new import sections that blend seamlessly with hand-authored content.

# Convert Rd LaTeX markup to Markdown
convert_rd_latex_to_markdown <- function(text) {
  if (is.null(text) || is.na(text)) return("")
  
  # Handle list objects and complex structures
  if (is.list(text)) {
    text <- paste(as.character(text), collapse = " ")
  }
  
  text <- as.character(text)
  
  # Remove R list() artifacts
  text <- gsub('list\\([^)]*\\)', "", text)
  text <- gsub('\\\\code\\{([^}]+)\\}', "`\\1`", text)
  text <- gsub('\\\\emph\\{([^}]+)\\}', "*\\1*", text)
  text <- gsub('\\\\strong\\{([^}]+)\\}', "**\\1**", text)
  text <- gsub('\\\\href\\{([^}]+)\\}\\{([^}]+)\\}', "[\\2](\\1)", text)
  text <- gsub('\\\\link\\[([^\\]]+)\\]\\{([^}]+)\\}', "[\\2](goFlux.qmd#\\1)", text)
  text <- gsub('\\\\link\\{([^}]+)\\}', "[\\1](goFlux.qmd#\\1)", text)
  text <- gsub('\\\\url\\{([^}]+)\\}', "[\\1](\\1)", text)
  text <- gsub('\\\\ifelse\\{html\\}\\{\\\\out\\{([^}]+)\\}\\}\\{[^}]+\\}', "\\1", text)
  text <- gsub('~', " ", text)
  text <- gsub('\\\\n', "\n", text)
  
  # Clean up excessive whitespace
  text <- gsub('\\s+', " ", text)
  trimws(text)
}

# Clean argument description for table display
clean_arg_description <- function(desc, max_length = 180) {
  if (is.null(desc) || is.na(desc)) return("")
  
  # Convert to string and remove artifacts
  desc <- as.character(desc)
  desc <- convert_rd_latex_to_markdown(desc)
  
  # Remove line breaks for table
  desc <- gsub("\n", " ", desc, fixed = TRUE)
  
  # Limit length
  if (nchar(desc) > max_length) {
    desc <- paste0(substr(desc, 1, max_length - 3), "...")
  }
  
  desc
}

# Extract prose templates from existing imports in import.qmd
extract_prose_templates <- function(import_qmd_path) {
  if (!file.exists(import_qmd_path)) return(list())
  
  lines <- readLines(import_qmd_path, warn = FALSE)
  text <- paste(lines, collapse = "\n")
  
  templates <- list(
    has_arguments_table = grepl("^\\|.*\\|.*\\|\\s*$", paste(lines, collapse = "\n"), perl = TRUE),
    has_details_section = grepl("#### Details|^#### Details", text, perl = TRUE, ignore.case = TRUE),
    has_examples_section = grepl("#### Example|^#### Example", text, perl = TRUE, ignore.case = TRUE)
  )
  
  # Extract example of existing Arguments table format
  arg_table_pattern <- "^\\|\\s*`?[^|]+`?\\s*\\|\\s*[^|]+\\s*\\|"
  if (grepl(arg_table_pattern, text, perl = TRUE)) {
    templates$arguments_table_exists <- TRUE
  }
  
  templates
}

# Learn manufacturer groups from import.qmd section headers
learn_manufacturer_groups <- function(import_qmd_path) {
  if (!file.exists(import_qmd_path)) {
    return(list(
      "LI-COR" = c(), "LGR" = c(), "GAIA" = c(), "Gasmet" = c(),
      "Picarro" = c(), "PP-Systems" = c(), "Aeris" = c(),
      "HealthyPhoton" = c(), "GasmetPD" = c()
    ))
  }
  
  lines <- readLines(import_qmd_path, warn = FALSE)
  
  groups <- list(
    "LI-COR" = c(),
    "LGR" = c(),
    "GAIA" = c(),
    "Gasmet" = c(),
    "Picarro" = c(),
    "PP-Systems" = c(),
    "Aeris" = c(),
    "HealthyPhoton" = c(),
    "GasmetPD" = c()
  )
  
  # Find section headers and extract import functions under each
  for (i in seq_along(lines)) {
    line <- lines[i]
    
    # Detect manufacturer section headers
    if (grepl("^##+ ", line)) {
      # Extract manufacturer name from header
      header <- sub("^#+\\s+", "", line)
      
      # Find import functions mentioned in next N lines
      for (j in (i+1):min(i+100, length(lines))) {
        if (grepl("^##+ ", lines[j])) break  # Stop at next section
        
        # Extract import.FUNCNAME patterns
        import_matches <- gregexpr("\\bimport\\.[A-Za-z0-9]+\\b", lines[j], perl = TRUE)
        if (length(import_matches) > 0 && import_matches[[1]][1] > 0) {
          funcs <- regmatches(lines[j], import_matches)[[1]]
          
          # Try to match manufacturer
          for (mfg in names(groups)) {
            if (grepl(mfg, header, ignore.case = TRUE) ||
                grepl(mfg, paste(lines[i:(i+5)], collapse = " "), ignore.case = TRUE)) {
              groups[[mfg]] <- c(groups[[mfg]], funcs)
            }
          }
        }
      }
    }
  }
  
  # Remove duplicates
  groups <- lapply(groups, unique)
  groups
}

# Extract structure template from an existing documented instrument section in import.qmd
extract_instrument_section_template <- function(import_qmd_path, reference_instrument) {
  if (!file.exists(import_qmd_path)) {
    return(list(
      has_usage = TRUE,
      has_arguments = TRUE,
      has_details = TRUE,
      has_examples = TRUE,
      subsection_order = c("usage", "arguments", "details", "examples"),
      table_format = "standard"
    ))
  }
  
  lines <- readLines(import_qmd_path, warn = FALSE)
  text <- paste(lines, collapse = "\n")
  
  # Find the reference instrument section (e.g., "### LI-6400" or "### LI-8200 (Smart Chamber)")
  # Look for a ### header that contains the reference instrument name
  section_pattern <- paste0("^###\\s+.*", reference_instrument, ".*$")
  section_indices <- grep(section_pattern, lines, perl = TRUE, ignore.case = TRUE)
  
  template <- list(
    has_usage = FALSE,
    has_arguments = FALSE,
    has_details = FALSE,
    has_examples = FALSE,
    subsection_order = c(),
    table_format = "standard"
  )
  
  if (length(section_indices) == 0) {
    # Reference instrument not found, return defaults
    return(template)
  }
  
  # Get the section range (from this header to the next ### header or end of file)
  start_idx <- section_indices[1]
  end_idx <- length(lines)
  
  next_section <- grep("^###\\s+", lines[(start_idx + 1):length(lines)], perl = TRUE)
  if (length(next_section) > 0) {
    end_idx <- start_idx + next_section[1]
  }
  
  section_lines <- lines[start_idx:end_idx]
  section_text <- paste(section_lines, collapse = "\n")
  
  # Detect subsections in order of appearance
  subsection_order <- c()
  for (line in section_lines) {
    if (grepl("^####\\s+Usage", line, ignore.case = TRUE)) {
      template$has_usage <- TRUE
      subsection_order <- c(subsection_order, "usage")
    } else if (grepl("^####\\s+Arguments", line, ignore.case = TRUE)) {
      template$has_arguments <- TRUE
      subsection_order <- c(subsection_order, "arguments")
    } else if (grepl("^####\\s+Details", line, ignore.case = TRUE)) {
      template$has_details <- TRUE
      subsection_order <- c(subsection_order, "details")
    } else if (grepl("^####\\s+Example", line, ignore.case = TRUE)) {
      template$has_examples <- TRUE
      subsection_order <- c(subsection_order, "examples")
    }
  }
  
  # Remove duplicates while preserving order
  template$subsection_order <- subsection_order[!duplicated(subsection_order)]
  
  # Detect table format (check for blank columns or specific separator patterns)
  arg_table_pattern <- "^\\|\\s*\\|\\s*\\|"
  if (grepl(arg_table_pattern, section_text, perl = TRUE)) {
    template$table_format <- "blank_headers"
  } else {
    template$table_format <- "standard"
  }
  
  template
}

# Generate intelligent import section from Rd metadata using template
generate_intelligent_import_section <- function(func_name, inventory_entry, metadata_store, prose_templates, template = NULL) {
  metadata <- metadata_store[[func_name]]
  if (is.null(metadata)) return("")
  
  # Use default template if none provided
  if (is.null(template)) {
    template <- list(
      has_usage = TRUE,
      has_arguments = TRUE,
      has_details = TRUE,
      has_examples = TRUE,
      subsection_order = c("usage", "arguments", "details", "examples"),
      table_format = "standard"
    )
  }
  
  lines <- c()
  
  # Section header
  section_header <- if (!is.null(inventory_entry$instrument) && !is.null(inventory_entry$instrument$name)) {
    inventory_entry$instrument$name
  } else {
    func_name
  }
  
  lines <- c(lines, paste0("### ", section_header, " {#sec-single-", tolower(gsub("\\.", "", func_name)), "}"))
  lines <- c(lines, "")
  
  # Title/Description paragraph (prose style)
  if (!is.na(metadata$title) && metadata$title != "") {
    lines <- c(lines, paste0("Import single raw gas measurement files from the ", metadata$title, " with the function `", func_name, "`."))
    lines <- c(lines, "")
  }
  
  # Build sections in template order
  for (section_type in template$subsection_order) {
    if (section_type == "usage") {
      lines <- c(lines, "#### Usage")
      lines <- c(lines, "")
      lines <- c(lines, "```{r}")
      lines <- c(lines, "#| eval: false")
      lines <- c(lines, "#| code-copy: false")
      
      if (!is.na(metadata$usage) && metadata$usage != "") {
        # Format usage nicely
        usage_lines <- strsplit(metadata$usage, "\n")[[1]]
        lines <- c(lines, usage_lines)
      }
      
      lines <- c(lines, "```")
      lines <- c(lines, "")
      
    } else if (section_type == "arguments") {
      # Arguments section (as table)
      if (length(metadata$arguments) > 0) {
        lines <- c(lines, "#### Arguments")
        lines <- c(lines, "")
        
        # Use table format from template
        if (template$table_format == "blank_headers") {
          lines <- c(lines, "|  |  |")
          lines <- c(lines, "|--------------------------|----------------------------------------------|")
        } else {
          lines <- c(lines, "| Parameter | Description |")
          lines <- c(lines, "|:----------|:--------------------------|")
        }
        
        for (arg_name in names(metadata$arguments)) {
          arg_desc <- metadata$arguments[[arg_name]]
          arg_desc <- clean_arg_description(arg_desc)
          
          # Escape pipe characters for markdown table
          arg_desc <- gsub("|", "\\|", arg_desc, fixed = TRUE)
          
          lines <- c(lines, paste0("| `", arg_name, "` | ", arg_desc, " |"))
        }
        lines <- c(lines, "")
      }
      
    } else if (section_type == "details") {
      # Details section
      if (!is.na(metadata$details) && metadata$details != "") {
        lines <- c(lines, "#### Details")
        lines <- c(lines, "")
        details_md <- convert_rd_latex_to_markdown(metadata$details)
        lines <- c(lines, details_md)
        lines <- c(lines, "")
      }
      
    } else if (section_type == "examples") {
      # Examples section
      if (!is.na(metadata$examples) && metadata$examples != "") {
        lines <- c(lines, "#### Example")
        lines <- c(lines, "")
        lines <- c(lines, "```{r}")
        lines <- c(lines, "#| eval: false")
        
        example_lines <- strsplit(metadata$examples, "\n")[[1]]
        lines <- c(lines, example_lines)
        
        lines <- c(lines, "```")
        lines <- c(lines, "")
      }
    }
  }
  
  paste(lines, collapse = "\n")
}

# Find the best insertion point in import.qmd for a function (ALPHABETICALLY INTELLIGENT)
find_insertion_point_for_import <- function(func_name, manufacturer_groups, import_qmd_lines) {
  import_qmd_text <- paste(import_qmd_lines, collapse = "\n")
  
  # Check if function is already in the file
  if (grepl(func_name, import_qmd_text, fixed = TRUE)) {
    return(NULL)  # Don't insert if already present
  }
  
  # Extract all existing instrument section headers (### import.XXXX or ### Instrument Name)
  # Look for level 3 headers that represent individual instruments
  section_pattern <- "^###\\s+(.+)$"
  section_indices <- grep(section_pattern, import_qmd_lines, perl = TRUE)
  
  if (length(section_indices) == 0) {
    # No instrument sections found, append at end
    return(NULL)
  }
  
  # Extract section titles and build a sortable list
  existing_sections <- list()
  for (idx in section_indices) {
    title <- gsub(section_pattern, "\\1", import_qmd_lines[idx], perl = TRUE)
    # Normalize for sorting: extract function name if it's "import.XXXXX"
    sort_key <- tolower(title)
    existing_sections[[as.character(idx)]] <- list(
      line_number = idx,
      title = title,
      sort_key = sort_key
    )
  }
  
  # Normalize func_name for comparison
  func_sort_key <- tolower(func_name)
  
  # Find the alphabetically correct insertion point
  insertion_point <- NULL
  for (idx_str in names(existing_sections)) {
    section <- existing_sections[[idx_str]]
    # If func_name comes before this section alphabetically, insert before it
    if (func_sort_key < section$sort_key) {
      insertion_point <- section$line_number - 1
      break
    }
  }
  
  # If not found (func_name comes after all existing sections alphabetically),
  # insert after the last instrument section
  if (is.null(insertion_point)) {
    last_idx <- max(as.numeric(names(existing_sections)))
    # Find the end of the last section (next level 2 header or end of file)
    for (j in (last_idx + 1):length(import_qmd_lines)) {
      if (grepl("^##\\s+", import_qmd_lines[j]) || j == length(import_qmd_lines)) {
        insertion_point <- j - 1
        break
      }
    }
  }
  
  insertion_point
}

# ==============================================================================
# STEP 3.6: Incremental import documentation sync (ground truth + auto-managed)
# ============================================================================== 

cat("\n=== Building incremental import sync report ===\n")

extract_documented_import_functions <- function(import_qmd_path) {
  if (!file.exists(import_qmd_path)) return(character(0))
  lines <- readLines(import_qmd_path, warn = FALSE)
  text <- paste(lines, collapse = "\n")
  matches <- gregexpr("\\bimport\\.[A-Za-z0-9]+\\b", text, perl = TRUE)
  documented <- regmatches(text, matches)[[1]]
  unique(documented)
}

build_import_inventory <- function(import_funcs, metadata_store) {
  inventory <- list()

  for (func_name in sort(import_funcs)) {
    metadata <- if (func_name %in% names(metadata_store)) metadata_store[[func_name]] else NULL

    args <- if (!is.null(metadata) && length(metadata$arguments) > 0) names(metadata$arguments) else character(0)
    usage <- if (!is.null(metadata) && !is.na(metadata$usage)) metadata$usage else ""
    title <- if (!is.null(metadata) && !is.na(metadata$title)) metadata$title else ""

    instrument <- if (!is.null(metadata)) {
      get_instrument_metadata(func_name, metadata)
    } else {
      get_instrument_metadata(func_name, list(instrument_metadata = NA))
    }

    instrument_key <- if (!is.null(instrument)) {
      paste(
        c(
          if (!is.null(instrument$manufacturer)) instrument$manufacturer else "",
          if (!is.null(instrument$code)) instrument$code else "",
          if (!is.null(instrument$name)) instrument$name else "",
          if (!is.null(instrument$link)) instrument$link else ""
        ),
        collapse = "|"
      )
    } else {
      ""
    }

    inventory[[func_name]] <- list(
      function_name = func_name,
      args = args,
      usage = usage,
      title = title,
      has_metadata = !is.null(metadata),
      instrument = instrument,
      signature_key = paste(args, collapse = ","),
      metadata_key = paste(c(title, instrument_key), collapse = "||")
    )
  }

  inventory
}

load_previous_inventory <- function(snapshot_path) {
  if (!file.exists(snapshot_path)) return(list())

  previous_raw <- tryCatch(
    jsonlite::fromJSON(snapshot_path, simplifyVector = FALSE),
    error = function(e) list()
  )

  if (is.null(previous_raw) || length(previous_raw) == 0) return(list())

  # Backward-compatible shape handling
  if (!is.null(previous_raw$inventory) && is.list(previous_raw$inventory)) {
    return(previous_raw$inventory)
  }

  if (is.list(previous_raw) && !is.null(names(previous_raw))) {
    return(previous_raw)
  }

  list()
}

classify_import_functions <- function(current_inventory, previous_inventory, documented_funcs) {
  classifications <- list(
    already_documented_no_change = c(),
    missing_doc = c(),
    signature_changed = c(),
    metadata_changed = c(),
    metadata_incomplete = c()
  )

  for (func_name in names(current_inventory)) {
    current <- current_inventory[[func_name]]
    previous <- previous_inventory[[func_name]]
    is_documented <- func_name %in% documented_funcs

    if (!is_documented) {
      classifications$missing_doc <- c(classifications$missing_doc, func_name)
      if (is.null(current$instrument) || identical(current$metadata_key, "||")) {
        classifications$metadata_incomplete <- c(classifications$metadata_incomplete, func_name)
      }
      next
    }

    if (!is.null(previous)) {
      previous_sig <- if (!is.null(previous$signature_key)) previous$signature_key else ""
      previous_meta <- if (!is.null(previous$metadata_key)) previous$metadata_key else ""

      if (!identical(current$signature_key, previous_sig)) {
        classifications$signature_changed <- c(classifications$signature_changed, func_name)
      } else if (!identical(current$metadata_key, previous_meta)) {
        classifications$metadata_changed <- c(classifications$metadata_changed, func_name)
      } else {
        classifications$already_documented_no_change <- c(classifications$already_documented_no_change, func_name)
      }
    } else {
      classifications$already_documented_no_change <- c(classifications$already_documented_no_change, func_name)
    }
  }

  classifications
}

generate_import_delta_block <- function(func_name, inventory_entry, metadata_store) {
  metadata <- metadata_store[[func_name]]

  lines <- c(
    paste0("### ", func_name),
    "",
    if (!is.null(inventory_entry$instrument) && !is.null(inventory_entry$instrument$name)) {
      paste0("- Instrument: **", inventory_entry$instrument$name, "**")
    } else {
      "- Instrument: **Not specified in metadata**"
    },
    paste0("- Reference function: `", func_name, "()`"),
    ""
  )

  if (!is.null(metadata) && !is.na(metadata$title) && metadata$title != "") {
    lines <- c(lines, paste0("- Summary: ", metadata$title), "")
  }

  if (!is.null(metadata) && !is.na(metadata$usage) && metadata$usage != "") {
    lines <- c(lines, "#### Usage", "", "```r", metadata$usage, "```", "")
  }

  if (!is.null(metadata) && length(metadata$arguments) > 0) {
    lines <- c(lines, "#### Arguments detected", "")
    for (arg_name in names(metadata$arguments)) {
      lines <- c(lines, paste0("- `", arg_name, "`"))
    }
    lines <- c(lines, "")
  }

  paste(lines, collapse = "\n")
}

update_automanaged_region <- function(import_qmd_path, region_content) {
  start_marker <- "<!-- AUTOGEN:IMPORT-DELTA:START -->"
  end_marker <- "<!-- AUTOGEN:IMPORT-DELTA:END -->"

  if (!file.exists(import_qmd_path)) return(FALSE)

  lines <- readLines(import_qmd_path, warn = FALSE)
  start_idx <- which(trimws(lines) == start_marker)
  end_idx <- which(trimws(lines) == end_marker)

  if (length(start_idx) == 1 && length(end_idx) == 1 && end_idx > start_idx) {
    replacement <- c(
      start_marker,
      "",
      "## Auto-detected import updates",
      "",
      "This section is managed automatically from `import.*` functions in the package source.",
      "",
      unlist(strsplit(region_content, "\\n", fixed = FALSE)),
      "",
      end_marker
    )

    new_lines <- c(lines[1:(start_idx - 1)], replacement, lines[(end_idx + 1):length(lines)])
    writeLines(new_lines, import_qmd_path)
    return(TRUE)
  }

  # Fallback: append managed region if markers are missing
  fallback <- c(
    "",
    start_marker,
    "",
    "## Auto-detected import updates",
    "",
    "This section is managed automatically from `import.*` functions in the package source.",
    "",
    unlist(strsplit(region_content, "\\n", fixed = FALSE)),
    "",
    end_marker,
    ""
  )

  writeLines(c(lines, fallback), import_qmd_path)
  TRUE
}

import_qmd_path <- file.path(dirname(output_dir), "import.qmd")
import_funcs <- setdiff(categories$imports, "import2RData")
documented_import_funcs <- extract_documented_import_functions(import_qmd_path)

current_inventory <- build_import_inventory(import_funcs, all_metadata)
snapshot_path <- file.path(output_dir, "import_inventory_snapshot.json")
previous_inventory <- load_previous_inventory(snapshot_path)
classifications <- classify_import_functions(current_inventory, previous_inventory, documented_import_funcs)

delta_targets <- unique(c(
  classifications$missing_doc,
  classifications$signature_changed,
  classifications$metadata_changed
))

# NEW: Intelligent insertion of generated import sections (ONLY for missing/new instruments)
sync_updated <- FALSE
if (length(delta_targets) > 0) {
  # Extract prose patterns and learn manufacturer groupings
  prose_templates <- extract_prose_templates(import_qmd_path)
  manufacturer_groups <- learn_manufacturer_groups(import_qmd_path)
  
  # Load current import.qmd lines
  qmd_lines <- readLines(import_qmd_path, warn = FALSE)
  
  # Extract a template from a reference documented instrument
  # Use a well-documented instrument as the template for new ones
  reference_instrument <- "LI-6400"  # Use LI-6400 as the reference template
  instrument_template <- extract_instrument_section_template(import_qmd_path, reference_instrument)
  
  # Track sections to insert (only for NEWLY MISSING instruments, not updates to existing ones)
  sections_to_insert <- list()
  
  # Only insert new/missing instruments, not ones with signature/metadata changes
  # (those are already documented and should not be touched)
  for (func_name in sort(classifications$missing_doc)) {
    generated_section <- generate_intelligent_import_section(
      func_name,
      current_inventory[[func_name]],
      all_metadata,
      prose_templates,
      template = instrument_template
    )
    
    if (generated_section != "") {
      insertion_line <- find_insertion_point_for_import(
        func_name,
        manufacturer_groups,
        qmd_lines
      )
      
      sections_to_insert[[func_name]] <- list(
        content = generated_section,
        insertion_line = insertion_line,
        func_name = func_name
      )
    }
  }
  
  # Insert sections into qmd_lines (in descending line number order to maintain accuracy)
  # Group by insertion line and sort descending
  insertions_by_line <- list()
  for (func_name in names(sections_to_insert)) {
    section_info <- sections_to_insert[[func_name]]
    insertion_line <- section_info$insertion_line
    
    if (is.null(insertion_line)) {
      insertion_line <- length(qmd_lines)
    }
    
    line_key <- as.character(insertion_line)
    if (is.null(insertions_by_line[[line_key]])) {
      insertions_by_line[[line_key]] <- list()
    }
    insertions_by_line[[line_key]][[func_name]] <- section_info
  }
  
  # Sort insertion points in descending order (insert from bottom to top)
  insertion_lines_sorted <- sort(as.numeric(names(insertions_by_line)), decreasing = TRUE)
  
  # Insert all sections, working from bottom to top
  for (insertion_line in insertion_lines_sorted) {
    line_key <- as.character(insertion_line)
    funcs_at_line <- names(insertions_by_line[[line_key]])
    
    # Sort functions alphabetically at this insertion point (for consistent ordering)
    funcs_at_line <- sort(funcs_at_line)
    
    # Build all content to insert at this line (in alphabetical order)
    all_content_at_line <- c()
    for (func_name in funcs_at_line) {
      section_info <- insertions_by_line[[line_key]][[func_name]]
      section_lines <- strsplit(section_info$content, "\n")[[1]]
      
      if (length(all_content_at_line) > 0) {
        # Add blank line separator between sections at same insertion point
        all_content_at_line <- c(all_content_at_line, "")
      }
      all_content_at_line <- c(all_content_at_line, section_lines)
    }
    
    # Add padding and insert
    new_lines <- c("", all_content_at_line, "")
    qmd_lines <- c(
      qmd_lines[1:insertion_line],
      new_lines,
      qmd_lines[(insertion_line+1):length(qmd_lines)]
    )
  }
  
  # Write updated import.qmd
  writeLines(qmd_lines, import_qmd_path)
  sync_updated <- TRUE
  
  cat("✓ Generated", length(sections_to_insert), "import section(s) and updated import.qmd\n")
}

sync_report <- list(
  generated_at = as.character(Sys.time()),
  import_function_count = length(import_funcs),
  documented_import_function_count = length(intersect(import_funcs, documented_import_funcs)),
  classifications = classifications,
  unresolved_missing_import_docs = length(intersect(classifications$missing_doc, classifications$metadata_incomplete)),
  delta_targets = sort(delta_targets)
)

writeLines(
  jsonlite::toJSON(sync_report, pretty = TRUE, auto_unbox = TRUE),
  file.path(output_dir, "import_sync_report.json")
)

writeLines(
  jsonlite::toJSON(
    list(generated_at = as.character(Sys.time()), inventory = current_inventory),
    pretty = TRUE,
    auto_unbox = TRUE
  ),
  snapshot_path
)

cat("Import sync report generated: import_sync_report.json\n")
cat("Missing docs:", length(classifications$missing_doc), "| Signature changed:",
    length(classifications$signature_changed), "| Metadata changed:",
    length(classifications$metadata_changed), "\n")
cat("Import page auto-managed region updated:", sync_updated, "\n")

# ==============================================================================
# STEP 11: Incremental core/analysis function documentation sync
# ==============================================================================
# Automatically detect and sync new core/analysis/wrapper functions in other.qmd
# Uses same snapshot-based change detection as imports

# Extract documented functions from other.qmd
extract_documented_functions_from_qmd <- function(qmd_path) {
  if (!file.exists(qmd_path)) return(c())
  
  lines <- readLines(qmd_path, warn = FALSE)
  
  # Match level 3 headers containing function names
  # Patterns: ### `function_name` for description, or ### function_name for description
  documented <- c()
  
  for (line in lines) {
    # Match ### followed by function name (with or without backticks)
    if (grepl("^###\\s+", line, perl = TRUE)) {
      # Extract function name using capture groups
      
      # First try: backtick-enclosed function name ### `func_name`
      match <- regexpr("^###\\s+`([a-zA-Z][a-zA-Z0-9._]*)`", line, perl = TRUE)
      if (match[1] > 0) {
        func_name <- sub("^###\\s+`([a-zA-Z][a-zA-Z0-9._]*)`.*$", "\\1", line, perl = TRUE)
        documented <- c(documented, func_name)
        next
      }
      
      # Second try: function name without backticks 
      match <- regexpr("^###\\s+([a-zA-Z][a-zA-Z0-9._]*)\\b", line, perl = TRUE)
      if (match[1] > 0) {
        func_name <- sub("^###\\s+([a-zA-Z][a-zA-Z0-9._]*)\\b.*$", "\\1", line, perl = TRUE)
        if (!grepl("^(the|and|or|for|with|in|on|at)$", func_name, ignore.case = TRUE)) {
          documented <- c(documented, func_name)
        }
      }
    }
  }
  
  # Debug: show what we're returning
  if (Sys.getenv("GOFLUX_DEBUG") == "1") {
    cat("[DEBUG extract_documented_functions_from_qmd] Found", length(documented), "functions:\n")
    for (f in documented) {
      cat("  - '", f, "' (", nchar(f), " chars)\n", sep="")
    }
  }
  
  return(unique(documented))
}

# Build inventory of core/analysis/wrapper functions
build_function_inventory_for_section <- function(func_names, category, metadata_store) {
  inventory <- list()
  
  for (func_name in func_names) {
    if (!func_name %in% names(metadata_store)) {
      next
    }
    
    metadata <- metadata_store[[func_name]]
    
    # Create signature key from arguments (order-independent)
    arg_names <- sort(names(metadata$arguments))
    signature_key <- paste(arg_names, collapse = "|")
    
    # Create metadata key from title/description
    metadata_key <- paste(
      trimws(metadata$title %||% ""),
      trimws(metadata$description %||% ""),
      collapse = "|"
    )
    
    inventory[[func_name]] <- list(
      name = func_name,
      category = category,
      title = metadata$title %||% NA,
      description = metadata$description %||% NA,
      usage = metadata$usage %||% NA,
      arguments = metadata$arguments %||% list(),
      details = metadata$details %||% NA,
      examples = metadata$examples %||% NA,
      signature_key = signature_key,
      metadata_key = metadata_key
    )
  }
  
  return(inventory)
}

# Classify core/analysis functions based on change detection
classify_core_analysis_functions <- function(current_inventory, previous_inventory, documented_funcs) {
  classifications <- list(
    already_documented_no_change = c(),
    missing_doc = c(),
    signature_changed = c(),
    metadata_changed = c(),
    metadata_incomplete = c()
  )
  
  if (length(previous_inventory) == 0) {
    # First time: all functions not documented are missing
    for (func_name in names(current_inventory)) {
      if (func_name %in% documented_funcs) {
        classifications$already_documented_no_change <- c(
          classifications$already_documented_no_change, func_name
        )
      } else {
        classifications$missing_doc <- c(classifications$missing_doc, func_name)
      }
    }
  } else {
    # Subsequent runs: compare snapshots
    for (func_name in names(current_inventory)) {
      if (func_name %in% documented_funcs) {
        # Already documented: check for changes
        if (func_name %in% names(previous_inventory)) {
          prev_entry <- previous_inventory[[func_name]]
          curr_entry <- current_inventory[[func_name]]
          
          if (curr_entry$signature_key != prev_entry$signature_key) {
            # Arguments changed (added/removed/reordered)
            classifications$signature_changed <- c(
              classifications$signature_changed, func_name
            )
          } else if (curr_entry$metadata_key != prev_entry$metadata_key) {
            # Title/description changed
            classifications$metadata_changed <- c(
              classifications$metadata_changed, func_name
            )
          } else {
            # No changes
            classifications$already_documented_no_change <- c(
              classifications$already_documented_no_change, func_name
            )
          }
        } else {
          # Previously unknown but now documented
          classifications$already_documented_no_change <- c(
            classifications$already_documented_no_change, func_name
          )
        }
      } else {
        # Not documented yet
        classifications$missing_doc <- c(classifications$missing_doc, func_name)
      }
    }
  }
  
  return(classifications)
}

# Generate function documentation section for other.qmd
generate_function_section_for_qmd <- function(func_name, inventory_entry, metadata_store) {
  metadata <- metadata_store[[func_name]]
  if (is.null(metadata)) return("")
  
  lines <- c()
  
  # Section header
  lines <- c(lines, paste0("### `", func_name, "`"))
  lines <- c(lines, "")
  
  # Brief description
  if (!is.na(metadata$title) && metadata$title != "") {
    lines <- c(lines, metadata$title)
    lines <- c(lines, "")
  }
  
  # Usage section
  lines <- c(lines, "#### Usage")
  lines <- c(lines, "")
  lines <- c(lines, "```{r}")
  lines <- c(lines, "#| eval: false")
  lines <- c(lines, "#| code-copy: false")
  
  if (!is.na(metadata$usage) && metadata$usage != "") {
    usage_lines <- strsplit(metadata$usage, "\n")[[1]]
    lines <- c(lines, usage_lines)
  }
  
  lines <- c(lines, "```")
  lines <- c(lines, "")
  
  # Arguments section
  if (length(metadata$arguments) > 0) {
    lines <- c(lines, "#### Arguments")
    lines <- c(lines, "")
    lines <- c(lines, "| Parameter | Description |")
    lines <- c(lines, "|:----------|:--------------------------|")
    
    for (arg_name in names(metadata$arguments)) {
      arg_desc <- metadata$arguments[[arg_name]]
      arg_desc <- clean_arg_description(arg_desc)
      arg_desc <- gsub("|", "\\|", arg_desc, fixed = TRUE)
      lines <- c(lines, paste0("| `", arg_name, "` | ", arg_desc, " |"))
    }
    lines <- c(lines, "")
  }
  
  # Details section
  if (!is.na(metadata$details) && metadata$details != "") {
    lines <- c(lines, "#### Details")
    lines <- c(lines, "")
    details_md <- convert_rd_latex_to_markdown(metadata$details)
    lines <- c(lines, details_md)
    lines <- c(lines, "")
  }
  
  # Examples section
  if (!is.na(metadata$examples) && metadata$examples != "") {
    lines <- c(lines, "#### Example")
    lines <- c(lines, "")
    lines <- c(lines, "```{r}")
    lines <- c(lines, "#| eval: false")
    
    example_lines <- strsplit(metadata$examples, "\n")[[1]]
    lines <- c(lines, example_lines)
    
    lines <- c(lines, "```")
    lines <- c(lines, "")
  }
  
  paste(lines, collapse = "\n")
}

# Find insertion point for a function within its section (alphabetically)
find_insertion_point_for_function <- function(func_name, category, qmd_lines) {
  # Find the section header for this category
  section_pattern <- switch(category,
    core = "^##\\s+Core",
    analysis = "^##\\s+Analysis",
    wrapper = "^##\\s+Wrapper",
    "^##\\s+Other"
  )
  
  section_indices <- grep(section_pattern, qmd_lines, perl = TRUE, ignore.case = TRUE)
  if (length(section_indices) == 0) {
    return(NULL)  # Section not found
  }
  
  section_start <- section_indices[1]
  
  # Find the end of this section (next level 2 header or end of file)
  section_end <- length(qmd_lines)
  next_sections <- grep("^##\\s+", qmd_lines[(section_start + 1):length(qmd_lines)], perl = TRUE)
  if (length(next_sections) > 0) {
    section_end <- section_start + next_sections[1] - 1
  }
  
  # Extract all level 3 headers (function headers) in this section
  section_lines <- qmd_lines[section_start:section_end]
  func_headers <- grep("^###\\s+", section_lines, perl = TRUE)
  
  if (length(func_headers) == 0) {
    # No functions in section yet, insert after section header
    return(section_start)
  }
  
  # Extract function names and their positions
  existing_funcs <- list()
  for (idx in func_headers) {
    header <- section_lines[idx]
    # Extract function name from header (between backticks or after ###)
    func_match <- gregexpr("`?([a-zA-Z][a-zA-Z0-9._]*)`?", header)
    if (func_match[[1]][1] > 0) {
      matches <- regmatches(header, func_match)[[1]]
      if (length(matches) > 0) {
        extracted_name <- sub("`", "", matches[1])
        existing_funcs[[length(existing_funcs) + 1]] <- list(
          name = extracted_name,
          line = section_start + idx - 1
        )
      }
    }
  }
  
  # Find alphabetical position
  func_sort_key <- tolower(func_name)
  insertion_point <- section_end
  
  for (existing in existing_funcs) {
    existing_sort_key <- tolower(existing$name)
    if (func_sort_key < existing_sort_key) {
      insertion_point <- existing$line - 1
      break
    }
  }
  
  return(insertion_point)
}

# Main STEP 11 sync logic
other_qmd_path <- file.path(dirname(output_dir), "other.qmd")
other_snapshot_path <- file.path(output_dir, "function_inventory_other_snapshot.json")

# Extract documented functions currently in other.qmd
documented_core_analysis <- extract_documented_functions_from_qmd(other_qmd_path)

# Build inventories for each function category
core_funcs <- categories$core
analysis_funcs <- categories$analysis
wrapper_funcs <- categories$wrapper

core_inventory <- build_function_inventory_for_section(core_funcs, "core", all_metadata)
analysis_inventory <- build_function_inventory_for_section(analysis_funcs, "analysis", all_metadata)
wrapper_inventory <- build_function_inventory_for_section(wrapper_funcs, "wrapper", all_metadata)

# Load previous snapshot if it exists
previous_other_inventory <- load_previous_inventory(other_snapshot_path)

# Classify functions based on change detection
core_classifications <- classify_core_analysis_functions(
  core_inventory, previous_other_inventory$core %||% list(), documented_core_analysis
)
analysis_classifications <- classify_core_analysis_functions(
  analysis_inventory, previous_other_inventory$analysis %||% list(), documented_core_analysis
)
wrapper_classifications <- classify_core_analysis_functions(
  wrapper_inventory, previous_other_inventory$wrapper %||% list(), documented_core_analysis
)

# Only auto-insert missing documentation (never touch existing)
missing_core <- core_classifications$missing_doc
missing_analysis <- analysis_classifications$missing_doc
missing_wrapper <- wrapper_classifications$missing_doc

other_sync_updated <- FALSE

# Process core/analysis/wrapper functions only if new ones need documentation
if (length(missing_core) > 0 || length(missing_analysis) > 0 || length(missing_wrapper) > 0) {
  if (file.exists(other_qmd_path)) {
    other_lines <- readLines(other_qmd_path, warn = FALSE)
    sections_to_insert <- list()
    
    # Generate sections for missing core functions
    for (func_name in sort(missing_core)) {
      generated_section <- generate_function_section_for_qmd(
        func_name, core_inventory[[func_name]], all_metadata
      )
      
      if (generated_section != "") {
        insertion_line <- find_insertion_point_for_function(func_name, "core", other_lines)
        
        if (!is.null(insertion_line)) {
          sections_to_insert[[func_name]] <- list(
            content = generated_section,
            insertion_line = insertion_line,
            func_name = func_name
          )
        }
      }
    }
    
    # Generate sections for missing analysis functions
    for (func_name in sort(missing_analysis)) {
      generated_section <- generate_function_section_for_qmd(
        func_name, analysis_inventory[[func_name]], all_metadata
      )
      
      if (generated_section != "") {
        insertion_line <- find_insertion_point_for_function(func_name, "analysis", other_lines)
        
        if (!is.null(insertion_line)) {
          sections_to_insert[[func_name]] <- list(
            content = generated_section,
            insertion_line = insertion_line,
            func_name = func_name
          )
        }
      }
    }
    
    # Generate sections for missing wrapper functions
    for (func_name in sort(missing_wrapper)) {
      generated_section <- generate_function_section_for_qmd(
        func_name, wrapper_inventory[[func_name]], all_metadata
      )
      
      if (generated_section != "") {
        insertion_line <- find_insertion_point_for_function(func_name, "wrapper", other_lines)
        
        if (!is.null(insertion_line)) {
          sections_to_insert[[func_name]] <- list(
            content = generated_section,
            insertion_line = insertion_line,
            func_name = func_name
          )
        }
      }
    }
    
    # Insert sections in descending line order to maintain accuracy
    if (length(sections_to_insert) > 0) {
      # Sort by insertion line (descending)
      line_order <- order(
        sapply(sections_to_insert, function(x) x$insertion_line),
        decreasing = TRUE
      )
      
      for (idx in line_order) {
        section <- sections_to_insert[[idx]]
        insertion_point <- section$insertion_line
        
        # Split content into lines and insert
        content_lines <- strsplit(section$content, "\n")[[1]]
        other_lines <- c(
          other_lines[1:insertion_point],
          content_lines,
          if (insertion_point < length(other_lines)) other_lines[(insertion_point + 1):length(other_lines)] else c()
        )
      }
      
      # Write updated other.qmd
      writeLines(other_lines, other_qmd_path)
      other_sync_updated <- TRUE
      
      if (Sys.getenv("GOFLUX_DEBUG") == "1") {
        cat("Updated other.qmd with", length(sections_to_insert), "new function sections\n")
      }
    }
  }
}

# Generate and save report for signature/metadata changes (for developer review)
function_sync_report <- list(
  generated_at = as.character(Sys.time()),
  core = list(
    documented = length(intersect(core_funcs, documented_core_analysis)),
    missing_doc = missing_core,
    signature_changed = core_classifications$signature_changed,
    metadata_changed = core_classifications$metadata_changed
  ),
  analysis = list(
    documented = length(intersect(analysis_funcs, documented_core_analysis)),
    missing_doc = missing_analysis,
    signature_changed = analysis_classifications$signature_changed,
    metadata_changed = analysis_classifications$metadata_changed
  ),
  wrapper = list(
    documented = length(intersect(wrapper_funcs, documented_core_analysis)),
    missing_doc = missing_wrapper,
    signature_changed = wrapper_classifications$signature_changed,
    metadata_changed = wrapper_classifications$metadata_changed
  ),
  other_qmd_updated = other_sync_updated
)

# Save current inventory snapshot for next run
writeLines(
  jsonlite::toJSON(
    list(
      generated_at = as.character(Sys.time()),
      core = core_inventory,
      analysis = analysis_inventory,
      wrapper = wrapper_inventory
    ),
    pretty = TRUE,
    auto_unbox = TRUE
  ),
  other_snapshot_path
)

writeLines(
  jsonlite::toJSON(function_sync_report, pretty = TRUE, auto_unbox = TRUE),
  file.path(output_dir, "function_sync_report.json")
)

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
    lines <- c(lines, "| Parameter | Description |")
    lines <- c(lines, "|:----------|:--------------------------|")
    
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
# STEP 11: Auto-sync core and analysis functions to other.qmd
# ==============================================================================

cat("\n=== Syncing core/analysis functions to other.qmd ===\n")

extract_documented_functions_from_qmd <- function(qmd_path) {
  # Extract functions documented in a .qmd file by parsing section headers
  if (!file.exists(qmd_path)) return(character(0))
  
  lines <- readLines(qmd_path, warn = FALSE)
  
  documented <- c()
  for (line in lines) {
    # Look for ### `function_name` headers
    if (grepl("^###\\s+`", line)) {
      # Extract function name from backticks: ### `function_name` ...
      func_name <- gsub("^###\\s+`([a-zA-Z._][a-zA-Z0-9._]*)`.*", "\\1", line)
      
      # Validate extraction succeeded (text changed) and it's not a false match
      if (func_name != line &&  # Must have extracted something different from original
          nchar(func_name) > 0 &&
          !grepl("^(Details|Arguments|Usage|Value|Examples?|Tip|Note|Error|Warning|Results|See|References)", func_name)) {
        documented <- c(documented, func_name)
      }
    }
  }
  
  unique(documented)
}

build_function_inventory_for_section <- function(func_names, category, metadata_store) {
  # Build inventory for core/analysis functions
  inventory <- list()
  
  for (func_name in sort(func_names)) {
    metadata <- if (func_name %in% names(metadata_store)) metadata_store[[func_name]] else NULL
    
    args <- if (!is.null(metadata) && length(metadata$arguments) > 0) {
      names(metadata$arguments)
    } else {
      character(0)
    }
    
    usage <- if (!is.null(metadata) && !is.na(metadata$usage)) metadata$usage else ""
    title <- if (!is.null(metadata) && !is.na(metadata$title)) metadata$title else ""
    
    inventory[[func_name]] <- list(
      function_name = func_name,
      category = category,
      args = args,
      usage = usage,
      title = title,
      has_metadata = !is.null(metadata),
      signature_key = paste(args, collapse = ","),
      metadata_key = title
    )
  }
  
  inventory
}

classify_core_analysis_functions <- function(current_inventory, previous_inventory, documented_funcs) {
  # Similar to import classification
  classifications <- list(
    already_documented_no_change = c(),
    missing_doc = c(),
    signature_changed = c(),
    metadata_changed = c(),
    metadata_incomplete = c()
  )
  
  for (func_name in names(current_inventory)) {
    current <- current_inventory[[func_name]]
    previous <- previous_inventory[[func_name]]
    is_documented <- func_name %in% documented_funcs
    
    if (!is_documented) {
      classifications$missing_doc <- c(classifications$missing_doc, func_name)
      if (is.null(current) || identical(current$metadata_key, "")) {
        classifications$metadata_incomplete <- c(classifications$metadata_incomplete, func_name)
      }
      next
    }
    
    if (!is.null(previous)) {
      previous_sig <- if (!is.null(previous$signature_key)) previous$signature_key else ""
      previous_meta <- if (!is.null(previous$metadata_key)) previous$metadata_key else ""
      
      if (!identical(current$signature_key, previous_sig)) {
        classifications$signature_changed <- c(classifications$signature_changed, func_name)
      } else if (!identical(current$metadata_key, previous_meta)) {
        classifications$metadata_changed <- c(classifications$metadata_changed, func_name)
      } else {
        classifications$already_documented_no_change <- c(classifications$already_documented_no_change, func_name)
      }
    } else {
      classifications$already_documented_no_change <- c(classifications$already_documented_no_change, func_name)
    }
  }
  
  classifications
}

generate_function_section_for_qmd <- function(func_name, inventory_entry, metadata_store) {
  # Generate a section for a function to be inserted into other.qmd
  metadata <- metadata_store[[func_name]]
  if (is.null(metadata)) return("")
  
  lines <- c()
  
  # Section header (matching other.qmd style)
  backtick_name <- paste0("`", func_name, "`")
  header_text <- if (!is.na(metadata$title) && metadata$title != "") {
    paste0(backtick_name, " for ", tolower(substr(metadata$title, 1, 1)), substr(metadata$title, 2, nchar(metadata$title)))
  } else {
    backtick_name
  }
  
  lines <- c(lines, paste0("### ", header_text))
  lines <- c(lines, "")
  
  # Short description from title
  if (!is.na(metadata$title) && metadata$title != "") {
    lines <- c(lines, metadata$title)
    lines <- c(lines, "")
  }
  
  # Usage section
  lines <- c(lines, "#### Usage")
  lines <- c(lines, "")
  lines <- c(lines, "```{r}")
  lines <- c(lines, "#| eval: false")
  lines <- c(lines, "#| code-copy: false")
  
  if (!is.na(metadata$usage) && metadata$usage != "") {
    usage_lines <- strsplit(metadata$usage, "\n")[[1]]
    lines <- c(lines, usage_lines)
  } else {
    lines <- c(lines, paste0(func_name, "("))
    lines <- c(lines, "  # arguments here")
    lines <- c(lines, ")")
  }
  
  lines <- c(lines, "```")
  lines <- c(lines, "")
  
  # Arguments section
  if (length(metadata$arguments) > 0) {
    lines <- c(lines, "#### Arguments")
    lines <- c(lines, "")
    lines <- c(lines, "| Parameter | Description |")
    lines <- c(lines, "|:----------|:---------------------------|")
    
    for (arg_name in names(metadata$arguments)) {
      arg_desc <- metadata$arguments[[arg_name]]
      arg_desc <- clean_arg_description(arg_desc)
      arg_desc <- gsub("|", "\\|", arg_desc, fixed = TRUE)
      
      lines <- c(lines, paste0("| `", arg_name, "` | ", arg_desc, " |"))
    }
    lines <- c(lines, "")
  }
  
  # Details section
  if (!is.na(metadata$details) && metadata$details != "") {
    lines <- c(lines, "#### Details")
    lines <- c(lines, "")
    details_md <- convert_rd_latex_to_markdown(metadata$details)
    lines <- c(lines, details_md)
    lines <- c(lines, "")
  }
  
  # Examples section
  if (!is.na(metadata$examples) && metadata$examples != "") {
    lines <- c(lines, "#### Example")
    lines <- c(lines, "")
    lines <- c(lines, "```{r}")
    lines <- c(lines, "#| eval: false")
    
    example_lines <- strsplit(metadata$examples, "\n")[[1]]
    lines <- c(lines, example_lines)
    
    lines <- c(lines, "```")
    lines <- c(lines, "")
  }
  
  paste(lines, collapse = "\n")
}

find_insertion_point_for_function <- function(func_name, category, qmd_lines) {
  # Find alphabetical insertion point for a function in a section
  
  # Look for section header matching the category
  section_patterns <- list(
    core = "^##\\s+Core",
    analysis = "^##\\s+Analysis",
    wrapper = "^##\\s+Wrapper"
  )
  
  section_pattern <- if (!is.null(section_patterns[[category]])) {
    section_patterns[[category]]
  } else {
    "^##\\s"  # Any level 2 header
  }
  
  # Find the section start
  section_start <- NA
  for (i in seq_along(qmd_lines)) {
    if (grepl(section_pattern, qmd_lines[i], perl = TRUE)) {
      section_start <- i
      break
    }
  }
  
  if (is.na(section_start)) {
    return(NULL)  # Section not found, append at end
  }
  
  # Extract all function headers (###) after section start
  func_headers <- c()
  for (i in (section_start + 1):length(qmd_lines)) {
    # Stop at next level 2 header
    if (i > section_start + 1 && grepl("^##\\s", qmd_lines[i])) {
      break
    }
    
    # Match function headers (### format)
    if (grepl("^###\\s+`?([a-zA-Z._][a-zA-Z0-9._]*)", qmd_lines[i])) {
      func_name_in_header <- gsub("^###\\s+`?([a-zA-Z._][a-zA-Z0-9._]*)", "\\1", qmd_lines[i])
      func_headers[[length(func_headers) + 1]] <- list(
        line = i,
        name = func_name_in_header
      )
    }
  }
  
  # Find alphabetically correct position
  func_sort_key <- tolower(func_name)
  
  for (header in func_headers) {
    if (tolower(header$name) > func_sort_key) {
      # Insert before this header
      return(header$line - 1)
    }
  }
  
  # func_name comes after all existing ones, insert after last header
  if (length(func_headers) > 0) {
    return(func_headers[[length(func_headers)]]$line)
  }
  
  # No headers found, insert after section start
  return(section_start + 1)
}

# ==============================================================================
# STEP 12: Generalized Multi-Section Auto-Sync Framework
# ==============================================================================
# Extends STEP 11 pattern to other .qmd files (goflux.qmd, bestflux.qmd, etc.)
# Uses same snapshot-based change detection for consistent behavior across all sections

# Configuration for all auto-managed .qmd documentation sections
# Maps source .R files to their corresponding .qmd files for auto-documentation
qmd_section_config <- list(
  # goflux.qmd: Documents goFlux.R and its argument changes
  goflux_section = list(
    qmd_file = "goflux.qmd",
    snapshot_file = "function_inventory_goflux_snapshot.json",
    section_header_patterns = list(
      arguments = "^###\\s+Arguments"  # goFlux function arguments
    ),
    categories = c("from_goFlux.R"),
    description = "goFlux main function and argument documentation"
  ),
  
  # bestflux.qmd: Documents best.flux.R and its argument changes
  bestflux_section = list(
    qmd_file = "bestflux.qmd",
    snapshot_file = "function_inventory_bestflux_snapshot.json",
    section_header_patterns = list(
      best_selection = "^###\\s+`?best\\.flux`"
    ),
    categories = c("from_best.flux.R"),
    description = "best.flux function and argument documentation"
  ),
  
  # flux2pdf.qmd: Documents flux2pdf.R and its argument changes
  flux2pdf_section = list(
    qmd_file = "flux2pdf.qmd",
    snapshot_file = "function_inventory_flux2pdf_snapshot.json",
    section_header_patterns = list(
      pdf_generation = "^###\\s+`?flux2pdf`"
    ),
    categories = c("from_flux2pdf.R"),
    description = "flux2pdf function and argument documentation"
  )
)

# Generic function to sync documentation for any .qmd section
sync_function_documentation_in_section <- function(
  config,
  all_functions,
  all_metadata,
  categories_list,
  output_dir
) {
  # output_dir is _generated, which is in quarto directory
  # config$qmd_file is just the filename (e.g., "goflux.qmd")
  qmd_path <- file.path(dirname(output_dir), config$qmd_file)
  snapshot_path <- file.path(output_dir, config$snapshot_file)
  
  # Check if .qmd file exists
  if (!file.exists(qmd_path)) {
    if (Sys.getenv("GOFLUX_DEBUG") == "1") {
      cat("[STEP 12] Skipping", config$qmd_file, "- file not found\n")
    }
    return(list(
      updated = FALSE,
      missing = c(),
      signature_changed = c(),
      metadata_changed = c()
    ))
  }
  
  # Extract documented functions from this .qmd
  documented <- extract_documented_functions_from_qmd(qmd_path)
  
  # Build inventories for this section's categories
  funcs_for_section <- c()
  for (cat in config$categories) {
    if (cat %in% names(categories_list)) {
      funcs_for_section <- c(funcs_for_section, categories_list[[cat]])
    }
  }
  
  if (length(funcs_for_section) == 0) {
    if (Sys.getenv("GOFLUX_DEBUG") == "1") {
      cat("[STEP 12]", config$qmd_file, "- no functions found for categories:", 
          paste(config$categories, collapse=", "), "\n")
    }
    return(list(
      updated = FALSE,
      missing = c(),
      signature_changed = c(),
      metadata_changed = c()
    ))
  }
  
  # Build inventory for these functions
  section_inventory <- build_function_inventory_for_section(
    funcs_for_section, 
    config$categories[1],  # Use first category as primary
    all_metadata
  )
  
  # Load previous snapshot
  previous_inventory <- load_previous_inventory(snapshot_path)
  
  # Classify changes
  classifications <- classify_core_analysis_functions(
    section_inventory,
    previous_inventory %||% list(),
    documented
  )
  
  # Only process missing documentation
  missing <- classifications$missing_doc
  
  # Generate and insert missing function sections
  section_updated <- FALSE
  if (length(missing) > 0) {
    qmd_lines <- readLines(qmd_path, warn = FALSE)
    sections_to_insert <- list()
    
    for (func_name in sort(missing)) {
      generated_section <- generate_function_section_for_qmd(
        func_name,
        section_inventory[[func_name]],
        all_metadata
      )
      
      if (generated_section != "") {
        # Find insertion point within appropriate section
        section_category <- config$categories[1]
        insertion_line <- find_insertion_point_for_function(
          func_name, section_category, qmd_lines
        )
        
        if (!is.null(insertion_line)) {
          sections_to_insert[[func_name]] <- list(
            content = generated_section,
            insertion_line = insertion_line
          )
        }
      }
    }
    
    # Insert all sections in descending line order
    if (length(sections_to_insert) > 0) {
      # Sort by line number (descending)
      line_order <- order(
        sapply(sections_to_insert, function(x) x$insertion_line),
        decreasing = TRUE
      )
      
      for (idx in line_order) {
        section <- sections_to_insert[[idx]]
        insertion_point <- section$insertion_line
        content_lines <- strsplit(section$content, "\n")[[1]]
        
        qmd_lines <- c(
          qmd_lines[1:insertion_point],
          content_lines,
          if (insertion_point < length(qmd_lines)) 
            qmd_lines[(insertion_point + 1):length(qmd_lines)] 
          else c()
        )
      }
      
      # Write updated .qmd
      writeLines(qmd_lines, qmd_path)
      section_updated <- TRUE
      
      if (Sys.getenv("GOFLUX_DEBUG") == "1") {
        cat("[STEP 12] Updated", config$qmd_file, "with", 
            length(sections_to_insert), "new sections\n")
      }
    }
  }
  
  # Save snapshot for next run
  writeLines(
    jsonlite::toJSON(
      list(
        generated_at = as.character(Sys.time()),
        inventory = section_inventory
      ),
      pretty = TRUE,
      auto_unbox = TRUE
    ),
    snapshot_path
  )
  
  return(list(
    updated = section_updated,
    missing = missing,
    signature_changed = classifications$signature_changed,
    metadata_changed = classifications$metadata_changed,
    documented_count = length(documented),
    total_count = length(funcs_for_section)
  ))
}

# Function categorization by source .R file
# Maps functions to the .qmd files where their documentation is maintained
extended_categories <- list(
  # goFlux.R: Main flux calculation function
  from_goFlux.R = c("goFlux"),
  
  # best.flux.R: Best flux selection function
  from_best.flux.R = c("best.flux"),
  
  # flux2pdf.R: PDF generation function
  from_flux2pdf.R = c("flux2pdf")
  
  # Note: Functions from other .R files documented in other.qmd are handled by STEP 11
  # core/analysis/wrapper categories in STEP 11 capture remainder
)

# Run STEP 12: Process all configured sections
if (Sys.getenv("GOFLUX_DEBUG") == "1") {
  cat("\n=== Running STEP 12: Generalized Multi-Section Sync ===\n")
}

step12_results <- list()

for (section_name in names(qmd_section_config)) {
  config <- qmd_section_config[[section_name]]
  
  result <- sync_function_documentation_in_section(
    config,
    all_functions = exported_functions,
    all_metadata = all_metadata,
    categories_list = extended_categories,
    output_dir = output_dir
  )
  
  step12_results[[section_name]] <- c(
    config = list(config),
    result = list(result)
  )
}

# Generate consolidated STEP 12 report
step12_report <- list(
  generated_at = as.character(Sys.time()),
  sections_processed = length(qmd_section_config),
  sections = step12_results
)

writeLines(
  jsonlite::toJSON(step12_report, pretty = TRUE, auto_unbox = TRUE),
  file.path(output_dir, "step12_sync_report.json")
)

# ==============================================================================
# COMPLETION

cat("\n✓ Documentation generation complete!\n")
cat("Generated files in:", output_dir, "\n")
cat("Files created:\n")
cat("  -", length(categories$imports), "import function references\n")
cat("  - function_index.qmd\n")
cat("  - instruments_matrix.qmd\n")
cat("  - function_metadata.RDS\n")
