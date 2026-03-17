#!/usr/bin/env Rscript

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  list(
    mode = if ("--full" %in% args) "full" else "changes-only",
    dry_run = "--dry-run" %in% args,
    validate_render = !("--no-render" %in% args)
  )
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}

md5_file <- function(path) {
  unname(tools::md5sum(path)[1])
}

strip_roxy_prefix <- function(x) {
  sub("^#' ?", "", x)
}

parse_tag_blocks <- function(roxy_lines) {
  blocks <- list()
  current_tag <- NULL
  current_lines <- character(0)

  flush <- function() {
    if (!is.null(current_tag)) {
      blocks[[length(blocks) + 1]] <<- list(tag = current_tag, lines = current_lines)
    }
  }

  for (line in roxy_lines) {
    if (grepl("^@[A-Za-z.]+", line)) {
      flush()
      tag <- sub("^@([A-Za-z.]+).*$", "\\1", line)
      rest <- sub("^@[A-Za-z.]+ ?", "", line)
      current_tag <- tag
      current_lines <- rest
    } else {
      if (is.null(current_tag)) next
      current_lines <- c(current_lines, line)
    }
  }
  flush()
  blocks
}

parse_params <- function(blocks) {
  params <- list()
  for (b in blocks) {
    if (b$tag != "param") next
    joined <- paste(b$lines, collapse = "\n")
    first <- b$lines[1] %||% ""
    name <- sub("^([A-Za-z0-9._,]+).*$", "\\1", first)
    desc_first <- sub("^[A-Za-z0-9._,]+ ?", "", first)
    desc_rest <- if (length(b$lines) > 1) b$lines[-1] else character(0)
    desc <- trimws(paste(c(desc_first, desc_rest), collapse = " "))
    params[[length(params) + 1]] <- list(
      name = name,
      description = gsub("\\s+", " ", desc),
      raw = joined
    )
  }
  params
}

parse_instrumentlink <- function(value) {
  if (is.null(value) || !nzchar(trimws(value))) return(NULL)
  parts <- trimws(strsplit(value, "\\|", perl = TRUE)[[1]])
  if (length(parts) == 3) {
    return(list(
      manufacturer = parts[1],
      code = parts[2],
      long_name = parts[3],
      url = "",
      valid = TRUE
    ))
  }
  if (length(parts) >= 4) {
    return(list(
      manufacturer = parts[1],
      code = parts[2],
      long_name = parts[3],
      url = paste(parts[4:length(parts)], collapse = "|"),
      valid = TRUE
    ))
  }
  list(raw = value, valid = FALSE)
}

extract_signatures <- function(file_path) {
  signatures <- list()
  exprs <- try(parse(file = file_path, keep.source = FALSE), silent = TRUE)
  if (inherits(exprs, "try-error")) return(signatures)

  for (e in as.list(exprs)) {
    if (!is.call(e)) next
    op <- as.character(e[[1]])
    if (!(op %in% c("<-", "="))) next
    lhs <- e[[2]]
    rhs <- e[[3]]
    if (!is.symbol(lhs) || !is.call(rhs)) next
    if (as.character(rhs[[1]]) != "function") next

    fname <- as.character(lhs)
    form <- rhs[[2]]
    f_names <- names(as.list(form))
    if (is.null(f_names)) f_names <- character(0)
    f_names <- f_names[nzchar(f_names)]
    signatures[[fname]] <- paste0(fname, "(", paste(f_names, collapse = ", "), ")")
  }
  signatures
}

extract_roxygen_entries <- function(file_path, signatures) {
  lines <- readLines(file_path, warn = FALSE)
  entries <- list()
  i <- 1

  while (i <= length(lines)) {
    if (!grepl("^#'", lines[i])) {
      i <- i + 1
      next
    }

    start <- i
    while (i <= length(lines) && grepl("^#'", lines[i])) i <- i + 1
    block <- lines[start:(i - 1)]

    j <- i
    while (j <= length(lines) && trimws(lines[j]) == "") j <- j + 1
    if (j > length(lines)) break

    fn_line <- lines[j]
    m <- regexec("^([A-Za-z][A-Za-z0-9._]*)\\s*<-\\s*function\\s*\\(", fn_line)
    hit <- regmatches(fn_line, m)[[1]]
    if (length(hit) < 2) next
    fname <- hit[2]

    roxy_clean <- strip_roxy_prefix(block)
    tag_blocks <- parse_tag_blocks(roxy_clean)
    params <- parse_params(tag_blocks)

    first_block <- function(tag_name) {
      idx <- which(vapply(tag_blocks, function(x) x$tag == tag_name, logical(1)))[1]
      if (is.na(idx)) return(NULL)
      tag_blocks[[idx]]
    }

    examples_block <- first_block("examples")
    details_block <- first_block("details")
    returns_block <- first_block("returns")
    if (is.null(returns_block)) returns_block <- first_block("return")
    instrumentlink_block <- first_block("instrumentlink")

    examples <- if (!is.null(examples_block)) paste(examples_block$lines, collapse = "\n") else ""
    details <- if (!is.null(details_block)) paste(details_block$lines, collapse = "\n") else ""
    returns <- if (!is.null(returns_block)) paste(returns_block$lines, collapse = "\n") else ""
    instrumentlink_raw <- if (!is.null(instrumentlink_block)) paste(instrumentlink_block$lines, collapse = " ") else ""

    entries[[length(entries) + 1]] <- list(
      function_name = fname,
      file_path = file_path,
      source_hash = md5_file(file_path),
      signature = signatures[[fname]] %||% paste0(fname, "(...)"),
      params = params,
      examples = examples,
      details = details,
      returns = returns,
      instrumentlink = parse_instrumentlink(instrumentlink_raw)
    )
  }

  entries
}

parse_rd_file <- function(file_path) {
  tryCatch({
    rd_obj <- tools::parse_Rd(file_path, fragment = FALSE)
    return(rd_obj)
  }, error = function(e) {
    return(NULL)
  })
}

flatten_rd_text <- function(x) {
  if (is.null(x)) return("")
  if (is.character(x)) return(paste(x, collapse = " "))
  if (is.list(x)) {
    result <- vapply(x, function(item) {
      if (is.null(attr(item, "Rd_tag"))) {
        as.character(item)
      } else {
        flatten_rd_text(item)
      }
    }, character(1))
    return(paste(result, collapse = " "))
  }
  as.character(x)
}

extract_rd_entries <- function(file_path) {
  rd <- parse_rd_file(file_path)
  if (is.null(rd)) return(list())
  
  entries <- list()
  
  fname_section <- NULL
  for (item in rd) {
    if (!is.null(attr(item, "Rd_tag"))) {
      tag <- attr(item, "Rd_tag")
      if (tag == "\\name") {
        fname_section <- flatten_rd_text(item)
        break
      }
    }
  }
  
  if (is.null(fname_section) || fname_section == "") return(list())
  
  params <- list()
  examples <- ""
  details <- ""
  returns <- ""
  
  for (item in rd) {
    if (is.null(attr(item, "Rd_tag"))) next
    tag <- attr(item, "Rd_tag")
    
    if (tag == "\\arguments") {
      if (is.list(item)) {
        for (arg_item in item) {
          if (is.null(attr(arg_item, "Rd_tag"))) next
          if (attr(arg_item, "Rd_tag") == "\\item") {
            if (is.list(arg_item) && length(arg_item) >= 2) {
              arg_name <- flatten_rd_text(arg_item[[1]])
              arg_desc <- flatten_rd_text(arg_item[[2]])
              arg_desc <- gsub("\\n+", " ", arg_desc)
              arg_desc <- gsub("\\s+", " ", arg_desc)
              arg_desc <- trimws(arg_desc)
              if (arg_name != "" && arg_desc != "") {
                params[[arg_name]] <- list(
                  name = arg_name,
                  description = arg_desc
                )
              }
            }
          }
        }
      }
    } else if (tag == "\\examples") {
      examples <- flatten_rd_text(item)
    } else if (tag == "\\details") {
      details <- flatten_rd_text(item)
    } else if (tag == "\\value") {
      returns <- flatten_rd_text(item)
    }
  }
  
  list(list(
    function_name = fname_section,
    file_path = file_path,
    source_hash = md5_file(file_path),
    params = params,
    examples = examples,
    details = details,
    returns = returns
  ))
}

target_qmd_for_function <- function(function_name) {
  if (function_name == "goFlux") return("goFlux.qmd")
  if (function_name == "best.flux") return("bestflux.qmd")
  if (function_name == "import2RData" || grepl("^import\\.", function_name)) return("import.qmd")
  "other.qmd"
}

metadata_from_repo <- function() {
  r_files <- list.files("../R", pattern = "\\.R$", full.names = TRUE)
  all_entries <- list()

  for (f in r_files) {
    sig <- extract_signatures(f)
    ents <- extract_roxygen_entries(f, sig)
    if (length(ents) > 0) {
      all_entries <- c(all_entries, ents)
    }
  }

  if (length(all_entries) == 0) {
    return(data.frame())
  }

  data.frame(
    function_name = vapply(all_entries, `[[`, character(1), "function_name"),
    file_path = vapply(all_entries, `[[`, character(1), "file_path"),
    source_hash = vapply(all_entries, `[[`, character(1), "source_hash"),
    signature = vapply(all_entries, `[[`, character(1), "signature"),
    qmd_target = vapply(all_entries, function(x) target_qmd_for_function(x$function_name), character(1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

load_snapshot <- function(path) {
  if (!file.exists(path)) return(data.frame())
  readRDS(path)
}

detect_changes <- function(current, previous) {
  if (nrow(current) == 0) return(current)
  if (nrow(previous) == 0) {
    current$action <- "ADD"
    return(current)
  }

  merged <- merge(
    current,
    previous[, c("function_name", "source_hash")],
    by = "function_name",
    all.x = TRUE,
    suffixes = c("_now", "_old")
  )

  merged$action <- ifelse(
    is.na(merged$source_hash_old),
    "ADD",
    ifelse(merged$source_hash_now != merged$source_hash_old, "UPDATE", "SKIP")
  )

  keep <- merged$action != "SKIP"
  out <- merged[keep, c("function_name", "file_path", "source_hash_now", "signature", "qmd_target", "action")]
  names(out)[names(out) == "source_hash_now"] <- "source_hash"
  out
}

escape_md_cell <- function(x) {
  # Escape markdown special characters
  x <- gsub("\\\\", "\\\\\\\\", x)  # Backslash first
  x <- gsub("\\|", "\\\\|", x)      # Pipe
  x <- gsub("\\*", "\\\\*", x)      # Asterisk
  x <- gsub("_", "\\\\_", x)        # Underscore
  x <- gsub("\\[", "\\\\[", x)      # Left bracket
  x <- gsub("\\]", "\\\\]", x)      # Right bracket
  x <- gsub("\\`", "\\\\`", x)      # Backtick
  # Normalize whitespace
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

build_usage_block <- function(signature) {
  c(
    "#### Usage",
    "",
    "```{r}",
    "#| eval: false",
    "#| code-copy: false",
    signature,
    "```",
    ""
  )
}

build_arguments_block <- function(params) {
  lines <- c(
    "#### Arguments",
    "",
    "| Parameter | Description |",
    "|:----------|:---------------------------|"
  )

  if (length(params) == 0) {
    lines <- c(lines, "| `-` | No parameters parsed from roxygen. |", "")
    return(lines)
  }

  for (p in params) {
    param_name <- if (is.list(p)) p$name else p
    param_desc <- if (is.list(p)) p$description else ""
    lines <- c(lines, paste0("| `", escape_md_cell(param_name), "` | ", escape_md_cell(param_desc), " |"))
  }
  c(lines, "")
}

build_example_block <- function(examples_text) {
  if (!nzchar(trimws(examples_text))) {
    return(c("#### Example", "", "```{r}", "#| eval: false", "# No examples provided in roxygen.", "```", ""))
  }
  ex_lines <- unlist(strsplit(examples_text, "\\n", fixed = FALSE), use.names = FALSE)
  c(
    "#### Example",
    "",
    "```{r}",
    "#| eval: false",
    ex_lines,
    "```",
    ""
  )
}

find_section_bounds <- function(lines, start_idx, heading_regex) {
  idx <- which(seq_along(lines) > start_idx & grepl(heading_regex, lines, perl = TRUE))
  if (length(idx) == 0) return(NULL)
  h_start <- idx[1]
  next_h <- which(seq_along(lines) > h_start & grepl("^#### ", lines))
  h_end <- if (length(next_h) == 0) length(lines) else next_h[1] - 1
  list(start = h_start, end = h_end)
}

replace_range <- function(lines, start, end, replacement) {
  c(lines[1:(start - 1)], replacement, lines[(end + 1):length(lines)])
}

update_function_qmd <- function(qmd_path, function_name, signature, params, examples, dry_run = FALSE) {
  if (!file.exists(qmd_path)) {
    message("[WARN] Missing qmd target: ", qmd_path)
    return(FALSE)
  }

  lines <- readLines(qmd_path, warn = FALSE)

  # Fix heading detection: look for function name at start of heading
  fn_heading_pattern <- paste0("^### [^#]*\\b", gsub("([.])", "\\\\\\1", function_name), "\\b")
  section_start_candidates <- which(grepl(fn_heading_pattern, lines, perl = TRUE, ignore.case = FALSE))
  if (length(section_start_candidates) == 0) {
    message("[WARN] Could not locate function heading in ", qmd_path, ": ", function_name)
    return(FALSE)
  }
  section_start <- section_start_candidates[1]

  next_section <- which(seq_along(lines) > section_start & grepl("^### ", lines))
  section_end <- if (length(next_section) == 0) length(lines) else next_section[1] - 1

  changed <- FALSE

  usage_bounds <- find_section_bounds(lines, section_start, "^#### Usage")
  if (!is.null(usage_bounds) && usage_bounds$start <= section_end) {
    usage_new <- build_usage_block(signature)
    lines <- replace_range(lines, usage_bounds$start, usage_bounds$end, usage_new)
    changed <- TRUE
    section_end <- section_end - (usage_bounds$end - usage_bounds$start + 1) + length(usage_new)
  }

  args_bounds <- find_section_bounds(lines, section_start, "^#### Arguments")
  if (!is.null(args_bounds) && args_bounds$start <= section_end) {
    args_new <- build_arguments_block(params)
    lines <- replace_range(lines, args_bounds$start, args_bounds$end, args_new)
    changed <- TRUE
    section_end <- section_end - (args_bounds$end - args_bounds$start + 1) + length(args_new)
  }

  example_bounds <- find_section_bounds(lines, section_start, "^#### Example[s]?")
  if (!is.null(example_bounds) && example_bounds$start <= section_end) {
    ex_new <- build_example_block(examples)
    lines <- replace_range(lines, example_bounds$start, example_bounds$end, ex_new)
    changed <- TRUE
  }

  if (changed && !dry_run) {
    writeLines(lines, qmd_path)
  }

  changed
}

extract_entry_details <- function(entry_row) {
  # Try roxygen first
  sig <- extract_signatures(entry_row$file_path)
  ents <- extract_roxygen_entries(entry_row$file_path, sig)
  idx <- which(vapply(ents, function(x) x$function_name == entry_row$function_name, logical(1)))[1]
  
  if (!is.na(idx)) {
    details <- ents[[idx]]
    
    # Check if params/examples are incomplete; try to merge with Rd data
    has_incomplete_params <- length(details$params) == 0
    has_incomplete_examples <- !nzchar(trimws(details$examples))
    
    if (has_incomplete_params || has_incomplete_examples) {
      # Look for corresponding Rd file
      rd_base <- sub("\\.R$", "", entry_row$file_path)
      rd_path <- file.path("../man", paste0(basename(rd_base), ".Rd"))
      
      if (file.exists(rd_path)) {
        rd_ents <- extract_rd_entries(rd_path)
        if (length(rd_ents) > 0 && !is.null(rd_ents[[1]])) {
          rd_data <- rd_ents[[1]]
          
          # Merge params from Rd if roxygen incomplete
          if (has_incomplete_params && length(rd_data$params) > 0) {
            details$params <- rd_data$params
          }
          
          # Merge examples from Rd if roxygen incomplete
          if (has_incomplete_examples && nzchar(trimws(rd_data$examples))) {
            details$examples <- rd_data$examples
          }
        }
      }
    }
    
    return(details)
  }
  
  return(NULL)
}

validate_render <- function() {
  message("[INFO] Validating quarto render...")
  result <- system("quarto render . --no-execute 2>&1", intern = TRUE)
  
  if (any(grepl("ERROR", result, ignore.case = TRUE))) {
    message("[ERROR] Quarto render validation failed:")
    message(paste(result, collapse = "\n"))
    return(FALSE)
  }
  
  message("[OK] Quarto render validation passed")
  return(TRUE)
}

build_import_function_section <- function(function_name, signature, params, examples) {
  # Build a function section for import.qmd's single-file import section
  # This is simpler than goFlux; just Usage/Arguments/Example
  c(
    "#### Usage",
    "",
    "```{r}",
    "#| eval: false",
    "#| code-copy: false",
    signature,
    "```",
    "",
    "#### Arguments",
    "",
    "| Parameter | Description |",
    "|:----------|:---------------------------|",
    if (length(params) == 0) {
      c("| `-` | No parameters parsed. |")
    } else {
      vapply(params, function(p) {
        param_name <- if (is.list(p)) p$name else p
        param_desc <- if (is.list(p)) p$description else ""
        paste0("| `", escape_md_cell(param_name), "` | ", escape_md_cell(param_desc), " |")
      }, character(1))
    },
    "",
    "#### Example",
    "",
    "```{r}",
    "#| eval: false",
    if (!nzchar(trimws(examples))) {
      "# No examples provided"
    } else {
      unlist(strsplit(examples, "\\n", fixed = FALSE), use.names = FALSE)
    },
    "```",
    ""
  )
}

get_instrumentlink_entries <- function() {
  # Extract all @instrumentlink entries from import.*.R files
  r_files <- list.files("../R", pattern = "^import\\.", full.names = TRUE)
  entries <- list()
  
  for (f in r_files) {
    func_name <- sub(".*/import", "import", sub("\\.R$", "", f))
    lines <- readLines(f, warn = FALSE)
    
    for (i in seq_along(lines)) {
      if (grepl("@instrumentlink", lines[i])) {
        # Extract the instrumentlink value
        raw <- sub("^#' *@instrumentlink *", "", lines[i])
        link_data <- parse_instrumentlink(raw)
        
        if (!is.null(link_data) && !is.null(link_data$valid) && link_data$valid) {
          entries[[func_name]] <- link_data
        }
        break
      }
    }
  }
  
  entries
}

group_by_manufacturer <- function(all_entries) {
  # Group import functions by manufacturer from instrumentlink
  groups <- list()
  
  for (func_name in names(all_entries)) {
    entry <- all_entries[[func_name]]
    mfg <- entry$manufacturer
    
    if (is.null(groups[[mfg]])) {
      groups[[mfg]] <- list()
    }
    
    groups[[mfg]][[func_name]] <- entry
  }
  
  groups
}

update_import_qmd <- function(qmd_path, all_details, dry_run = FALSE) {
  if (!file.exists(qmd_path)) {
    message("[WARN] Missing qmd target: ", qmd_path)
    return(FALSE)
  }
  
  lines <- readLines(qmd_path, warn = FALSE)
  changed <- FALSE
  
  # NOTE: import2RData wrapper documentation is NOT updated by this function.
  # It contains examples and is maintained manually.
  # Only single-file import functions (import.*) are updated.
  
  # Find the "# Single file import" section
  single_heading_idx <- which(grepl("^# Single file import\\b", lines))
  if (length(single_heading_idx) == 0) {
    message("[WARN] No 'Single file import' section found in ", qmd_path)
    return(FALSE)
  }
  
  single_section_start <- single_heading_idx[1]
  single_section_end <- length(lines)  # End of file or could be refined
  
  # For each import.* function, find and update its section
  for (func_name in names(all_details)) {
    if (func_name == "import2RData") next
    
    details <- all_details[[func_name]]
    
    # Build expected anchor from function name
    # Anchors can follow different patterns:
    # e.g., import.LI6400 -> sec-single-LICOR-LI6400 (with manufacturer prefix)
    # e.g., import.eosMX12 -> sec-single-eosmx12 (simple pattern)
    func_short <- sub("^import\\.", "", func_name)
    func_short_lower <- tolower(func_short)
    
    # Search for ### heading that contains this function name anchor
    # Pattern: ### ... {#sec-single-...}
    # First try simple pattern (no manufacturer), then try pattern endings
    func_pattern <- paste0("sec-single-", func_short_lower, "}")
    func_line_idx <- grep(func_pattern, lines, ignore.case = TRUE)
    
    if (length(func_line_idx) == 0) {
      # Try alternate pattern with dots
      func_pattern_escaped <- paste0("sec-single.*", func_short_lower, "}")
      func_line_idx <- grep(func_pattern_escaped, lines, ignore.case = TRUE)
    }
    
    if (length(func_line_idx) == 0) {
      # Anchor not found - section doesn't exist yet, skip it
      # (In Phase 3, we may want to create missing sections)
      next
    }
    
    section_start <- func_line_idx[1]
    
    # Find the end of this ### section (either next ### or ##)
    next_heading_idx <- which(seq_along(lines) > section_start & 
                               (grepl("^### ", lines) | grepl("^## ", lines)))
    section_end <- if (length(next_heading_idx) > 0) next_heading_idx[1] - 1 else single_section_end
    
    # Update Usage block
    usage_pattern <- "^#### Usage"
    usage_idx <- which(seq_along(lines) >= section_start & seq_along(lines) <= section_end & 
                       grepl(usage_pattern, lines))
    
    if (length(usage_idx) > 0) {
      usage_start <- usage_idx[1]
      # Find the end of Usage block (next #### or ###)
      next_block_idx <- which(seq_along(lines) > usage_start & seq_along(lines) <= section_end & 
                              grepl("^####? ", lines) & seq_along(lines) > usage_start)
      usage_end <- if (length(next_block_idx) > 0) next_block_idx[1] - 1 else section_end
      
      # Build new Usage block
      usage_new <- c("#### Usage", "", "```{r}", "#| eval: false", "#| code-copy: false", 
                     details$signature, "```", "")
      
      lines <- replace_range(lines, usage_start, usage_end, usage_new)
      changed <- TRUE
      
      # Recalculate section boundaries after replacement
      line_diff <- length(usage_new) - (usage_end - usage_start + 1)
      section_end <- section_end + line_diff
    }
    
    # Update Arguments block
    args_pattern <- "^#### Arguments"
    args_idx <- which(seq_along(lines) >= section_start & seq_along(lines) <= section_end & 
                      grepl(args_pattern, lines))
    
    if (length(args_idx) > 0) {
      args_start <- args_idx[1]
      # Find the end of Arguments block
      next_block_idx <- which(seq_along(lines) > args_start & seq_along(lines) <= section_end & 
                              grepl("^#### ", lines) & seq_along(lines) > args_start)
      args_end <- if (length(next_block_idx) > 0) next_block_idx[1] - 1 else section_end
      
      # Build new Arguments block
      args_new <- build_arguments_block(details$params)
      
      lines <- replace_range(lines, args_start, args_end, args_new)
      changed <- TRUE
      
      # Recalculate section boundaries after replacement
      line_diff <- length(args_new) - (args_end - args_start + 1)
      section_end <- section_end + line_diff
    }
    
    # Update Example block
    example_pattern <- "^#### Example"
    example_idx <- which(seq_along(lines) >= section_start & seq_along(lines) <= section_end & 
                         grepl(example_pattern, lines))
    
    if (length(example_idx) > 0) {
      example_start <- example_idx[1]
      # Find the end of Example block (could be end of section)
      next_block_idx <- which(seq_along(lines) > example_start & seq_along(lines) <= section_end & 
                              grepl("^#### ", lines) & seq_along(lines) > example_start)
      example_end <- if (length(next_block_idx) > 0) next_block_idx[1] - 1 else section_end
      
      # Build new Example block
      example_new <- build_example_block(details$examples)
      
      lines <- replace_range(lines, example_start, example_end, example_new)
      changed <- TRUE
    }
  }
  
  if (changed && !dry_run) {
    writeLines(lines, qmd_path)
  }
  
  changed
}

main <- function() {
  opts <- parse_args()
  ensure_dir("../.goflux_automation")
  snapshot_path <- "../.goflux_automation/snapshot.rds"

  message("=== goFlux documentation generator ===")
  message("Mode: ", opts$mode, if (opts$dry_run) " (dry-run)" else "")
  if (!opts$dry_run && opts$validate_render) {
    message("Render validation: ON")
  }

  current <- metadata_from_repo()
  previous <- load_snapshot(snapshot_path)
  changes <- detect_changes(current, previous)

  if (nrow(changes) == 0 && opts$mode == "changes-only") {
    message("No changes detected. Exiting.")
    quit(status = 0)
  }

  message("Functions queued: ", nrow(changes))

  touched <- character(0)
  import_details <- list()
  
  # First pass: collect import details for batch processing
  for (i in seq_len(nrow(changes))) {
    row <- changes[i, ]
    details <- extract_entry_details(row)
    if (is.null(details)) {
      message("[WARN] Could not re-extract metadata: ", row$function_name)
      next
    }
    
    target <- row$qmd_target
    
    # Collect import functions for later batch update
    if (target == "import.qmd" && grepl("^import", row$function_name)) {
      import_details[[row$function_name]] <- details
    }
  }
  
  # Second pass: update qmd files
  for (i in seq_len(nrow(changes))) {
    row <- changes[i, ]
    details <- extract_entry_details(row)
    if (is.null(details)) {
      next
    }

    target <- row$qmd_target
    if (target %in% c("goFlux.qmd", "bestflux.qmd")) {
      updated <- update_function_qmd(
        qmd_path = target,
        function_name = row$function_name,
        signature = details$signature,
        params = if (length(details$params) > 0) details$params else list(),
        examples = details$examples,
        dry_run = opts$dry_run
      )
      if (isTRUE(updated)) {
        touched <- unique(c(touched, target))
        message("[OK] Updated: ", row$function_name, " -> ", target)
      }
    } else if (target == "import.qmd") {
      # Will be handled in batch below
    } else {
      message("[INFO] Pending implementation for target ", target, " (", row$function_name, ")")
    }
  }
  
  # Batch update import.qmd if any import functions changed
  if (length(import_details) > 0) {
    updated <- update_import_qmd(
      qmd_path = "import.qmd",
      all_details = import_details,
      dry_run = opts$dry_run
    )
    if (isTRUE(updated)) {
      touched <- unique(c(touched, "import.qmd"))
      message("[OK] Updated: ", length(import_details), " import functions -> import.qmd")
    }
  }

  if (!opts$dry_run) {
    # Validate render if enabled
    if (opts$validate_render) {
      if (!validate_render()) {
        message("[ERROR] Quarto render failed. Snapshot NOT saved.")
        quit(status = 1)
      }
    }
    
    saveRDS(current, snapshot_path)
    message("Snapshot saved: ", snapshot_path)
  }

  if (length(touched) > 0) {
    message("Touched qmd files: ", paste(touched, collapse = ", "))
  }

  message("Done.")
}

main()
