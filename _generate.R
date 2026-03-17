#!/usr/bin/env Rscript

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  list(
    mode = if ("--full" %in% args) "full" else "changes-only",
    dry_run = "--dry-run" %in% args
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

target_qmd_for_function <- function(function_name) {
  if (function_name == "goFlux") return("quarto/goFlux.qmd")
  if (function_name == "best.flux") return("quarto/bestflux.qmd")
  if (function_name == "import2RData" || grepl("^import\\.", function_name)) return("quarto/import.qmd")
  "quarto/other.qmd"
}

metadata_from_repo <- function() {
  r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
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
  x <- gsub("\\|", "\\\\|", x)
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
    lines <- c(lines, paste0("| `", escape_md_cell(p$name), "` | ", escape_md_cell(p$description), " |"))
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

  fn_heading <- paste0("^### .*`", gsub("([.])", "\\\\\\1", function_name), "`")
  section_start_candidates <- which(grepl(fn_heading, lines, perl = TRUE))
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
  sig <- extract_signatures(entry_row$file_path)
  ents <- extract_roxygen_entries(entry_row$file_path, sig)
  idx <- which(vapply(ents, function(x) x$function_name == entry_row$function_name, logical(1)))[1]
  if (is.na(idx)) return(NULL)
  ents[[idx]]
}

main <- function() {
  opts <- parse_args()
  ensure_dir(".goflux_automation")
  snapshot_path <- ".goflux_automation/snapshot.rds"

  message("=== goFlux documentation generator ===")
  message("Mode: ", opts$mode, if (opts$dry_run) " (dry-run)" else "")

  current <- metadata_from_repo()
  previous <- load_snapshot(snapshot_path)
  changes <- detect_changes(current, previous)

  if (nrow(changes) == 0 && opts$mode == "changes-only") {
    message("No changes detected. Exiting.")
    quit(status = 0)
  }

  message("Functions queued: ", nrow(changes))

  touched <- character(0)
  for (i in seq_len(nrow(changes))) {
    row <- changes[i, ]
    details <- extract_entry_details(row)
    if (is.null(details)) {
      message("[WARN] Could not re-extract metadata: ", row$function_name)
      next
    }

    target <- row$qmd_target
    if (target %in% c("quarto/goFlux.qmd", "quarto/bestflux.qmd")) {
      updated <- update_function_qmd(
        qmd_path = target,
        function_name = row$function_name,
        signature = details$signature,
        params = details$params,
        examples = details$examples,
        dry_run = opts$dry_run
      )
      if (isTRUE(updated)) {
        touched <- unique(c(touched, target))
        message("[OK] Updated: ", row$function_name, " -> ", target)
      }
    } else {
      message("[INFO] Pending implementation for target ", target, " (", row$function_name, ")")
    }
  }

  if (!opts$dry_run) {
    saveRDS(current, snapshot_path)
    message("Snapshot saved: ", snapshot_path)
  }

  if (length(touched) > 0) {
    message("Touched qmd files: ", paste(touched, collapse = ", "))
  }

  message("Done.")
}

main()