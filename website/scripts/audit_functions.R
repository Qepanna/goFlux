#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# audit_functions.R
#
# Systematically check that every documented function in the package has its
# autodoc block on the correct website page.
#
# Run from the repo root:   Rscript website/scripts/audit_functions.R
# (or pass the repo root as the first argument)
#
# Base R only. Classification:
#   * specific name patterns -> their page (import.* -> import.qmd, ...)
#   * EVERYTHING else        -> other.qmd   (no manual categorization needed)
#   * ignore list            -> deliberately not shown on the site
#
# Reports: OK / MISSING (with suggested page) / MISPLACED / IGNORED / STALE.
# Exit code is non-zero when there is anything to fix (MISSING/MISPLACED/STALE).
# ---------------------------------------------------------------------------

# ---- 0. Locate repo root -----------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args)) args[1] else "."
if (!file.exists(file.path(root, "man")) &&
    file.exists(file.path("..", "man"))) root <- ".."

# ---- 1. Name-pattern rules -> page ------------------------------------------
# Longest / most specific first. Anything not matched defaults to "other.qmd".
page_rules <- c(
  "^import2RData$"    = "import.qmd",
  "^import\\."        = "import.qmd",
  "^autoID$"          = "manualID.qmd",
  "^obs\\.win$"       = "manualID.qmd",
  "^click\\.peak2$"   = "manualID.qmd",
  "^goFlux$"          = "goFlux.qmd",
  "^best\\.flux$"     = "bestflux.qmd",
  "^flux\\.plot$"     = "flux2pdf.qmd",
  "^flux2pdf$"        = "flux2pdf.qmd",
  "^crop\\.meas$"     = "other.qmd",
  "^auto\\.deadband$" = "other.qmd",
  "^iso\\.comp$"      = "other.qmd",
  "^goAquaFlux"          = "goAquaFlux.qmd",
  "^flux\\.plot\\.aqua$" = "goAquaFlux.qmd",
  "^find\\.bubbles$"     = "goAquaFlux.qmd"
)
default_page <- "other.qmd"

# ---- 2. Deliberately not documented as blocks --------------------------------
ignore_patterns <- c(
  "^imp\\.",              # example-data documentation, not functions
  "^manID\\.",
  # internal/conceptual helpers already explained in goFlux/bestflux narrative;
  # remove any line here if you DO want a block for it:
  "^HM\\.flux$", "^LM\\.flux$", "^MDF$", "^k\\.max$",
  "^flux\\.term$", "^g\\.factor$"
)

# ---- 3. Universe = every documented function (man/*.Rd) -----------------------
rd_names <- sub("\\.Rd$", "", basename(list.files(file.path(root, "man"),
                                                  pattern = "\\.Rd$")))

# ---- 4. What the site currently documents (autodoc() calls) -------------------
qmd_files  <- list.files(file.path(root, "website"), pattern = "\\.qmd$",
                         full.names = TRUE)
documented <- character()          # named: fn -> page
for (f in qmd_files) {
  txt  <- paste(readLines(f, warn = FALSE), collapse = "\n")
  hits <- regmatches(txt, gregexpr('autodoc\\("([^"]+)"', txt))[[1]]
  if (length(hits)) {
    fns <- sub('autodoc\\("([^"]+)"', "\\1", hits)
    documented[fns] <- basename(f)
  }
}

# ---- 5. Classify --------------------------------------------------------------
match_any <- function(x, pats) any(vapply(pats, grepl, logical(1), x = x))

page_of <- function(fn) {
  hit <- names(page_rules)[vapply(names(page_rules), grepl, logical(1), x = fn)]
  if (!length(hit)) return(default_page)
  unname(page_rules[hit[1]])
}

ok <- character(); missing <- character(); misplaced <- character(); ignored <- character()
for (fn in rd_names) {
  if (match_any(fn, ignore_patterns)) { ignored <- c(ignored, fn); next }
  expected <- page_of(fn)
  if (!(fn %in% names(documented)))      missing   <- c(missing, fn)
  else if (documented[[fn]] != expected) misplaced <- c(misplaced, fn)
  else                                   ok        <- c(ok, fn)
}

stale <- setdiff(names(documented), rd_names)

# --- Instrument links ------------------------------------------------------
# Parse @instrumentlink records from the R source of the import functions.
# Format:  Manufacturer|ID|Name|URL   (URL optional)
read_instrument_links <- function(root = ".") {
  files <- list.files(file.path(root, "R"), pattern = "^import.*\\.R$",
                      full.names = TRUE)
  recs <- unlist(lapply(files, function(f) {
    lines <- readLines(f, warn = FALSE)
    hits  <- regmatches(lines, regexpr("#'\\s*@instrumentlink\\s+(.+)", lines))
    hits  <- hits[nzchar(hits)]
    lapply(hits, function(h) {
      x <- strsplit(sub("#'\\s*@instrumentlink\\s+", "", h), "\\|")[[1]]
      data.frame(manufacturer = x[1], id = x[2], name = x[3],
                 url = if (length(x) > 3) x[4] else NA,
                 fn = paste0("import.", x[2]), stringsAsFactors = FALSE)
    })
  }), recursive = FALSE)
  do.call(rbind, recs)
}

# ---- 5b. Instrument links
# Instruments whose docs are NOT yet on import.qmd (i.e. new entries to add)
new_instruments <- function(root = ".") {
  links <- read_instrument_links(root)
  qmd   <- paste(readLines(file.path(root, "website", "import.qmd"), warn = FALSE),
                 collapse = "\n")
  has   <- vapply(links$fn, function(fn) grepl(paste0('autodoc\\("', fn, '"'), qmd),
                 logical(1))
  links[!has, , drop = FALSE]
}

# Render a list entry the way import.qmd formats its instrument bullets
format_instrument <- function(r) {
  nm <- if (!is.na(r$url)) sprintf("[%s](%s)", r$name, r$url) else r$name
  sprintf("**%s** — %s", r$manufacturer, nm)     # was "- **%s** — %s"
}

# ---- 6. Report ---------------------------------------------------------------
sec <- function(title, items) {
  cat("\n##", title, paste0("(", length(items), ")"), "\n")
  if (length(items)) cat(paste0("  - ", sort(items), collapse = "\n"), "\n")
}
suggest <- function(fn) sprintf("%s  ->  %s", fn, page_of(fn))

cat("# autodoc coverage audit\n")
cat(sprintf("Documented functions (man/): %d | autodoc blocks on site: %d\n",
            length(rd_names), length(documented)))

new_inst <- new_instruments(root)
sec("NEW INSTRUMENTS - in package, no entry on import.qmd; add:",
    vapply(seq_len(nrow(new_inst)), function(i) {
      format_instrument(new_inst[i, ])
    }, character(1)))           
     
sec("OK", ok)
sec("MISSING - no block; suggested page shown", vapply(missing, suggest, character(1)))
sec("MISPLACED - block on the wrong page", vapply(misplaced, suggest, character(1)))
sec("IGNORED - intentionally not documented", ignored)
sec("STALE - block for a function with no man/.Rd", stale)

problems <- length(missing) + length(misplaced) + length(stale)
cat(sprintf("\nProblems to fix: %d\n", problems))
quit(status = as.integer(problems > 0))