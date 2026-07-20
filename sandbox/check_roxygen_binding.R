#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# Guard against the "roxygen block binds to the wrong object" bug.
#
# roxygen2 attaches a #' block to the NEXT object defined below it. If a helper
# sits between a block and its intended function, the docs (and @param tags)
# silently land on the helper -- you get dot-helper.Rd instead of the function's
# Rd, plus R CMD check warnings about mismatched arguments.
#
# This script reports, for every roxygen block, which object it will bind to,
# and flags blocks that bind to a dot-prefixed internal helper.
#
# Usage:  Rscript check_roxygen_binding.R [files...]   (defaults to R/*.R)
# -----------------------------------------------------------------------------

files <- commandArgs(trailingOnly = TRUE)
if (!length(files)) files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
if (!length(files)) files <- list.files(".", pattern = "\\.R$", full.names = TRUE)

def_re <- "^\\s*([a-zA-Z._][a-zA-Z0-9._]*)\\s*(<-|=)\\s*function"
problems <- 0L

for (f in files) {
  ln <- readLines(f, warn = FALSE)
  ln <- sub("\r$", "", ln)
  is_rox <- grepl("^\\s*#'", ln)
  if (!any(is_rox)) next

  # Identify contiguous roxygen blocks.
  starts <- which(is_rox & !c(FALSE, head(is_rox, -1)))
  ends   <- which(is_rox & !c(tail(is_rox, -1), FALSE))

  for (i in seq_along(starts)) {
    b_end <- ends[i]
    # The block's title = first non-empty roxygen line.
    title <- sub("^\\s*#'\\s*", "", ln[starts[i]])
    # Find the next object definition after the block.
    rest <- ln[(b_end + 1):length(ln)]
    hit  <- grep(def_re, rest)
    if (!length(hit)) {
      # A block with no following definition is fine only if it is a
      # package-level or @rdname/@name block.
      if (!any(grepl("@name|@rdname|@docType|@keywords\\s+internal", ln[starts[i]:b_end]))) {
        cat(sprintf("  [warn] %s:%d  block binds to NOTHING: \"%s\"\n",
                    basename(f), starts[i], substr(title, 1, 50)))
        problems <<- problems + 1L
      }
      next
    }
    obj <- sub(paste0(def_re, ".*$"), "\\1", rest[hit[1]])
    line_of_obj <- b_end + hit[1]
    flag <- startsWith(obj, ".")
    cat(sprintf("  %s %s:%-4d block -> %s (line %d)%s\n",
                if (flag) "[BUG] " else "  ok  ",
                basename(f), starts[i], obj, line_of_obj,
                if (flag) "   <-- binds to an internal helper!" else ""))
    if (flag) problems <- problems + 1L
  }
}

cat("\n")
if (problems) {
  cat(sprintf("FAIL: %d roxygen block(s) bind to an unintended object.\n", problems))
  cat("Fix: move internal helpers BELOW the documented function.\n")
  quit(status = 1)
} else {
  cat("PASS: every roxygen block binds to its intended function.\n")
}
