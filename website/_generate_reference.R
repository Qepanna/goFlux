#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# Auto-generate Quarto reference pages from the package's roxygen-derived .Rd
# files, so the website never duplicates -- and never drifts from -- the
# documentation held in the R source. Called by the pre-render hook in
# _quarto.yml, so it runs automatically on every render (including on Posit
# Connect Cloud).
# -----------------------------------------------------------------------------

pkg     <- "goFlux"      # the installed package to document
out_dir <- "reference"

# Topics to publish. Keep explicit so the aqua reference stays focused;
# set topics <- NULL to document every topic in the package.
topics <- c("goAquaFlux", "goAquaFlux.diffusive", "goAquaFlux.ebullition",
            "goAquaFlux.total", "find.bubbles", "flux.plot.aqua")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

db <- tools::Rd_db(pkg)
names(db) <- sub("\\.Rd$", "", names(db))
if (!is.null(topics)) {
  missing <- setdiff(topics, names(db))
  if (length(missing))
    warning("Topics not found in '", pkg, "': ", paste(missing, collapse = ", "),
            ". Did you run devtools::document() and reinstall?")
  db <- db[intersect(topics, names(db))]
}
if (!length(db)) stop("No matching Rd topics found in package '", pkg, "'.")

for (nm in names(db)) {
  tmp <- tempfile(fileext = ".html")
  tools::Rd2HTML(db[[nm]], out = tmp, package = pkg)
  h <- readLines(tmp, warn = FALSE)

  # Keep only the documentation body, dropping R's own page chrome.
  start <- grep("<h2", h)[1]
  end   <- grep("</main>|</body>", h)
  end   <- if (length(end)) end[length(end)] - 1L else length(h)
  body  <- h[start:end]

  # The first <h2> is the Rd \title; lift it out as the page subtitle so it
  # is not repeated as a heading under the Quarto title.
  h2i   <- grep("<h2", body)[1]
  subtitle <- gsub("<[^>]*>", "", body[h2i])
  subtitle <- trimws(gsub('"', "'", subtitle))
  body  <- body[-h2i]

  # Promote R's <h3> section headings to <h2> so Quarto's TOC picks them up.
  body <- gsub("<h3>", "<h2>", body, fixed = TRUE)
  body <- gsub("</h3>", "</h2>", body, fixed = TRUE)

  writeLines(c(
    "---",
    sprintf('title: "%s"', nm),
    sprintf('subtitle: "%s"', subtitle),
    "---",
    "",
    "::: {.callout-note appearance='simple' collapse='true'}",
    "## Where this page comes from",
    sprintf("Generated automatically from the roxygen documentation of `%s`.", nm),
    "Edit the roxygen block in the R source and re-render -- do not edit this page.",
    ":::",
    "",
    body
  ), file.path(out_dir, paste0(nm, ".qmd")))

  message("wrote ", file.path(out_dir, paste0(nm, ".qmd")))
}
