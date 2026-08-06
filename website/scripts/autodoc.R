# ---------------------------------------------------------------------------
# autodoc.R — render a function's "Usage" + "Arguments" from its man/<fn>.Rd
#
# 
#   The Usage (signature) and Arguments (argument table) sections of the site
#   are pure transcriptions of the .Rd files, which are themselves generated
#   from roxygen2 comments by devtools::document(). Those are the only two
#   sections that drift when the code changes (e.g. the goFlux k.min default).
#   This helper regenerates them fresh at every `quarto render`, so they can
#   never go stale, and nothing generated is ever committed to git.
#
#   Everything else on a page (Details, Value, Examples, narrative, equations,
#   callouts) stays hand-written in the .qmd, OUTSIDE the autodoc chunk.
# ---------------------------------------------------------------------------

# Base-R null fallback (tiny helper, keeps the code clean)
`%||%` <- function(a, b) if (is.null(a)) b else a

# First top-level child of a parsed Rd element with a given tag (e.g. "\usage")
rd_child <- function(el, tag) {
  for (x in el) if (identical(attr(x, "Rd_tag"), tag)) return(x)
  NULL
}

# Flatten one parsed-Rd element (a string or a tag) to markdown text.
# inline = TRUE  -> \code / \link become `backticked`   (for tables and prose)
# inline = FALSE -> they stay plain                      (for the Usage block)
rd_to_md <- function(x, inline = TRUE) {
  if (is.null(x)) return("")
  if (is.character(x)) return(paste(x, collapse = ""))

  tag <- attr(x, "Rd_tag")

  # A plain list (no Rd_tag) is a SEQUENCE of Rd elements -- e.g. a \item
  # description made of several inline pieces ("text", \code{...}, \ifelse{...}).
  # Render each piece and concatenate. Without this, paste() would dump the
  # raw Rd list structure into the output.
  if (is.null(tag)) {
    return(paste(vapply(x, rd_to_md, character(1), inline = inline), collapse = ""))
  }

  kids <- function() paste(vapply(x, rd_to_md, character(1), inline = inline), collapse = "")

  switch(tag,
    "TEXT"    = paste(x, collapse = ""),
    "RCODE"   = paste(x, collapse = ""),
    "VERB"    = paste(x, collapse = ""),
    "COMMENT" = "",
    "\\code"  = if (inline) {
                  k <- trimws(kids())
                  if (grepl("^`.*`$", k)) k else paste0("`", k, "`")
                } else kids(),
    "\\link"  = paste0("`", trimws(kids()), "`"),
    "\\kbd"   = paste0("`", kids(), "`"),
    "\\samp"  = paste0("`", kids(), "`"),
    "\\env"   = paste0("`", kids(), "`"),
    "\\pkg"   = paste0("**", kids(), "**"),
    "\\emph"  = paste0("*", kids(), "*"),
    "\\var"   = paste0("*", kids(), "*"),
    "\\strong"= paste0("**", kids(), "**"),
    "\\dots"  = "...",
    "\\ldots" = "...",
    "\\cr"    = " ",
    "\\itemize"   = list_md(x, "-"),
    "\\enumerate" = list_md(x, "1."),
    "\\describe"  = describe_md(x),
    "\\eqn"   = rd_to_md(x[[1]], inline = inline),
    "\\deqn"  = rd_to_md(x[[1]], inline = inline),
    "\\ifelse"= if (trimws(rd_to_md(x[[1]], inline = FALSE)) == "html")
                  rd_to_md(x[[2]], inline = inline)
                else rd_to_md(x[[3]], inline = inline),
    "\\out"   = kids(),
    "\\href"  = paste0("[", rd_to_md(x[[2]], inline = inline), "](",
                       trimws(rd_to_md(x[[1]], inline = FALSE)), ")"),
    "\\url"   = paste0("<", kids(), ">"),
    "\\preformatted" = paste0("\n\n```\n", kids(), "\n```\n\n"),
    "\\item"  = kids(),
    "\\value" = kids(),
    "\\tabular" = "",
    kids()
  )
}

# \itemize / \enumerate -> markdown bullet / numbered list
list_md <- function(x, bullet) {
  items <- Filter(function(el) identical(attr(el, "Rd_tag"), "\\item"), x)
  out <- vapply(items, function(it) paste0(bullet, " ", rd_to_md(it)), character(1))
  paste(out, collapse = "\n")
}

# \describe{ \item{term}{desc} ... } -> markdown definition list
describe_md <- function(x) {
  items <- Filter(function(el) identical(attr(el, "Rd_tag"), "\\item"), x)
  out <- vapply(items, function(it) {
    paste0("- **", trimws(rd_to_md(it[[1]])), "**: ", trimws(rd_to_md(it[[2]])))
  }, character(1))
  paste(out, collapse = "\n")
}

# The Usage section: the raw signature, no backticks
render_usage <- function(usage_tag) {
  trimws(rd_to_md(usage_tag, inline = FALSE))
}

# The Arguments section: one table row per \item{arg}{description}
render_arguments <- function(args_tag) {
  items <- Filter(function(el) identical(attr(el, "Rd_tag"), "\\item"), args_tag)
  if (!length(items)) return("")
  rows <- vapply(items, function(it) {
    arg  <- trimws(rd_to_md(it[[1]]))
    desc <- gsub("[[:space:]]+", " ", trimws(rd_to_md(it[[2]])))
    paste0("| `", arg, "` | ", desc, " |")
  }, character(1))
  c("| Argument | Description |", "|---|---|", rows)
}

# Main entry point -----------------------------------------------------------
# autodoc(fn, level) -> the Usage + Arguments block as a markdown string.
#   fn    : function name, must match man/<fn>.Rd
#   level : heading level of the generated "Usage" / "Arguments" headings
#           (4 for most blocks, 3 for import2RData / autoID)
#   man_dir: where man/ lives; auto-detected if NULL
autodoc <- function(fn, level = 4, man_dir = NULL) {
  if (is.null(man_dir)) {
    cands <- c("../man", "man", "website/man", "website/../man")
    hit <- cands[file.exists(file.path(cands, paste0(fn, ".Rd")))]
    if (!length(hit)) stop("Could not find man/", fn, ".Rd -- pass man_dir explicitly.")
    man_dir <- hit[1]
  }
  rd_path <- file.path(man_dir, paste0(fn, ".Rd"))
  if (!file.exists(rd_path)) stop("No such file: ", rd_path)

  rd    <- tools::parse_Rd(rd_path)
  usage <- rd_child(rd, "\\usage")
  args  <- rd_child(rd, "\\arguments")
  if (is.null(usage)) stop(fn, ".Rd has no \\usage section")
  if (is.null(args))  stop(fn, ".Rd has no \\arguments section")

  h <- function(txt) paste0(strrep("#", level), " ", txt)

  paste(c(
    h("Usage"),
    "",
    "::: callout-note",
    "Code chunks under **Usage** sections are not part of the demonstration. They are meant to show you how to use the arguments in the function.",
    ":::",
    "",
    "```r",
    render_usage(usage),
    "```",
    "",
    h("Arguments"),
    "",
    render_arguments(args)
  ), collapse = "\n")
}