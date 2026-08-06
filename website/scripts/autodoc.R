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
# The Details section: prose from \details, one paragraph per block
render_details <- function(details_tag) {
  # Take the rendered text and reformat it into a clean paragraph:
  #  1) flatten every run of whitespace (line breaks + the fragments parse_Rd
  #     inserts) into a single space,
  #  2) drop spaces that landed before punctuation (e.g. "`mov.win` )"),
  #  3) re-wrap into readable ~80-char lines.
  txt <- rd_to_md(details_tag, inline = TRUE)
  txt <- gsub("[[:space:]]+", " ", txt)
  txt <- gsub("[[:space:]]+([.,;:!?)\\]])", "\\1", txt)
  txt <- strwrap(txt, width = 80)
  paste(txt, collapse = "\n")
}
# The Examples section: R code from \examples, as a code block.
# \dontrun / \donttest blocks are shown; \dontshow / \testonly are hidden.
render_examples <- function(examples_tag) {
  parts <- vapply(examples_tag, function(el) {
    tag <- attr(el, "Rd_tag")
    if (identical(tag, "\\dontshow") || identical(tag, "\\testonly")) return("")
    body <- rd_to_md(el, inline = FALSE)
    if (identical(tag, "\\dontrun")) body <- paste0("# Not run:\n", body)
    body
  }, character(1))
  txt <- paste(parts, collapse = "")      # was "\n" -- elements already carry newlines
  txt <- gsub("[ \t]+\n", "\n", txt)      # trim trailing spaces per line
  txt <- gsub("\n{3,}", "\n\n", txt)      # collapse excess blank lines
  trimws(txt)
}
# Extract the raw source of a top-level Rd section (e.g. \references{...})
# from an .Rd file; returns the content between the braces, or NULL.
rd_section_source <- function(rd_path, tag) {
  txt <- paste(readLines(rd_path, warn = FALSE), collapse = "\n")
  m <- regexpr(paste0("\\\\", tag, "\\s*\\{"), txt)
  if (m < 0L) return(NULL)
  start <- m + attr(m, "match.length")
  chars <- strsplit(substr(txt, start, nchar(txt)), "")[[1]]
  depth <- 0L; end <- 0L
  for (i in seq_along(chars)) {
    if (chars[i] == "{") depth <- depth + 1L
    if (chars[i] == "}") {
      depth <- depth - 1L
      if (depth == 0L) { end <- i; break }
    }
  }
  if (end == 0L) return(NULL)
  substr(txt, start, start + end - 2L)
}
# The References section: entries from \references as a bullet list
render_references <- function(ref_tag, rd_path = NULL) {
  # Render each reference as its own paragraph, like the site's bottom
  # bibliography. Split on the real blank lines in the raw \references source.
  entries <- NULL
  if (!is.null(rd_path)) {
    raw <- rd_section_source(rd_path, "references")
    if (!is.null(raw)) {
      entries <- strsplit(raw, "\n[[:space:]]*\n")[[1]]
      entries <- vapply(entries, function(e) {
        mini <- paste0("\\name{x}\n\\alias{x}\n\\title{t}\n\\references{", e, "}")
        rd <- tools::parse_Rd(textConnection(mini))
        txt <- rd_to_md(rd_child(rd, "\\references"), inline = TRUE)
        txt <- gsub("[[:space:]]+", " ", txt)
        txt <- gsub("[[:space:]]+([.,;:!?)\\]])", "\\1", txt)
        trimws(txt)
      }, character(1))
    }
  }
  if (is.null(entries)) {
    entries <- gsub("[[:space:]]+", " ",
                    paste(vapply(ref_tag, rd_to_md, character(1), inline = TRUE),
                          collapse = " "))
  }
  entries <- entries[nzchar(entries)]
  if (!length(entries)) return("")
  paste(entries, collapse = "\n\n")
}
# Main entry point -----------------------------------------------------------
# autodoc(fn, level) -> the Usage + Arguments block as a markdown string.
#   fn    : function name, must match man/<fn>.Rd
#   level : heading level of the generated "Usage" / "Arguments" headings
#           (4 for most blocks, 3 for import2RData / autoID)
#   man_dir: where man/ lives; auto-detected if NULL
autodoc <- function(fn, level = 4, man_dir = NULL,
                    details = TRUE, examples = TRUE, references = TRUE) {
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
  det   <- rd_child(rd, "\\details")
  exm   <- rd_child(rd, "\\examples")
  ref   <- rd_child(rd, "\\references")
  if (is.null(usage)) stop(fn, ".Rd has no \\usage section")
  if (is.null(args))  stop(fn, ".Rd has no \\arguments section")

  h <- function(txt) paste0(strrep("#", level), " ", txt)

  block <- c(
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
  )

  if (details && !is.null(det)) {
    block <- c(block, "", h("Details"), "", render_details(det))
  }
  if (examples && !is.null(exm)) {
    block <- c(block, "", h("Examples"), "", "```r", render_examples(exm), "```")
  }
  if (references && !is.null(ref)) {
    block <- c(block, "", h("References"), "", render_references(ref, rd_path))
  }

  paste(block, collapse = "\n")
}