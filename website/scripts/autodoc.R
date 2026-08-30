# ---------------------------------------------------------------------------
# autodoc.R — render a function's "Usage", "Arguments", "Details", "Examples"
#             and "References" from its man/<fn>.Rd
#
# 
#   The Usage (signature) and Arguments (argument table) sections of the site
#   are pure transcriptions of the .Rd files, which are themselves generated
#   from roxygen2 comments by devtools::document(). Those are the only two
#   sections that drift when the code changes (e.g. the goFlux k.min default).
#   This helper regenerates them fresh at every `quarto render`, so they can
#   never go stale, and nothing generated is ever committed to git.
#
#   Everything else on a page (narrative, equations,
#   callouts) stays hand-written in the .qmd, OUTSIDE the autodoc chunk,
#    if the user sets these arguments to FALSE.
# ---------------------------------------------------------------------------

# Base-R null fallback (tiny helper, keeps the code clean)
`%||%` <- function(a, b) if (is.null(a)) b else a

# Normalize whitespace INSIDE one paragraph or list item to a clean single
# space. This is the "template" for inline spacing: it collapses every run of
# whitespace (source line wraps, indentation, and the space fragments
# parse_Rd inserts around \code{} / \link{} / \ifelse{}) into one space, then
# removes spaces that landed before punctuation (e.g. "`mov.win` )").
normalize_inline <- function(txt) {
  txt <- gsub("[[:space:]]+", " ", txt)
  txt <- gsub("[[:space:]]+([.,;:!?)\\]])", "\\1", txt)
  trimws(txt)
}

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
  # parse_Rd stores \itemize/\enumerate item TEXT as a SIBLING of the bare
  # \item marker (unlike \describe/\arguments, where it's \item's argument).
  # Emit the bullet at each marker, then attach the following text to it.
  parts <- character(0)
  cur   <- character(0)
  open  <- FALSE
  for (el in x) {
    if (identical(attr(el, "Rd_tag"), "\\item")) {
      if (open) {
        parts <- c(parts, paste0(bullet, " ", normalize_inline(paste(cur, collapse = " "))))
      }
      cur <- character(0)
      open <- TRUE
    } else {
      txt <- rd_to_md(el, inline = TRUE)
      if (nzchar(trimws(txt))) cur <- c(cur, txt)
    }
  }
  if (open) parts <- c(parts, paste0(bullet, " ", normalize_inline(paste(cur, collapse = " "))))
  paste(parts, collapse = "\n")
}

# \describe{ \item{term}{desc} ... } -> markdown definition list
describe_md <- function(x) {
  items <- Filter(function(el) identical(attr(el, "Rd_tag"), "\\item"), x)
  out <- vapply(items, function(it) {
    paste0("- **", normalize_inline(rd_to_md(it[[1]])), "**: ",
           normalize_inline(rd_to_md(it[[2]])))
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
  arg_names <- vapply(items, function(it) trimws(rd_to_md(it[[1]])), character(1))
  rows <- vapply(seq_along(items), function(i) {
    desc <- gsub("[[:space:]]+", " ", trimws(rd_to_md(items[[i]][[2]])))
    paste0("| `", arg_names[i], "` | ", desc, " |")
  }, character(1))
  # pandoc/Quarto sets pipe-table column widths from the relative dash count
  # in the separator row (verified: "|---|---|" -> 50%/50%). Make the
  # Argument column narrow -- proportional to the longest argument name,
  # capped -- so short names don't leave a big empty gap before Description.
  max_arg <- max(nchar(arg_names))
  arg_dashes <- max(6L, min(30L, max_arg + 4L))
  sep <- paste0("|", strrep("-", arg_dashes), "|", strrep("-", 80L), "|")
  c("| Argument | Description |", sep, rows)
}
# The Details section: prose from \details, one paragraph per block
# Split the raw \details source (as written by the author in roxygen) into an
# ordered list of blocks, each list(type = ..., text = ...):
#   type "prose" -> a plain paragraph
#   type "\\itemize" / "\\enumerate" / "\\describe" / "\\preformatted"
#                 -> a whole block-level markup span
# Paragraph boundaries are the blank lines the author wrote. Block-level
# markup is pulled out FIRST so a list sitting next to prose without a blank
# line (e.g. "... raw file:\n\itemize{...}\nIf your LGR ...") still becomes its
# own block instead of being glued to the surrounding prose.
segment_details <- function(raw) {
  block_tags <- c("\\itemize", "\\enumerate", "\\describe", "\\preformatted")
  blocks <- list()
  push <- function(type, text) {
    text <- trimws(text)
    if (nzchar(text)) blocks[[length(blocks) + 1L]] <<- list(type = type, text = text)
  }
  # Blank lines => paragraph breaks (the author's "new paragraph" signal).
  chunks <- strsplit(raw, "\n[[:space:]]*\n")[[1]]
  for (chunk in chunks) {
    rest <- chunk
    repeat {
      # Find the earliest block-level tag in this chunk.
      pos <- Inf; hit <- NA_character_
      for (t in block_tags) {
        m <- regexpr(paste0("\\", t, "[[:space:]]*\\{"), rest)
        if (m > 0L && m < pos) { pos <- m; hit <- t }
      }
      if (!is.finite(pos)) break
      # Prose before the block.
      push("prose", substr(rest, 1L, pos - 1L))
      # Brace-match the block's {...} span (handles nested/multi-line braces).
      chars <- strsplit(substr(rest, pos, nchar(rest)), "")[[1]]
      depth <- 0L; end <- NA_integer_
      for (i in seq_along(chars)) {
        if (chars[i] == "{") depth <- depth + 1L
        if (chars[i] == "}") {
          depth <- depth - 1L
          if (depth == 0L) { end <- i; break }
        }
      }
      if (!is.finite(end)) break          # safety: unterminated tag -> keep as prose
      push(hit, substr(rest, pos, pos + end - 1L))
      rest <- substr(rest, pos + end, nchar(rest))   # continue after the block
    }
    push("prose", rest)
  }
  blocks
}

# Parse a raw Rd fragment (one block of \details content) and render it to
# markdown. Same trick as render_references: wrap the fragment in a minimal Rd
# document so tools::parse_Rd can parse it, then reuse rd_to_md.
mini_parse_details <- function(text) {
  mini <- paste0("\\name{x}\n\\alias{x}\n\\title{t}\n\\details{", text, "}")
  rd   <- tools::parse_Rd(textConnection(mini))
  rd_to_md(rd_child(rd, "\\details"), inline = TRUE)
}

# The Details section: raw \details content from man/<fn>.Rd, split into
# author-defined paragraphs and lists, each rendered with a fixed template:
#   prose -> one paragraph, whitespace normalized, flowing continuously
#   lists -> markdown bullets / definition lists (via list_md / describe_md)
# Blocks are joined with a blank line so markdown renders them as real
# paragraphs / lists, never merged into one blob.
render_details <- function(rd_path) {
  raw <- rd_section_source(rd_path, "details")
  if (is.null(raw)) return("")

  blocks <- segment_details(raw)

  out <- vapply(blocks, function(b) {
    if (identical(b$type, "prose")) {
      txt <- tryCatch(mini_parse_details(b$text),
                      warning = function(w) NULL, error = function(err) NULL)
      if (is.null(txt)) txt <- normalize_inline(b$text)   # graceful fallback
      normalize_inline(txt)
    } else {
      trimws(mini_parse_details(b$text))
    }
  }, character(1))

  out <- out[nzchar(trimws(out))]
  if (!length(out)) return("")
  paste(out, collapse = "\n\n")
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
  depth <- 1L; end <- 0L
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
        txt <- tryCatch({
            mini <- paste0("\\name{x}\n\\alias{x}\n\\title{t}\n\\references{", e, "}")
            rd <- tools::parse_Rd(textConnection(mini))
            rd_to_md(rd_child(rd, "\\references"), inline = TRUE)
        }, warning = function(w) NULL, error = function(err) NULL)
        if (is.null(txt) || !nzchar(trimws(txt))) {
            txt <- e   # graceful fallback: use the raw entry text as-is
        }
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
    "```r",
    render_usage(usage),
    "```",
    "",
    h("Arguments"),
    "",
    render_arguments(args)
  )

  if (details && !is.null(det)) {
    block <- c(block, "", h("Details"), "", render_details(rd_path))
  }
  if (examples && !is.null(exm)) {
    block <- c(block, "", h("Examples"), "", "```r", render_examples(exm), "```")
  }
  if (references && !is.null(ref)) {
    block <- c(block, "", h("References"), "", render_references(ref, rd_path))
  }

  paste(block, collapse = "\n")
}