#' Save plots to pdf
#'
#' Save a list of plots to a pdf file, and combine plots sharing a UniqueID
#' onto the same page.
#'
#' @param plot.list a list of plots; output from the function
#'                  \code{\link[goFlux]{flux.plot}}.
#' @param outfile a character string indicating the full name of the pdf
#'                output file (including the path and '.pdf' extension). If
#'                \code{outfile} is not provided, it will be saved in R's
#'                temporary directory (\code{tempdir()}), with the name of
#'                the \code{plot.list}.
#' @param width numerical value; width of the pdf page. Default is 11.6 inches.
#' @param height numerical value; height of the pdf page. Default is 8.2 inches.
#'
#' @return A pdf file containing all plots from plot.list, sorted by UniqueID.
#'
#' @include goFlux-package.R
#'
#' @seealso See also the function \code{\link[goFlux]{goFlux}},
#'          \code{\link[goFlux]{best.flux}} and
#'          \code{\link[goFlux]{flux.plot}}
#'          for more information about usage.
#'
#' @examples
#' data(manID.UGGA)
#' CO2_flux <- goFlux(manID.UGGA, "CO2dry_ppm")
#' criteria <- c("MAE", "g.factor", "MDF", "SErel")
#' CO2_best <- best.flux(CO2_flux, criteria)
#' CO2_plots <- flux.plot(
#'   flux.results = CO2_best, dataframe = manID.UGGA,
#'   gastype = "CO2dry_ppm", quality.check = FALSE,
#'   plot.legend = c("MAE", "RMSE", "k.ratio", "g.factor", "SErel"),
#'   plot.display = c("Ci", "C0", "MDF", "prec", "nb.obs", "flux.term"))
#'
#' flux2pdf(CO2_plots)
#'
#' @export
#'
flux2pdf <- function(plot.list, outfile = NULL,
                     width = 11.6, height = 8.2) {

  # Check arguments ####

  ## plot.list ####
  if (missing(plot.list)) stop("'plot.list' is required")
  if (!is.list(plot.list) || length(plot.list) == 0) {
    stop("'plot.list' must be a non-empty list.")
  }

  # Ensure each element has a UniqueID in plot_env
  has_uid <- vapply(
    plot.list,
    function(x) !is.null(x$plot_env$UniqueID) &&
      is.character(x$plot_env$UniqueID) &&
      length(x$plot_env$UniqueID) == 1 &&
      !is.na(x$plot_env$UniqueID),
    logical(1)
  )

  if (!all(has_uid)) {
    bad <- which(!has_uid)
    stop(
      "All elements of 'plot.list' must have a non-missing character scalar ",
      "'plot_env$UniqueID'. Problem at element(s): ",
      paste(bad, collapse = ", ")
    )
  }

  # Ensure all elements of plot.list are ggplot objects
  is_gg <- vapply(plot.list, inherits, logical(1), what = "ggplot")
  if (!all(is_gg)) {
    stop("All elements of 'plot.list' must inherit from class 'ggplot'.")
  }

  ## outfile ####
  if (!is.null(outfile)) {
    if (!is.character(outfile) || length(outfile) != 1) {
      stop("'outfile' must be a single character string")
    }
    if (!grepl("\\.pdf$", outfile, ignore.case = TRUE)) {
      stop("'outfile' requires the extension '.pdf'")
    }
  }

  # Create an outfile if missing
  auto_outfile <- FALSE

  if (is.null(outfile)) {
    outfile <- file.path(
      tempdir(),
      paste0(deparse(substitute(plot.list)), ".pdf")
    )
    auto_outfile <- TRUE
  }

  # Normalize path
  outfile <- normalizePath(outfile, winslash = "/", mustWork = FALSE)

  # Check that output location is writable
  outdir <- dirname(outfile)
  if (!dir.exists(outdir)) {
    stop("Directory does not exist: ", outdir)
  }
  if (file.access(outdir, 2) != 0) {
    stop("Cannot write to directory: ", outdir)
  }

  ## width and height ####
  if (!is.numeric(width)  || length(width)  != 1 || !is.finite(width)  ||
      width  <= 0) stop("'width' must be a single positive number")
  if (!is.numeric(height) || length(height) != 1 || !is.finite(height) ||
      height <= 0) stop("'height' must be a single positive number")

  # Assign NULL to variables without binding ####
  . <- NULL

  # FUNCTION STARTS ####
  group_plot.list <- rlist::list.group(plot.list, .$plot_env$UniqueID)

  pbapply::pboptions(char = "=")
  outplot <- pbapply::pblapply(group_plot.list, function(p) {

    title <- grid::textGrob(paste("Unique ID:", p[[1]]$plot_env$UniqueID),
                            gp = grid::gpar(fontsize = 16))
    footnote <- grid::textGrob(
      paste("page", which(
        names(group_plot.list) == p[[1]]$plot_env$UniqueID), "of",
        length(group_plot.list)),
      gp = grid::gpar(fontface = 3, fontsize = 12), hjust = 1)
    n.plot <- as.numeric(length(p))
    nrow <- ifelse(n.plot <= 2, 1,
                   ifelse(n.plot <= 4, 2, 4))
    ncol <- ifelse(n.plot <= 1, 1, 2)

    gridExtra::marrangeGrob(grobs = p, ncol = ncol, nrow = nrow,
                            top = title, bottom = footnote)
  })

  # Print pdf
  if (length(outplot) > 10000) {
    stop("The outfile contains more than 10,000 pages. 'plot.list' must be reduced.")
  }

  grDevices::pdf(file = outfile, width = width, height = height)
  on.exit(grDevices::dev.off(), add = TRUE)

  max.print <- getOption("max.print")
  on.exit(options(max.print = max.print), add = TRUE)
  options(max.print = 10000)

  SimDesign::quiet(print(outplot))

  # Return outfile if not provided
  if (auto_outfile) {
    message(
      "'outfile' was not provided. The PDF was saved to:\n  ",
      outfile,
      "\n(Note: this file is stored in a temporary directory and may be deleted when the R session ends.)"
    )
  }
  invisible(outfile)

}
