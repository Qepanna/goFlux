#' Save plots to pdf
#'
#' Save a list of plots to a pdf file, and combine plots sharing a UniqueID
#' onto the same page.
#'
#' @param plot.list a list of plots; output from the function
#'                  \code{\link[GoFluxYourself]{flux.plot}}.
#' @param outfile a character string indicating the full name of the pdf
#'                output file (including the path). Default is to save the pdf
#'                in the working directory, with the name of the plot.list.
#' @param width numerical value; width of the pdf page. Default is 11.6 inches.
#' @param height numerical value; height of the pdf page. Default is 8.2 inches.
#'
#' @return A pdf file containing all plots from plot.list, sorted by UniqueID.
#'
#' @include GoFluxYourself-package.R
#'
#' @seealso See also the function \code{\link[GoFluxYourself]{goFlux}},
#'          \code{\link[GoFluxYourself]{best.flux}} and
#'          \code{\link[GoFluxYourself]{flux.plot}}
#'          for more information about usage.
#'
#' @examples
#' data(LGR_manID)
#' LGR_flux <- goFlux(LGR_manID, "CO2dry_ppm")
#' criteria <- c("MAE", "g.factor", "MDF", "SErel")
#' LGR_res <- best.flux(LGR_flux, criteria)
#' LGR_plots <- flux.plot(
#'   flux.results = LGR_res, dataframe = LGR_manID,
#'   gastype = "CO2dry_ppm", quality.check = FALSE,
#'   plot.legend = c("MAE", "RMSE", "k.ratio", "g.factor", "SErel"),
#'   plot.display = c("Ci", "C0", "MDF", "prec", "nb.obs", "flux.term"))
#' flux2pdf(LGR_plots)
#'
#' @export
#'
flux2pdf <- function(plot.list, outfile = NULL,
                     width = 11.6, height = 8.2) {

  # Check arguments
  ## plot.list
  if(missing(plot.list)) stop("'plot.list' is required") else {
    if(!is.list(plot.list)) stop("'plot.list' must be of class list") else {
      if(length(grep("ggplot", sapply(plot.list, class))) != length(plot.list)){
        stop("all elements of 'plot.list' must be of class 'gg, ggplot'")
      }
    }
  }
  ## outfile
  if(!is.null(outfile)){
    if(!is.character(outfile)) stop("'outfile' must be of class character")
  }
  ## width and height
  if(!is.numeric(width)) stop("'width' must be of class numeric")
  if(!is.numeric(height)) stop("'height' must be of class numeric")

  # Assign NULL to variables without binding ####
  . <- NULL

  # FUNCTION STARTS ####
  group_plot.list <- list.group(plot.list, .$plot_env$UniqueID)

  pboptions(char = "=")
  outplot <- pblapply(group_plot.list, function(p) {

    title <- textGrob(paste("Unique ID:", p[[1]]$plot_env$UniqueID),
                      gp = gpar(fontsize = 16))
    footnote <- textGrob(paste("page", which(
      names(group_plot.list) == p[[1]]$plot_env$UniqueID), "of",
      length(group_plot.list)), gp = gpar(fontface = 3, fontsize = 12), hjust = 1)
    n.plot <- as.numeric(length(p))
    nrow <- ifelse(n.plot <= 2, 1,
                   ifelse(n.plot <= 4, 2, 4))
    ncol <- ifelse(n.plot <= 1, 1, 2)

    marrangeGrob(grobs = p, ncol = ncol, nrow = nrow,
                 top = title, bottom = footnote)
  })

  # Create an outfile if missing
  if (is.null(outfile)) {
    outfile <- paste(getwd(), deparse(substitute(plot.list)), ".pdf")
  }
  # Print pdf
  if(length(outplot) > 10000) stop("The outfile contains more than 10,000 pages. 'plots.list' must be reduced.")
  pdf(file = outfile, width = width, height = height)
  options(max.print = 10000)
  quiet(print(outplot))
  dev.off()

}
