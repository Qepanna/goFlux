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
#' @return a pdf file containing all plots from plot.list, sorted by UniqueID.
#'
#' @include GoFluxYourself-package.R
#'
#' @seealso See also the function \code{\link[GoFluxYourself]{goFlux}},
#'          \code{\link[GoFluxYourself]{best.flux}} and
#'          \code{\link[GoFluxYourself]{flux.plot}}
#'          for more information about usage.
#'
#' @examples
#' data(example_LGR_manID)
#' example_LGR_flux <- goFlux(example_LGR_manID, "CO2dry_ppm")
#' criteria <- c("MAE", "g.factor", "kappa", "MDF", "SE.rel")
#' example_LGR_res <- best.flux(example_LGR_flux)
#' example_LGR_plots <- flux.plot(example_LGR_res, example_LGR_manID, "CO2dry_ppm")
#' flux2pdf(example_LGR_plots)
#'
#' @export
#'
flux2pdf <- function(plot.list, outfile = NULL,
                     width = 11.6, height = 8.2) {

  # Assign NULL to variables without binding
  . <- NULL

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

  # Create au outfile if missing
  if (is.null(outfile)) {
    outfile <- paste(getwd(), deparse(substitute(plot.list)), ".pdf")
  }
  # Print pdf
  pdf(file = outfile, width = width, height = height)
  quiet(print(outplot))
  dev.off()

}
