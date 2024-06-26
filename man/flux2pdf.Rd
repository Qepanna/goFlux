% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flux2pdf.R
\name{flux2pdf}
\alias{flux2pdf}
\title{Save plots to pdf}
\usage{
flux2pdf(plot.list, outfile = NULL, width = 11.6, height = 8.2)
}
\arguments{
\item{plot.list}{a list of plots; output from the function
\code{\link[goFlux]{flux.plot}}.}

\item{outfile}{a character string indicating the full name of the pdf
output file (including the path). Default is to save the pdf
in the working directory, with the name of the plot.list.}

\item{width}{numerical value; width of the pdf page. Default is 11.6 inches.}

\item{height}{numerical value; height of the pdf page. Default is 8.2 inches.}
}
\value{
A pdf file containing all plots from plot.list, sorted by UniqueID.
}
\description{
Save a list of plots to a pdf file, and combine plots sharing a UniqueID
onto the same page.
}
\examples{
data(manID.UGGA)
CO2_flux <- goFlux(manID.UGGA, "CO2dry_ppm")
criteria <- c("MAE", "g.factor", "MDF", "SErel")
CO2_best <- best.flux(CO2_flux, criteria)
CO2_plots <- flux.plot(
  flux.results = CO2_best, dataframe = manID.UGGA,
  gastype = "CO2dry_ppm", quality.check = FALSE,
  plot.legend = c("MAE", "RMSE", "k.ratio", "g.factor", "SErel"),
  plot.display = c("Ci", "C0", "MDF", "prec", "nb.obs", "flux.term"))

\donttest{
flux2pdf(CO2_plots)}

}
\seealso{
See also the function \code{\link[goFlux]{goFlux}},
         \code{\link[goFlux]{best.flux}} and
         \code{\link[goFlux]{flux.plot}}
         for more information about usage.
}
