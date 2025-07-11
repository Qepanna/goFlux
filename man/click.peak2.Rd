% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/click.peak2.R
\name{click.peak2}
\alias{click.peak2}
\title{Manual identification of start and end of gas measurements}
\usage{
click.peak2(
  ow.list,
  gastype = "CO2dry_ppm",
  sleep = 3,
  plot.lim = c(380, 1000),
  seq = NULL,
  warn.length = 60,
  save.plots = NULL,
  width = 14,
  height = 8,
  abline = TRUE,
  abline_corr = TRUE
)
}
\arguments{
\item{ow.list}{list of data.frame; output from the function
\code{\link[goFlux]{obs.win}}. Must contain the columns
\code{gastype} (see below), \code{POSIX.time} and \code{UniqueID}.}

\item{gastype}{character string; specifies which gas should be displayed on
the plot to manually select start time and end time of
measurements. Must be one of the following: "CO2dry_ppm",
"COdry_ppb", "CH4dry_ppb", "N2Odry_ppb", "NO2dry_ppb", "NOdry_ppb",
"NH3dry_ppb" or "H2O_ppm". Default is "CO2dry_ppm".}

\item{sleep}{numerical value; delay before closing the resulting plot. Grants
a delay between measurements to visually inspect the output
before processing the next measurement. Sleep must be shorter
than 10 seconds. If \code{sleep = NULL}, the  plots will not close.}

\item{plot.lim}{numerical vector of length 2; sets the Y axis limits in the
plots. Default values are set for a typical gas measurement
of "CO2dry_ppm" from soils: \code{plot.lim = c(380,1000)}.}

\item{seq}{a numerical sequence that indicates objects in a list. By default,
\code{seq = NULL} and the function loops through all data frames
in \code{ow.list}.}

\item{warn.length}{numerical value; limit under which a measurement is flagged
for being too short (\code{nb.obs < warn.length}). Default
value is \code{warn.length = 60}.}

\item{save.plots}{character string; a file path with the extension .pdf to
save the plots produced with click.peak2. By default,
\code{save.plot = NULL} and plots are not saved.}

\item{width}{numerical value; width of the pop-up window.}

\item{height}{numerical value; height of the pop-up window.}

\item{abline}{logical; if TRUE (default), blue vertical lines indicate
\code{start.time} and \code{end.time} on both plots.}

\item{abline_corr}{logical; if TRUE (default), red vertical lines indicate
\code{start.time_corr} and \code{end.time_corr} on validation plot.}
}
\value{
A data.frame, identical to an unlisted version of the input
         \code{ow.list}, with the additional columns \code{flag}, \code{Etime},
         \code{start.time_corr}, \code{end.time_corr} and \code{obs.length_corr}.
}
\description{
Identify the start and the end of a measurement by clicking on them in a
scatter plot. First apply the function \code{\link[goFlux]{obs.win}} to
identify the observation window of each measurement and then use the function
\code{\link[goFlux]{click.peak2}} (see examples below).
}
\details{
The argument \code{plot.lim} is used to remove any data points below and
above the plot limits for a better view of the scatter plot. If the gas
measurements are larger than the minimum or smaller than the maximum plot
limit values, then the plot will automatically zoom in and adjust to those
values. The default plot limits are set for a typical gas measurement of
"CO2dry_ppm" from a soil respiration measurement: \code{plot.lim = c(380,1000)},
where 380 ppm is the minimum plotted concentration, which should be close to
atmospheric concentration, and 1000 ppm is the maximum plotted concentration,
which correspond to a maximal accumulated concentration in the chamber before
considering it an outlier (e.g. caused by breath or gas bubble). For other
gasses, the user must specify the plot limits themselves. Here are some
suggestions of plot limits for the other gases:
\itemize{
  \item "CH4dry_ppb": \code{plot.lim = c(2200, 1800)}
  \item "N2Odry_ppb": \code{plot.lim = c(250, 500)}
  \item "NH3dry_ppb": \code{plot.lim = c(0, 200)}
  \item "COdry_ppb": \code{plot.lim = c(0, 200)}
  \item "H2O_ppm": \code{plot.lim = c(10000, 20000)}
}
These values will vary depending on ecosystem type and chamber application scheme.

IMPORTANT! This function makes use of the function \code{\link[graphics]{identify}}
which is only supported on screen devices such as X11, windows and quartz.
It is therefore essential to verify that your system options are compatible
with this function before running it, to avoid errors. Use the function
\code{getOption("device")} to find which device your system uses.

The argument \code{seq} is used to select a subset of data frame from the list
of data frames in \code{ow.list}. For example, to apply the function on the
first measurement in the list, set \code{seq = 1}, or \code{seq = seq(1,10)}
for the first 10 measurements.

\code{warn.length} is the limit below which the chamber closure time is
flagged for being too short (\code{nb.obs < warn.length}). Portable
greenhouse gas analyzers typically measure at a frequency of 1 Hz. Therefore,
for the default setting of \code{warn.length = 60}, the chamber closure time
should be approximately one minute (60 seconds). If the number of
observations is smaller than the threshold, a warning is printed: "Number of
observations for UniqueID: 'UniqueID' is X observations".

In \code{gastype}, the gas species listed are the ones for which this package
has been adapted. Please write to the maintainer of this package for
adaptation of additional gases.

The arguments \code{width} and \code{heigth} are used with the function
\code{\link[grDevices]{dev.new}} to define the dimensions of the pop-up
window.
}
\examples{
# IMPORTANT! This function makes use of the function graphics::identify()
# which is only supported on screen devices such as X11, windows and quartz.
# It is therefore essential to verify that your system options are compatible
# with this function before running it, to avoid errors. Here is an example
# of how to modify your system options for graphics device:
\dontrun{
default.device <- getOption("device") # save default option
options(device = "X11") # change system option to device = "X11"
options(device = default.device) # revert back to default option }

# There are multiple ways to use this function. Here are some examples:

library(dplyr)
\donttest{
## with a LGR instrument and an auxiliary file (.txt),
## and plots saved as pdf
aux.path <- system.file("extdata", "aux_UGGA/aux_UGGA.txt", package = "goFlux")
auxfile <- read.delim(aux.path) \%>\%
  mutate(start.time = as.POSIXct(start.time, tz = "UTC"))
data(imp.UGGA)
ow.UGGA <- obs.win(inputfile = imp.UGGA, auxfile = auxfile,
                   obs.length = 180, shoulder = 60)
save.to <- paste(tempdir(), "manID.UGGA.pdf", sep = "/")
manID.UGGA <- click.peak2(ow.UGGA, save.plots = save.to)
# In this example, plots are saved in a temporary directory using
# the function tempdir(). To save files in your working directory
# instead, use the function getwd().


## with a LI-COR instrument, the Smart Chamber as auxiliary file
## and only the first data frame selected
data(imp.LI8200)
data(imp.LI7820)
ow.LI7820 <- obs.win(inputfile = imp.LI7820, auxfile = imp.LI8200, shoulder = 60)
manID.LI7820 <- click.peak2(ow.LI7820, gastype = "N2Odry_ppb",
                            plot.lim = c(250,500), seq = 1)


## with the LI-6400, no auxiliary file
## and seq through the first 3 data frames
data(imp.LI6400)
ow.LI6400 <- obs.win(inputfile = imp.LI6400, shoulder = 0)
manID.LI6400 <- click.peak2(ow.LI6400, seq = seq(1,3)) }
}
\seealso{
See also \code{\link[goFlux]{obs.win}} to prepare a list of data.frame.

For an automatic identification of gas measurements, see the function
\code{\link[goFlux]{autoID}}.
}
