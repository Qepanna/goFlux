% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LGR_manID-data.R
\docType{data}
\name{LGR_manID}
\alias{LGR_manID}
\title{Example gas measurements data from LGR instruments}
\format{
An object of class \code{"data.frame"}
}
\usage{
data(LGR_manID)
}
\description{
Processed data from the micro-GGA Los Gatos Research greenhouse gas
analyzer after manual identification of measurements with the functions
\code{\link[goFlux]{obs.win}} and
\code{\link[goFlux]{click.peak.loop}} of this package.
}
\examples{
data(LGR_manID)
head(LGR_manID)

}
\keyword{datasets}
