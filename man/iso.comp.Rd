% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iso.comp.R
\name{iso.comp}
\alias{iso.comp}
\title{Determine the isotopic composition of
\ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}} measurements}
\usage{
iso.comp(dataframe, CO2dry_ppm = "CO2dry_ppm", delta_13C = "Delta_Raw_iCO2")
}
\arguments{
\item{dataframe}{a data.frame containing measurements of isotopic fractions of
\ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}}
and a \code{UniqueID} (or \code{chamID}) per measurements.}

\item{CO2dry_ppm, delta_13C}{character string; specifies which columns should be
used for \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}}
measurements, and the delta 13C, respectively.}
}
\value{
A data frame of the isotopic composition per measurement (UniqueID)
}
\description{
Extracts the y-intercept of the regression between the isotope ratios plotted
againts the inverse of the \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}}
concentration
}
\examples{
data(manID.G2201i)
my.iso.comp <- iso.comp(manID.G2201i)
}
\seealso{
\code{\link[goFlux]{import.G2201i}}
}
