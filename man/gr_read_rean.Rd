% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{gr_read_rean}
\alias{gr_read_rean}
\title{Read grwat daily reanalysis}
\usage{
gr_read_rean(file_prec, file_temp)
}
\arguments{
\item{file_prec}{Precipitation NetCDF file}

\item{file_temp}{Temperature NetCDF file}
}
\value{
List containing time series, precipitation series, temperature series 
  and spatial points (sf)
}
\description{
Read grwat daily reanalysis
}
\examples{
\dontrun{
grwat::read_interim("rean/prec.nc", "rean/temp.nc")
}
}
