% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process.R
\name{gr_join_rean}
\alias{gr_join_rean}
\title{Join reanalysis data}
\usage{
gr_join_rean(hdata, rean, buffer)
}
\arguments{
\item{hdata}{\code{data.frame} containing 2 columns: \code{Date} and runoff}

\item{rean}{\code{list} as returned by \code{\link[=gr_read_rean]{gr_read_rean()}}}

\item{buffer}{\code{sf} object containing the region to select reanalysis data. Usually a river basin is used to select the meteorological data. Use \code{\link[=gr_buffer_geo]{gr_buffer_geo()}} to buffer the basin by specified distance and get more data, if needed.}
}
\value{
\code{data.frame} with four columns: date, runoff, temperature, precipitation.
}
\description{
The function performs spatial join of meteorological variables (temperature and precipitation) from \href{https://www.dropbox.com/sh/5xjnf620tlwfk7a/AABhTaPEDWLII8rV04dp0MWna?dl=0}{grwat reanalysis} to the daily runoff time series. Reanalysis covers the East European Plain with 0.75 degrees spatial resolution and is obtained based on CIRES-DOE (1880-1949) and ERA5 (1950-2021) data. This function is useful when the data from meteorological stations are missing inside the basin.
}
\details{
Download the reanalysis archive from \href{https://www.dropbox.com/sh/5xjnf620tlwfk7a/AABhTaPEDWLII8rV04dp0MWna?dl=0}{here}.
}
\examples{
if (require("sf") && require("ncdf4")) {

  library(grwat)
  library(dplyr)

  # example Spas-Zagorye daily runoff data is included with grwat package
  data_path = system.file("extdata", "spas-zagorye.txt",
                          package = "grwat")

  hdata_raw = read.delim(data_path, header = FALSE, 
                         sep = ' ', na.strings = c('-999', '-999.0', '-'),
                         col.names = c('d', 'm', 'y', 'q'))

  hdata = hdata_raw \%>\%
    transmute(Date = lubridate::make_date(y, m, d),
              Q = q)

  head(hdata)

  # read basin
  basin_path = system.file("extdata", "spas-zagorye.gpkg",
                           package = "grwat")
  basin = sf::st_read(basin_path, layer = 'basin') # read basin region
  basin_buffer = gr_buffer_geo(basin, 25000)

  \dontrun{
    # read reanalysis data
    rean = gr_read_rean(
      '/Volumes/Data/Spatial/Reanalysis/grwat/pre_1880-2021.nc',
      '/Volumes/Data/Spatial/Reanalysis/grwat/temp_1880-2021.nc'
    )

    # spatial join of reanalysis data to runoff data
    hdata_rean = gr_join_rean(hdata, rean, basin_buffer)

    head(hdata_rean)
  }

}
}
