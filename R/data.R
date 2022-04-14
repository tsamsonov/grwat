#' Spas-Zagorye daily runoff data
#'
#' A dataset containing the daily runoff data for [Spas-Zagorye](https://allrivers.info/gauge/protva-obninsk) gauge on [Protva](https://en.wikipedia.org/wiki/Protva) river in Central European plane. The dataset is supplemented by meteorological variables (temperature and precipitation) obtained from CIRES-DOE (1880-1949) and ERA5 (1950-2021) data. 
#'
#' @format A data frame with 23742 rows and 4 variables:
#' \describe{
#'   \item{Date}{date, in dates}
#'   \item{Q}{daily runoff, in m3/s}
#'   \item{Temp}{daily temperature, in Celsius degrees}
#'   \item{Prec}{daily precipitation, in mm}
#' }
#' 
#' @source \url{https://allrivers.info/gauge/protva-obninsk}
#' @source \url{https://gmvo.skniivh.ru}
#' @source \url{http://meteo.ru}
#' @source \url{https://www.ecmwf.int/en/forecasts/dataset/ecmwf-reanalysis-v5}
#' @source \url{https://psl.noaa.gov/data/gridded/data.20thC_ReanV3.html}
#' 
"spas"