#' Read reanalysis data
#' 
#' The function reads meteorological variables (temperature and precipitation) from [grwat reanalysis](https://carto.geogr.msu.ru/grwat/) for using with [gr_join_rean()]. Reanalysis covers the East European Plain with 0.75 degrees spatial resolution and is obtained based on CIRES-DOE (1880-1949) and ERA5 (1950-2021) data.
#' 
#' Download the reanalysis archive from [here](https://www.dropbox.com/sh/5xjnf620tlwfk7a/AABhTaPEDWLII8rV04dp0MWna?dl=0).
#'
#' @param file_prec Character string path to precipitation NetCDF file.
#' @param file_temp Character string path to temperature NetCDF file.
#'
#' @return `list` containing time series, precipitation series, temperature series 
#'   and spatial points (sf)
#' @export
#'
#' @example inst/examples/gr_read_rean.R
#' 
gr_read_rean <- function(file_prec, file_temp){
  
  rlang::check_installed(c("ncdf4", "sf"), reason = "to use `gr_read_rean()`")

  # Read NetCDF data
  precip = ncdf4::nc_open(file_prec)
  temps = ncdf4::nc_open(file_temp)
  prate = ncdf4::ncvar_get(precip, precip$var$prate) * 86400
  temp = ncdf4::ncvar_get(temps, temps$var$air) - 273.15

  # Number of days and their values
  ndays.full = temps$dim$time$len
  vals.full = temps$dim$time$vals

  # Reconstruct spatial points
  lat = precip$dim$lat$vals
  lon = precip$dim$lon$vals
  lonidx = 1:length(lon)
  latidx = 1:length(lat)
  coords = expand.grid(lon, lat)
  idx = expand.grid(lonidx, latidx)
  data = data.frame(idx, coords)
  colnames(data) = c('nlon', 'nlat', 'lon', 'lat')

  pts = sf::st_as_sf(data, coords = c("lon", "lat"), crs = 4326)

  return(list(vals.full = vals.full,
              prate = prate,
              temp = temp,
              pts = pts)
  )
}