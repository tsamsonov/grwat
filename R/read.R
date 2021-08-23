#' Read grwat daily reanalysis
#'
#' @param file_prec Precipitation NetCDF file
#' @param file_temp Temperature NetCDF file
#'
#' @return List containing time series, precipitation series, temperature series 
#'   and spatial points (sf)
#' @export
#'
#' @examples
#' \dontrun{
#' grwat::read_interim("rean/prec.nc", "rean/temp.nc")
#' }
gr_read_rean <- function(file_prec, file_temp){
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