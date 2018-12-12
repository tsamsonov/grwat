#' Read ERA-INTERIM reanalysis NetCDF files with temperature and precipitation
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
read_interim <- function(file_prec, file_temp){
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

#' Read file with hydrograph variables
#'
#' @param total_file Path to a file with hydrograph characteristics variables
#'
#' @return data.frame
#' @export
read_variables <- function(file_tot){
  
  params = params_out %>% 
    dplyr::filter(Source == 1) %>% 
    dplyr::arrange(ID)
  
  col_collectors = params %>% 
    dplyr::pull(Readtype) %>% 
    lapply(get_col_type) # TODO: safe loading of parameters
  
  # excl = which(params_out$Source == 0) # exclude calculated variables
  
  total = readr::read_fwf(file_tot, 
                   readr::fwf_widths(widths = params$Width, 
                                     col_names = params$Name), 
                   col_types = col_collectors,
                   skip = 1)
  
  total = total %>%
    dplyr::mutate(monmmsummer = lubridate::ymd(paste("2000", monmmsummer, "01")),
                  nommwin = lubridate::ymd(paste("2000", nommwin, "01")))
  
  total$DaysPavsSum[abs(total$DaysPavsSum) > 300] = 0
  total$DaysThawWin[abs(total$DaysThawWin) > 300] = 0
  
  # New variables
  total$Year2 = c(total$Year1[-1], tail(total$Year1,1))
  total$PolProd = as.integer(total$datepolend - total$datestart)
  
  return(total)
}

#' Read file with hydrograph separation
#'
#' @param file_sep Path to a file with hydrograph separation data
#'
#' @return data.frame
#' @export
read_separation <- function(file_sep){
  readr::read_fwf(file = file_sep, 
           skip = 1,
           readr::fwf_widths(c(16, rep(10, 7)), 
                      c("Date", "Qin", "Qgr", "Qpol", 
                        "Qpav", "Qthaw", "Tin", "Pin")),
           col_types = readr::cols(
             Date = readr::col_date(format = "%d%.%m%.%Y"),
             Qin = readr::col_double(),
             Qgr = readr::col_double(),
             Qpol = readr::col_double(),
             Qpav = readr::col_double(),
             Qthaw = readr::col_double()
           )
  )
}