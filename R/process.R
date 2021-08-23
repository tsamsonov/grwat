#' Buffer sf objects in geographic coordinates through conic projection
#'
#' @param g `sf` or `sfg` object in geographic coordinates
#' @param bufsize double. Size of a buffer, in metres
#'
#' @return `sf` or `sfg` object, buffered to `bufsize` distance
#' @export
#'
#' @examples
#' \dontrun{
#' gr_buffer_geo(basin, 50000)
#' }
gr_buffer_geo <- function(g, bufsize){
  # TODO: create geodetic buffering instead
  g %>% 
    st_transform_opt() %>%
    sf::st_buffer(bufsize) %>% 
    sf::st_transform(4326)
}


#' Get gaps in the data
#'
#' @param hdata a data frame where the first column is date, and the second column is discharge value
#'
#' @return a data frame with periods of data and periods of gaps
#' @export
#'
#' @examples
gr_get_gaps <- function(hdata) {
  hdata %>% 
    dplyr::rename(Date = 1, Q = 2) %>% 
    dplyr::filter(!is.na(Date)) %>% 
    tidyr::complete(Date = seq(min(Date, na.rm = T), max(Date, na.rm = T), by = 'day')) %>% 
    mutate(type = if_else(is.na(Q), 'gap', 'data'),
           num = with(rle(type), rep(seq_along(lengths), lengths))) %>% 
    group_by(num) %>% 
    summarise(start_date = min(Date),
              end_date = max(Date),
              duration = end_date - start_date + 1,
              type = first(type))
}
  
#' Fill missing water discharge data using linear interpolation
#'
#' @param hdata Water discharge series. Data frame containing 4 columns: year (YYYY), montth (MM), day (DD) and level (water level)
#' @param autocorr Autocorrelation value that defines possible length of the period. Defaults to 0.7.  If `nobserv` parameter is set, then this parameter is ignored. If both parameters are `NULL`, then all gaps are filled disregard of their lengths (not recommended).
#' @param nobserv Maximum number of contiguous observations that can be filled. Defaults to `NULL`. If this parameter is set by the user, then `autocorr` parameter is ignored. If both parameters are `NULL`, then all gaps are filled disregard of their lengths (not recommended).
#' @param expand Should the algorithm insert missing dates?
#' @param dates 
#' @param order 
#'
#' @return filled discharge values
#' @export
#'
#' @examples
gr_fill_gaps <- function(hdata, autocorr = 0.7, nobserv = NULL) {
  
  tab = hdata %>% 
    rename(Date = 1, Q = 2) %>% 
    dplyr::filter(!is.na(Date), !is.na(Q)) %>% 
    tidyr::complete(Date = seq(min(Date, na.rm = T), max(Date, na.rm = T), by = 'day'))
  
  # Calculate via autocorrelation
  if (is.null(nobserv)) {
    
    timerep = tab %>% 
      mutate(type = if_else(is.na(Q), 'gap', 'data'),
             num = with(rle(type), rep(seq_along(lengths), lengths))) %>% 
      group_by(num) %>% 
      summarise(start_date = min(Date),
                end_date = max(Date),
                duration = end_date - start_date + 1,
                type = first(type))
    
    max_period = dplyr::filter(timerep, type == 'data', duration == max(duration))
    
    afun = tab %>% 
      dplyr::filter(between(Date, max_period$start_date, max_period$end_date)) %>% 
      pull(Q) %>% 
      acf(plot = FALSE)
    
    nobserv = purrr::detect_index(afun$acf, ~ .x < autocorr) - 1
  
  }
  
  Qrep = zoo::na.approx(tab$Q, maxgap = nobserv)
  
  message(crayon::white$bold('grwat:'), ' filled ', 
          sum(is.na(tab$Q)) - sum(is.na(Qrep)), ' observations using ', nobserv, ' days window')
  
  tab = tab %>% 
    mutate(Q = Qrep) %>% 
    setNames(names(hdata))
  
  return(tab)
  
}

#' Join grwat reanalysis to hydrological level series
#'
#' @param hdata data frame containing 2 columns: date and discharge level
#' @param rean List. Contains three components: days series, values and location points
#' @param buffer Region to select reanalysis data
#'
#' @return List of two components: water level table enriched with reanalysis data
#'   and selected points
#' @export
#'
#' @examples
#' \dontrun{
#' grwat::process(hdata, rean, buffer)
#' }
gr_join_rean <- function(hdata, rean, buffer){
  
  # determine the first and last date
  date_first = min(hdata[[1]], na.rm = T)
  date_last = max(hdata[[1]], na.rm = T)
  
  # generate sequence of dates
  hdates = seq(date_first, date_last, "days")
  ndates = length(hdates)
  
  # frame for visualizing points
  # frame = sf::st_bbox(buffer) %>% as.numeric()
  # frame_sf = sf::st_bbox(buffer) %>% st_as_sfc()
   
  # Select points
  pts_selected = rean$pts[buffer, ]
  npts = nrow(pts_selected)
  
  sum_table_with_dates = NULL
  
  if (npts > 0){
    # Extract point numbers for subsetting ncdf array
    pts_numbers = pts_selected %>% 
      dplyr::select(nlon, nlat)
    st_geometry(pts_numbers) <- NULL
    
    # Extract dates for subsetting ncdf array
    datevals = lubridate::ymd(18000101) + lubridate::hours(rean$vals.full)
    flt = (datevals >= hdates[1]) & (datevals <= hdates[length(hdates)]) # !!!!
    days = seq_along(rean$vals.full)[flt]
    ndays = length(days)
    
    # Replicate position each day
    pts_positions = sapply(pts_numbers, rep.int, times = ndays)
    pts_days = rep(days, each = npts)
    
    # Generate index table for subsetting
    selection_table = data.frame(pts_positions, pts_days)
    
    # Subset data
    temp_selected = rean$temp[as.matrix(selection_table)]
    prate_selected = rean$prate[as.matrix(selection_table)]
    result = data.frame(selection_table, temp_selected, prate_selected)
    
    # calculate average temp and prate per day
    sum_table = result %>%
      dplyr::group_by(pts_days) %>% 
      dplyr::summarise(Temp = mean(temp_selected) %>% round(2),
                       Prec = mean(prate_selected) %>% round(3)) %>% 
      mutate(Date = hdates) %>% 
      rename_at(4, ~ names(hdata)[[1]]) %>% 
      select(-1)
    
    sum_table_with_dates = hdata %>%
      dplyr::left_join(sum_table)
  }
  
  return(sum_table_with_dates)
}