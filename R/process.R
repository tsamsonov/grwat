#' Quasi-geographic buffering
#' 
#' Generate the buffer of spatial object in geographic coordinates. The function transforms the object into Azimuthal equidistant projection, then buffers it by the specified radius and then reprojects into geographical coordinate system (WGS84)
#'
#' @param g `sf` or `sfg` object with known coordinate system.
#' @param bufsize Numeric value of a buffer distance, in meters.
#'
#' @return `sf` or `sfg` object, buffered to `bufsize` and projected into geographic coordinates (WGS84).
#' @export
#'
#' @example inst/examples/gr_buffer_geo.R
#' 
gr_buffer_geo <- function(g, bufsize){
  box = sf::st_bbox(g)
  lon0 = 0.5 * (box[1] + box[3]) # longitude
  lat0 = 0.5 * (box[2] + box[4]) # latitude
  prj = stringr::str_interp('+proj=aeqd +lat_0=${lat0} +lon_0=${lon0} +x_0=0 +y_0=0 +datum=WGS84')
  
  g %>% 
    sf::st_transform(prj) %>% 
    sf::st_buffer(bufsize) %>% 
    sf::st_transform(4326)
}


#' Get gaps in the runoff data
#' 
#' Use the function to detect periods of missing data. These can be both missing dates and missing runoff values
#'
#' @param hdata `data.frame` with two columns, where the first column is `Date`, and the second column is numeric runoff value
#'
#' @return `data.frame` with periods of data and periods of gaps
#' @export
#'
#' @example inst/examples/gr_get_gaps.R
#' 
gr_get_gaps <- function(hdata) {
  hdata %>% 
    dplyr::rename(Date = 1) %>% 
    dplyr::filter(!is.na(Date)) %>% 
    tidyr::complete(Date = seq(min(Date, na.rm = T), max(Date, na.rm = T), by = 'day')) %>% 
    dplyr::mutate(type = dplyr::if_else(complete.cases(.[-1]), 'data', 'gap'),
                  num = with(rle(type), rep(seq_along(lengths), lengths))) %>% 
    dplyr::group_by(num) %>% 
    dplyr::summarise(start_date = min(Date),
                     end_date = max(Date),
                     duration = end_date - start_date + 1,
                     type = dplyr::first(type))
}
  
#' Fill missing runoff data
#' 
#' Use the function to fill the missing runoff data by linear interpolation. These can be both missing dates and missing runoff values. A preliminary summary of missing data can be viewed by [grwat::gr_get_gaps()]
#'
#' @param hdata `data.frame` with two columns, where the first column is `Date`, and the second column is numeric runoff value.
#' @param autocorr Autocorrelation value that defines possible length of the period that can be filled. Defaults to 0.7.  If `nobserv` parameter is set, then this parameter is ignored. If both parameters are `NULL`, then all gaps are filled disregard of their lengths (not recommended).
#' @param nobserv Maximum number of contiguous observations that can be interpolated. Defaults to `NULL`. If this parameter is set, then `autocorr` parameter is ignored. If both parameters are `NULL`, then all gaps are filled disregard of their lengths (not recommended).
#'
#' @return `data.frame` which is a filled version of `hdata`
#' @export
#'
#' @example inst/examples/gr_fill_gaps.R
#' 
gr_fill_gaps <- function(hdata, autocorr = 0.7, nobserv = NULL) {
  
  if (!lubridate::is.Date(hdata[[1]]))
    stop(crayon::white$bgRed$bold('grwat:'), ' the first column of data frame must have Date type')
  
  nms = colnames(hdata)[-1]
  
  tab = hdata %>% 
    dplyr::rename(Date = 1) %>% 
    dplyr::filter(!is.na(Date)) %>% 
    tidyr::complete(Date = seq(min(Date, na.rm = T), max(Date, na.rm = T), by = 'day'))
  
  # Calculate via autocorrelation
  if (is.null(nobserv)) {
    timerep = tab %>% 
      dplyr::mutate(type = dplyr::if_else(complete.cases(tab[-1]), 'data', 'gap'),
             num = with(rle(type), rep(seq_along(lengths), lengths))) %>% 
      dplyr::group_by(num) %>% 
      dplyr::summarise(start_date = min(Date),
                end_date = max(Date),
                duration = end_date - start_date + 1,
                type = dplyr::first(type))
    
    max_period = dplyr::filter(timerep, type == 'data', duration == max(duration))
    
    tab_afun = dplyr::filter(tab, dplyr::between(Date, max_period$start_date, max_period$end_date))
    
    nobserv = sapply(2:ncol(tab_afun), function(i) {
      afun = tab_afun %>% 
        dplyr::pull(i) %>% 
        acf(plot = FALSE)
      
      purrr::detect_index(afun$acf, ~ .x < autocorr) - 1
    }) %>% setNames(nms)
  } else {
    nvars = ncol(hdata)-1
    if (length(nobserv) == 1) {
      nobserv = as.list(rep(nobserv, nvars)) %>% setNames(nms)
    } else if (length(nobserv) == nvars) {
      nobserv = as.list(nobserv)%>% setNames(nms)
    } else {
      stop(crayon::white$bgRed$bold('grwat:'), ' nobserv parameter should have length 1 or ', 
           length(df)-1, ' but ', length(nobserv), ' elements are given.')
    }
  }
  
  tabres = tab %>%
    dplyr::mutate(across(2:ncol(tab), 
                         ~zoo::na.approx(tab[[dplyr::cur_column()]], 
                                         maxgap = nobserv[dplyr::cur_column()], na.rm = FALSE))) %>% 
    setNames(colnames(hdata))
  
  message(crayon::white$bold('grwat:'), ' filled ', 
          sum(complete.cases(tabres)) - sum(complete.cases(tab)), 
          ' observations using ', paste(nobserv, collapse = ', '), ' days window for each variable')
  
  return(tabres)
  
}

#' Join reanalysis data
#' 
#' The function performs spatial join of meteorological variables (temperature and precipitation) from [grwat reanalysis](http://carto.geogr.msu.ru/grwat/) to the daily runoff time series. Reanalysis covers the East European Plain with 0.75 degrees spatial resolution and is obtained based on CIRES-DOE (1880-1949) and ERA5 (1950-2021) data. This function is useful when the data from meteorological stations are missing inside the basin.
#' 
#' Download the reanalysis archive from [http://carto.geogr.msu.ru/grwat/].
#'
#' @param hdata `data.frame` containing 2 columns: `Date` and runoff
#' @param rean `list` as returned by [grwat::gr_read_rean()]
#' @param buffer `sf` object containing the region to select reanalysis data. Usually a river basin is used to select the meteorological data. Use [grwat::gr_buffer_geo()] to buffer the basin by specified distance and get more data, if needed.
#'
#' @return `data.frame` with four columns: date, runoff, temperature, precipitation.
#' @export
#'
#' @example inst/examples/gr_join_rean.R
#' 
gr_join_rean <- function(hdata, rean, buffer){
  
  # determine the first and last date
  date_first = min(hdata[[1]], na.rm = T)
  date_last = max(hdata[[1]], na.rm = T)
  
  # generate sequence of dates
  hdates = seq(date_first, date_last, "days")
  ndates = length(hdates)
   
  # Select points
  pts_selected = rean$pts[buffer, ]
  npts = nrow(pts_selected)
  
  sum_table_with_dates = NULL
  
  if (npts > 0){
    # Extract point numbers for subsetting ncdf array
    pts_numbers = pts_selected %>% 
      dplyr::select(nlon, nlat) %>%
      sf::st_drop_geometry()
    # st_geometry(pts_numbers) <- NULL
    
    # Extract dates for subsetting ncdf array
    datevals = lubridate::ymd(18000101) + lubridate::hours(rean$vals.full)
    flt = (datevals >= hdates[1]) & (datevals <= hdates[length(hdates)]) # !!!!
    days = seq_along(rean$vals.full)[flt]
    ndays = length(days)
    
    if (ndays == 0) {
      stop(crayon::white$bgRed$bold('grwat:'), ' no reanalysis data for hydrological series time period')
    } else if (ndays < ndates) {
      date_first = min(datevals[flt])
      date_last = max(datevals[flt])
      hdates = seq(date_first, date_last, "days")
      ndates = length(hdates)
      warning(crayon::white$bgBlue$bold('grwat:'), ' reanalysis data does not cover the full hydrological series time period, only common dates are kept')
    }
    
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
      dplyr::mutate(Date = hdates) %>% 
      dplyr::rename_at(4, ~ names(hdata)[[1]]) %>% 
      dplyr::select(-1)
    
    sum_table_with_dates = hdata %>%
      dplyr::inner_join(sum_table)
    
  } else {
    stop(crayon::white$bgRed$bold('grwat:'), ' no reanalysis data for requested location')
  }
  
  return(sum_table_with_dates)
}
