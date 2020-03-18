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
#' st_buffer_geo(basin, 50000)
#' }
st_buffer_geo <- function(g, bufsize){
  # TODO: create geodetic buffering instead
  g %>% 
    st_transform_opt() %>%
    sf::st_buffer(bufsize) %>% 
    sf::st_transform(4326)
}

#' Fill missing water discharge data using linear interpolation
#'
#' @param hdata Water discharge series. Data frame containing 4 columns: year (YYYY), montth (MM), day (DD) and level (water level)
#' @param autocorr Autocorrelation value that defines possible length of the period
#' @param max_dur A number of obsrvations to fill
#' @param expand Should the algorithm insert missing dates?
#'
#' @return filled discharge values
#' @export
#'
#' @examples
fill_gaps <- function(hdata, autocorr = 0.7, nobserv = NULL, expand = TRUE, dates = FALSE) {
  
  tab = hdata
  
  if (!dates) {
    tab = hdata %>%
      mutate(Date = ymd(paste(year, month, day))) %>% 
      dplyr::filter(!is.na(Date)) %>% 
      complete(Date = seq(min(Date, na.rm = T), max(Date, na.rm = T), by = 'day'))
  }
  
  # Calculate via autocorrelation
  if (is.null(nobserv)) {
    
    timerep = tab %>% 
      mutate(type = if_else(is.na(level), 'gap', 'data'),
             num = with(rle(type), rep(seq_along(lengths), lengths))) %>% 
      group_by(num) %>% 
      summarise(start_date = min(Date),
                end_date = max(Date),
                duration = end_date - start_date + 1,
                type = first(type))
    
    max_period = dplyr::filter(timerep, type == 'data', duration == max(duration))
    
    afun = tab %>% 
      filter(between(Date, max_period$start_date, max_period$end_date)) %>% 
      pull(level) %>% 
      acf()
    
    nobserv = purrr::detect_index(afun$acf, ~ .x < autocorr) 
  
  }
  
  tab_interp = tab %>% 
    mutate(level_interp = zoo::na.approx(level, maxgap = nobserv) %>% round(1))
  
  return(tab_interp)
  
}

#' Join ERA-INTERIM data to hydrological level series
#'
#' @param hdata Water discharge series. Data frame containing 4 columns: YYYY, MM, DD and water level
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
join_interim <- function(hdata, rean, buffer){
  
  hdata = hdata[, 1:4]
  colnames(hdata) <- c('D', 'M', 'Y', 'L')
  
  # determine the first and last date
  first = hdata[1, 1:3]
  last = hdata[nrow(hdata), 1:3]
  first.date = paste(first[3], first[2], first[1], sep = "/")
  last.date = paste(last[3], last[2], last[1], sep = "/")
  
  # generate sequence of dates
  hdates = seq(as.Date(first.date), as.Date(last.date), "days")
  ndates = length(hdates)
  
  # frame for visualizing points
  frame = sf::st_bbox(buffer) %>% as.numeric()
  frame_sf = sf::st_bbox(buffer) %>% st_as_sfc()
   
  # Select points
  pts.selected = rean$pts[buffer, ]
  npts = nrow(pts.selected)
  
  sum.table.with.dates = NULL
  
  if (npts > 0){
    # Extract point numbers for subsetting ncdf array
    pts.numbers = pts.selected %>% dplyr::select(nlon, nlat)
    st_geometry(pts.numbers) <- NULL
    
    # Extract dates for subsetting ncdf array
    datevals = lubridate::ymd(18000101) + lubridate::hours(rean$vals.full)
    flt = (datevals >= hdates[1]) & (datevals <= hdates[length(hdates)]) # !!!!
    days = (1:length(rean$vals.full))[flt]
    ndays = length(days)
    
    # Replicate position each day
    pts.positions = sapply(pts.numbers, rep.int, times = ndays)
    pts.days = rep(days, each = npts)
    
    # Generate index table for subsetting
    selection.table = data.frame(pts.positions, pts.days)
    
    # Subset data
    temp.selected = rean$temp[as.matrix(selection.table)]
    prate.selected = rean$prate[as.matrix(selection.table)]
    result = data.frame(selection.table, temp.selected, prate.selected)
    
    # calculate average temp and prate per day
    sum.table = result %>%
      dplyr::group_by(pts.days) %>% 
      dplyr::summarise(mean_temp = mean(temp.selected) %>% round(2),
                mean_prate = mean(prate.selected) %>% round(3))
    
    # split dates into three columns
    dates.matrix = hdates %>% 
      as.character() %>% 
      lapply(strsplit, split = '-') %>% 
      unlist() %>% 
      as.integer() %>% 
      matrix(ncol = 3, byrow = TRUE)
    
    # prepare output table
    sum.table.with.dates = data.frame(sum.table[, 1], dates.matrix, sum.table[, 2:3])
    colnames(sum.table.with.dates) <- c("N", "Y", "M", "D", "T", "P")
    
    sum.table.with.dates = sum.table.with.dates %>% dplyr::left_join(hdata)
  }
  
  return(list(df = sum.table.with.dates, pts = pts.selected))
}

#' Process gauge directory
#'
#' @param wd Path to a directory containing water level series data and basin region
#' @param rean List with reanalysis data (generated by read_interim function)
#' @param bufsize Buffer distance to select additional points around basin region
#' @param eol The symbol used to specify end of line. Defaults to "\\n", which will work well on most of the systems. On some Windows setups, an alternative "\\r\\n" must be used if the standard option does not work.
#'
#' @return  Number of reanalysis points processed for buffered basin. All other
#'   operations are performed inside a directory
#' @export
#'
#' @examples
#' \dontrun{
#' grwat::process_gauge(wd, rean, 50000)
#' }
process_gauge <- function(wd, rean, bufsize=50000, eol="\n"){
  
  oldwd = setwd(wd)
  on.exit(setwd(oldwd), add = TRUE)
  
  hydro.file = list.files('.','.txt')[1]
  input_name = strsplit(hydro.file, '[.]')[[1]][1]
  
  reanpts = -1
  hdata = NULL
  try(hdata <- read.csv(hydro.file, sep = ' ', header = FALSE), silent = TRUE)
  
  if(is.null(hdata)){
    warning("Failed to calculate statistics for ", wd, ". Error reading input file")
  } else {
    
    # read only first file with .shp extension
    shapes = list.files()
    flt = grep("\\.gpkg$|\\.shp$", shapes, perl = TRUE)
    
    if (length(flt) == 0){
      warning('Cannot find a geopackage with basin border')
    } else {
      
      region = st_read(shapes[flt][1], quiet = TRUE) # select only first geopackage
      
      # buffer region to select more points
      buffer = grwat::st_buffer_geo(region, bufsize)
      
      results = grwat::join_interim(hdata, rean, buffer) # TODO: nice processing here
      
      sum.table.with.dates = results[[1]]
      pts.selected = results[[2]]
      
      if (is.null(pts.selected)){
        warning("Failed to calculate statistics for ", gauge, ". No reanalysis data available")
      } else {
        
        reanpts = nrow(pts.selected)
        
        Cairo::CairoPNG(paste0(basename(getwd()), '.png'), 
                 height = 5, width = 5, 
                 units = 'in', dpi = 300)
        
        par_default = par(no.readonly = TRUE)
        par(mfrow = c(1,1))
        
        grwat::map(rean$pts, pts.selected, region, buffer)
        
        par(par_default)
        
        dev.off()
        
        # write output files
        sum.table.with.dates %>% 
          dplyr::select(`T`, P) %>% 
          # readr::write_delim(path = paste(input_name, "_rean.txt", sep = ""),
          #                    delim = " ",
          #                    col_names = FALSE)
          write.table(file = paste(input_name, "_rean.txt", sep = ""), 
                      sep = " ",
                      eol = eol,
                      row.names = FALSE,
                      col.names = FALSE)
        sum.table.with.dates %>% 
          dplyr::select(D, M, Y, `T`, P) %>% 
          # readr::write_delim(path = paste(input_name, "_rean_ymd.txt", sep = ""),
          #             delim = " ",
          #             col_names = FALSE)
          write.table(file = paste(input_name, "_rean_ymd.txt", sep = ""), 
                      sep = " ",
                      eol = eol,
                      row.names = FALSE,
                      col.names = FALSE)
      } 
    }
  }
  
  return(reanpts)
}

#' Join reanalysis data based on geographic location and time
#'
#' @param wd Path to a working directory with specified structure
#' @param rean Reanalysis data (as read by `read_interim()`)
#' @param bufsize Size of a buffer that is used to select reanalysis data
#'   around each basin
#' @param eol The symbol used to specify end of line. Defaults to "\\n", which will work well on most of the systems. On some Windows setups, an alternative "\\r\\n" must be used if the standard option does not work.
#' @param clear Boolean. Whether to remove out directory before processing. Defaults to TRUE
#'
#' @return Generates water level files enriched with reanalysis data.
#'   Produces text reports on a number of a processed gauges in each basin
#' @export
#'
#' @examples
#' \dontrun{
#' grwat::process_basins(wd, rean, bufsize=50000, clear=TRUE)
#' }
process_basins <- function(wd, rean, bufsize=50000, eol="\n", clear=TRUE){
  
  old = setwd(wd)
  on.exit(setwd(old), add = TRUE)
  
  # clear previous data
  if(clear) {
    unlink('out', recursive = TRUE)
    dir.create('out')
  }
  
  file.copy('in/.', 'out', recursive = TRUE)
  
  wd = paste0(getwd(), '/out/')
  setwd(wd)
  
  # list basins
  basins = list.dirs(recursive = FALSE, full.names = FALSE)
  
  nbasins = length(basins)
  reports = vector('list', length = nbasins)
  
  i = 1
  
  for (basin in basins) {
    setwd(wd)
    setwd(basin)
    gauges = list.dirs(recursive = FALSE, full.names = FALSE)
    
    reanpts = vector(mode = "integer", length(gauges))
    j = 1
    
    pb = progress::progress_bar$new(format = stringr::str_interp("Processing basin ${i} of ${nbasins} [:bar] :percent :elapsed"),
                                    total = length(gauges),
                                    show_after = 2)
    pb$tick(0)
    
    for (gauge in gauges){
      reanpts[j] = grwat::process_gauge(gauge, rean, bufsize, eol) # TODO: correct working directory
      j = j + 1
      pb$tick()
    }
    
    reports[[i]] = data.frame(gauge = gauges, reanpts)
    readr::write_delim(reports[[i]], "summary.txt")
    i = i + 1
  }
  
  setwd(wd)
  
  # export large report for all basins
  ngauges = sapply(reports, nrow)
  
  report = do.call(rbind, reports) %>% 
    dplyr::mutate(basin = rep(basins, ngauges)) %>% 
    dplyr::select(basin, gauge, reanpts)
  
  readr::write_delim(report, "summary.txt") 
}