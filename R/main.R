#' Join reanalysis data based on geographic location and time
#'
#' @param wd Character. Working directory with specified structure
#' @param bufsize Size of a buffer that is used to select reanalysis data
#'   around each basin
#'
#' @return Generates water level files enriched with reanalysis data.
#'   Produces text reports on a number of a processed gauges in each basin
#' @export
#'
#' @examples
join_reanalysis <- function(wd, bufsize = 50000){
  old = setwd(wd)
  on.exit(setwd(old), add = TRUE)
  
  # Read NetCDF data
  precip = nc_open("rean/prec.nc")
  temps = nc_open("rean/temp.nc")
  prate = ncvar_get(precip, precip$var$prate) * 86400
  temp = ncvar_get(temps, temps$var$air) - 273.15
  
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
  
  pts = st_as_sf(data, coords = c("lon", "lat"), crs = 4326)
  
  # clear previous data
  unlink('out', recursive = TRUE)
  dir.create('out')
  file.copy('in/.', 'out', recursive = TRUE)
  
  wd = paste0(wd, 'out/')
  setwd(wd)
  
  # list basins
  basins = list.dirs(recursive = FALSE, full.names = FALSE)
  reports = vector('list', length = length(basins))
  
  i = 1
  for (basin in basins) {
    setwd(wd)
    setwd(basin)
    gauges = list.dirs(recursive = FALSE, full.names = FALSE)
    
    reanpts = vector(mode = "integer", length(gauges))
    j = 1
    
    for (gauge in gauges){
      setwd(gauge)
      
      hydro.file = list.files('.','.txt')[1]
      
      input_name = strsplit(hydro.file, '[.]')[[1]][1]
      
      hdata = NULL
      
      try(hdata <- read.csv(hydro.file, sep = ' ', header = FALSE), silent = TRUE)
      
      if(is.null(hdata)){
        reanpts[j] = -1
        warning("Failed to calculate statistics for ", gauge, ". Error reading input file")
      } else {
        
        # determine the first and last date
        first = hdata[1, 1:3]
        last = hdata[nrow(hdata), 1:3]
        first.date = paste(first[3], first[2], first[1], sep = "/")
        last.date = paste(last[3], last[2], last[1], sep = "/")
        
        # generate sequence of dates
        hdates = seq(as.Date(first.date), as.Date(last.date), "days")
        ndates = length(hdates)
        
        # read only first file with .shp extension
        shapes = list.files("Shape")
        flt = grep(".shp$", shapes, perl = TRUE)
        region = str_interp('Shape/${shapes[flt][1]}') %>% st_read() 
        
        # buffer region to select more points
        buffer = region %>% 
          st_transform_opt() %>% # optimal projection
          st_buffer(bufsize) %>% 
          st_transform(4326)
        
        # frame for visualizing points
        frame = st_bbox(buffer) %>% as.numeric()
        frame_sf = st_bbox(buffer) %>% st_as_sfc()
        
        # Select points
        pts.selected = pts[buffer, ]
        npts = nrow(pts.selected)
        reanpts[j] = npts
        
        if (npts < 1){
          warning("Failed to calculate statistics for ", gauge, ". No reanalysis data available")
        } else {
          
          CairoPNG(str_interp('${gauge}.png'),  
                   height = 5, width = 5, 
                   units = 'in', dpi = 300)
          
          par_default = par(no.readonly = TRUE)
          par(mfrow = c(1,1))
          
          
          plot(buffer %>% st_geometry(), col = rgb(1, 0, 0, 0.2), border = rgb(1, 0, 0), lwd = 0.5)
          plot(region, col = rgb(1, 0, 0, 0.5), border = rgb(1, 0, 0), add = TRUE)
          plot(rivers, col = 'steelblue4', lwd = 0.5, add = TRUE)
          plot(rivers_europe, col = 'steelblue4', lwd = 0.2, add = TRUE)
          plot(lakes, border = 'steelblue4', col = 'skyblue', lwd = 0.2, add = TRUE)
          plot(lakes_europe, border = 'steelblue4', col = 'skyblue', lwd = 0.2, add = TRUE)
          plot(ocean, border = 'steelblue4', col = 'skyblue', lwd = 0.5, add = TRUE)
          plot(pts, pch = 19, col = 'gray30', cex = 0.3, add = TRUE)
          plot(pts.selected, pch = 19, col = 'black', cex = 0.7, add = TRUE)
          box(lwd = 0.2, col = 'black')
          
          par(par_default)
          
          dev.off()
          
          # Extract point numbers for subsetting ncdf array
          pts.numbers = pts.selected %>% select(nlon, nlat)
          st_geometry(pts.numbers) = NULL
          
          # Extract dates for subsetting ncdf array
          datevals = ymd(18000101) + hours(vals.full)
          flt = (datevals >= hdates[1]) & (datevals <= hdates[length(hdates)]) # !!!!
          days = (1:ndays.full)[flt]
          ndays = length(days)
          
          # Replicate position each day
          pts.positions = sapply(pts.numbers, rep.int, times = ndays)
          pts.days = rep(days, each = npts)
          
          # Generate index table for subsetting
          selection.table = data.frame(pts.positions, pts.days)
          
          # Subset data
          temp.selected = temp[as.matrix(selection.table)]
          prate.selected = prate[as.matrix(selection.table)]
          result = data.frame(selection.table, temp.selected, prate.selected)
          
          # calculate average temp and prate per day
          sum.table = result %>%
            group_by(pts.days) %>% 
            summarise(mean_temp = mean(temp.selected) %>% round(2),
                      mean_prate = mean(prate.selected) %>% round(3))
          
          # split dates into three columns
          dates.matrix = hdates %>% 
            as.character() %>% 
            lapply(strsplit, split = '-') %>% 
            unlist() %>% 
            as.integer() %>% 
            matrix(ncol = 3, byrow = TRUE)
          
          # prepare output table
          sum.table.with.dates = data.frame(sum.table[,1], dates.matrix, sum.table[,2:3])
          colnames(sum.table.with.dates) = c("N", "Y", "M", "D", "T", "P")
          
          # write output files
          sum.table.with.dates %>% 
            select(`T`, P) %>% 
            write_delim(path = paste(input_name, "_rean.txt", sep = ""),
                        delim = " ",
                        col_names = FALSE
            )
          sum.table.with.dates %>% 
            select(D, M, Y, `T`, P) %>% 
            write_delim(path = paste(input_name, "_rean_ymd.txt", sep = ""),
                        delim = " ",
                        col_names = FALSE
            )
        }
      }
      
      setwd(wd)
      setwd(basin)
      j = j + 1
    }
    
    reports[[i]] = data.frame(gauge = gauges, reanpts)
    write_delim(reports[[i]], "summary.txt")
    i = i + 1
  }
  
  setwd(wd)
  
  # export large report for all basins
  ngauges = sapply(reports, nrow)
  
  report = do.call(rbind, reports) %>% 
    mutate(basin = rep(basins, ngauges)) %>% 
    select(basin, gauge, reanpts)
  
  write_delim(report, "summary.txt") 
}

#' Generate reports with detailed hydrograph analysis
#'
#' @param wd Character. A working directory with specified structure
#'
#' @return Generates a new out working directory with detailed reports
#' @export
#'
#' @examples
report <- function(wd){
  # list basins
  old = setwd(wd)
  on.exit(setwd(old), add = TRUE)
  
  basins = list.dirs(recursive = FALSE, full.names = FALSE)
  
  # Generate reports for each gauge
  for (basin in basins) {
    setwd(wd)
    setwd(basin)
    gauges = list.dirs(recursive = FALSE, full.names = FALSE)
    
    for (gauge in gauges){
      setwd(gauge)
      
      fullpath = getwd()
      
      rmarkdown::render(input = system.file('reports', 'Report.Rmd', package = 'grwat'), 
                        output_file = 'report.pdf',
                        output_dir = fullpath,
                        knit_root_dir = fullpath,
                        encoding = 'UTF-8',
                        params = list(name = gauge, namen = gauge))
      setwd(wd)
      setwd(basin)
      break
    }
    break
  } 
}