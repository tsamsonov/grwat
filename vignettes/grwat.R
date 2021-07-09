## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(sf) # reading and manipulating spatial data
library(tidyverse) # general data wrangling
library(mapview) # interactive mapping of spatial data
library(ecmwfr) # this is to access ERA5 reanalysis data
library(grwat)

mapviewOptions(fgb = FALSE)

# this is path to sample data installed with grwat
path = system.file("extdata", "spas-zagorye.txt", package = "grwat")

# for your own data just provide the full path:
# path = /this/is/the/path/to/discharge/discharge.csv

hdata = read_delim(path, col_names = c('d', 'm', 'y', 'q'), col_types = 'iiid', delim = ' ') # read gauge data
head(hdata) # see the data

## -----------------------------------------------------------------------------
hdata_alt = hdata %>% 
  transmute(D = lubridate::make_date(y, m, d), 
            Q = q)
head(hdata_alt)

## ---- out.width='100%'--------------------------------------------------------
# this is path to sample basin geopackage installed with grwat
path = system.file("extdata", "spas-zagorye.gpkg", package = "grwat")

# for your own data just provide the full path:
# path = /this/is/the/path/to/discharge/basin.shp

basin = st_read(path, layer = 'basin') # read basin region
mapview(basin)

## ---- out.width='100%'--------------------------------------------------------
basin_buffer = gr_buffer_geo(basin, 25000) 
mapview(basin_buffer, col.regions = 'red') +
  mapview(basin)

## ---- out.width='100%'--------------------------------------------------------
gauge = st_read(path, layer = 'gauge') # read gauge point
gauge_buffer = gr_buffer_geo(gauge, 50000) 
mapview(gauge_buffer, col.regions = 'red') +
  mapview(gauge)

## ---- echo=FALSE, out.width='50%'---------------------------------------------
knitr::include_graphics('img/reanalysis.png')

## ---- echo=FALSE, out.width='100%'--------------------------------------------
knitr::include_graphics('img/rean_files.png')

## -----------------------------------------------------------------------------
rean = gr_read_rean('/Volumes/Data/Spatial/Reanalysis/grwat/pre_1880-2021.nc',
                    '/Volumes/Data/Spatial/Reanalysis/grwat/temp_1880-2021.nc') # read reanalysis data
hdata_rean = gr_join_rean(hdata, rean, basin_buffer) # join reanalysis data to hydrological series

head(hdata_rean$df)

## ---- out.width='100%'--------------------------------------------------------
 # plot spatial configuration
m = mapview(basin_buffer, col.regions = 'red') +
  mapview(basin) +
  mapview(hdata_rean$pts, col.regions = 'black') +
  mapview(rean$pts, cex = 1)

box = st_bbox(basin_buffer)
center = st_coordinates(st_centroid(basin_buffer))

m@map %>% leaflet::setView(center[1], center[2], zoom = 7)

## -----------------------------------------------------------------------------
tab = gr_fill_gaps(hdata_rean$df,
                   nobserv = 10)

tab = gr_fill_gaps(hdata_rean$df,
                   autocorr = 0.7)

## ---- out.width='100%', fig.width = 8, fig.height = 6, dpi = 300--------------
afun = acf(hdata_rean$df$Q)
abline(h = 0.7, col = 'red')
abline(v = purrr::detect_index(afun$acf, ~ .x < 0.7), col = 'blue', lwd = 2)

## ---- out.width='100%', fig.width = 8, fig.height = 6, dpi = 300--------------
resbase = tab %>% 
  mutate(Date = lubridate::dmy(paste(Day, Month, Year))) %>% 
  mutate(Qbase = gr_baseflow(Q, method = 'lynehollick'))

# quick look at the table
head(resbase, 10)
  
resbase %>% 
  filter(Year == 2020) %>% 
  ggplot() +
  geom_area(aes(Date, Q), fill = 'steelblue') +
  geom_area(aes(Date, Qbase), fill = 'orangered')

## ---- out.width='100%', fig.width = 8, fig.height = 6, dpi = 300--------------
resbase = tab %>% 
  mutate(Date = lubridate::dmy(paste(Day, Month, Year))) %>% 
  mutate(Qbase = gr_baseflow(Q, method = 'lynehollick', a = 0.95, passes = 5))
  
resbase %>% 
  filter(Year == 2020) %>% 
  ggplot() +
  geom_area(aes(Date, Q), fill = 'steelblue') +
  geom_area(aes(Date, Qbase), fill = 'orangered')

