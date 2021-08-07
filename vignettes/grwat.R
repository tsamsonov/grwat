## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

knitr::opts_chunk$set(fig.width = 10, fig.height = 7, out.width = '100%', dpi = 300, collapse = TRUE)
Sys.setlocale("LC_TIME", "en_US.UTF-8")

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

hdata_raw = read_delim(path, 
                   col_names = c('d', 'm', 'y', 'q'), 
                   col_types = 'iiid', delim = ' ') # read gauge data
head(hdata_raw) # see the data

## -----------------------------------------------------------------------------
hdata = hdata_raw %>% 
  transmute(D = lubridate::make_date(y, m, d), 
            Q = q)
head(hdata)

## -----------------------------------------------------------------------------
gaps = gr_get_gaps(hdata)
gaps

## -----------------------------------------------------------------------------
tab = gr_fill_gaps(hdata, autocorr = 0.7)

tab = gr_fill_gaps(hdata, nobserv = 10)

## ---- out.width='80%'---------------------------------------------------------
max_period = gaps %>% 
  filter(type == 'data', duration == max(duration))
    
acf_data = tab %>% 
    filter(between(D, max_period$start_date, max_period$end_date)) %>% 
    pull(Q)

afun = acf(acf_data)
abline(h = 0.7, col = 'red')
abline(v = purrr::detect_index(afun$acf, ~ .x < 0.7), col = 'blue', lwd = 2)

## ---- dpi = 72----------------------------------------------------------------
# this is path to sample basin geopackage installed with grwat
path = system.file("extdata", "spas-zagorye.gpkg", package = "grwat")

# for your own data just provide the full path:
# path = /this/is/the/path/to/discharge/basin.shp

basin = st_read(path, layer = 'basin') # read basin region
gauge = st_read(path, layer = 'gauge') # read gauge point
mapview(basin) + mapview(gauge)

## ---- dpi = 72----------------------------------------------------------------
basin_buffer = gr_buffer_geo(basin, 25000) 
mapview(basin_buffer, col.regions = 'red') +
  mapview(basin)

## ---- dpi = 72----------------------------------------------------------------
gauge_buffer = gr_buffer_geo(gauge, 50000) 
mapview(gauge_buffer, col.regions = 'red') +
  mapview(gauge)

## ---- echo=FALSE, out.width='50%'---------------------------------------------
knitr::include_graphics('img/reanalysis.png')

## ---- echo=FALSE--------------------------------------------------------------
knitr::include_graphics('img/rean_files.png')

