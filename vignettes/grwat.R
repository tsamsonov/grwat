## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(sf)
library(tidyverse)
library(grwat)
library(mapview)

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

## -----------------------------------------------------------------------------

# this is path to sample basin geopackage installed with grwat
path = system.file("extdata", "spas-zagorye.gpkg", package = "grwat")

# for your own data just provide the full path:
# path = /this/is/the/path/to/discharge/basin.shp

basin = st_read(path) # read basin region
mapview(basin)

## -----------------------------------------------------------------------------
basin_pr = gr_buffer_geo(basin, 50000) 

