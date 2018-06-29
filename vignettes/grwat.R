## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE, warning=FALSE---------------------------------------
library(sf)
library(grwat)

wd = "/Volumes/Data/Work/_grwat/Mezen_Malonisog/"

setwd(wd)

hdata = read.csv('in_Mezen_Malonisog.txt', sep = ' ', header = FALSE) # read gauge data
head(hdata)

basin = st_read('Mezen_Malonisog.gpkg', quiet = TRUE) # read basin region
basin_pr = grwat::st_buffer_geo(basin, 50000)  # buffer region by 50 km

rean = grwat::read_interim('prec.nc', 'temp.nc') # read reanalysis data
hdata_rean = grwat::join_interim(hdata, rean, basin_pr) # join reanalysis data to hydrological series
head(hdata_rean$df)

## ---- message=FALSE, warning=FALSE---------------------------------------
grwat::map(rean$pts, hdata_rean$pts, basin, basin_pr) # plot spatial configuration

## ---- message=FALSE, warning=FALSE---------------------------------------
grwat::process_gauge(wd, rean, bufsize = 50000) # process single folder

