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

hdata = read.csv('in_Mezen_Malonisog.txt',
                 header = FALSE, 
                 sep = ' ') # read gauge data
head(hdata)

basin = st_read('Mezen_Malonisog.gpkg', quiet = TRUE) # read basin region
basin_pr = grwat::st_buffer_geo(basin, 50000)  # buffer region by 50 km

rean = grwat::read_interim('prec.nc', 'temp.nc') # read reanalysis data
hdata_rean = grwat::join_interim(hdata, rean, basin_pr) # join reanalysis data to hydrological series
head(hdata_rean$df)

## ---- message=FALSE, warning=FALSE---------------------------------------
grwat::map(rean$pts, hdata_rean$pts, basin, basin_pr) # plot spatial configuration

## ---- eval=FALSE---------------------------------------------------------
#  grwat::process_gauge(wd, rean, bufsize = 50000) # process single folder

## ---- eval=FALSE---------------------------------------------------------
#  wd = "/Volumes/Data/Work/_grwat/2018/"
#  grwat::process_basins(wd, rean, bufsize = 50000) # process single folder

## ---- message=FALSE------------------------------------------------------
setwd("/Volumes/Data/Work/_grwat/Mezen_Malonisog/")

sep = grwat::read_separation('AllGrWat.txt')#
head(sep)

## ------------------------------------------------------------------------
grwat::plot_separation(sep, 1978) # plot single year
grwat::plot_separation(sep, c(1994, 2001)) # plot two years sequentially
grwat::plot_separation(sep, 1994:1997, # plot four years on the same page
                       layout = matrix(c(1,2,3,4), nrow=2, byrow=TRUE))

## ------------------------------------------------------------------------
setwd("/Volumes/Data/Work/_grwat/Mezen_Malonisog/")

df = grwat::read_parameters('Total.txt') # read parameters file
head(df)

## ------------------------------------------------------------------------
grwat::get_parameters()

## ------------------------------------------------------------------------


