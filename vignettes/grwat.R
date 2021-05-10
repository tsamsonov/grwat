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

## -----------------------------------------------------------------------------
# this is path to sample basin geopackage installed with grwat
path = system.file("extdata", "spas-zagorye.gpkg", package = "grwat")

# for your own data just provide the full path:
# path = /this/is/the/path/to/discharge/basin.shp

basin = st_read(path, layer = 'basin') # read basin region
mapview(basin)

## -----------------------------------------------------------------------------
basin_buffer = gr_buffer_geo(basin, 25000) 
mapview(basin_buffer, col.regions = 'red') +
  mapview(basin)

## -----------------------------------------------------------------------------
gauge = st_read(path, layer = 'gauge') # read gauge point
gauge_buffer = gr_buffer_geo(gauge, 50000) 
mapview(gauge_buffer, col.regions = 'red') +
  mapview(gauge)

## -----------------------------------------------------------------------------
box = st_bbox(basin_buffer)
box

## -----------------------------------------------------------------------------
knitr::include_graphics('img/reanalysis.png')

## -----------------------------------------------------------------------------
knitr::include_graphics('img/rean_files.png')

## -----------------------------------------------------------------------------
rean = gr_read_rean('/Volumes/Data/Spatial/Reanalysis/grwat/pre_1880-2021.nc',
                    '/Volumes/Data/Spatial/Reanalysis/grwat/temp_1880-2021.nc') # read reanalysis data
hdata_rean = gr_join_rean(hdata, rean, basin_buffer) # join reanalysis data to hydrological series

head(hdata_rean$df)

## ---- message=FALSE, warning=FALSE--------------------------------------------
gr_map(rean$pts, hdata_rean$pts, basin, basin_buffer) # plot spatial configuration

## -----------------------------------------------------------------------------
tab = gr_fill_gaps(hdata_rean$df,
                   autocorr = 0.7)

## -----------------------------------------------------------------------------
# Расчленение
p = gr_get_params()
p$nPav = 5
p$prodspada = 85

## -----------------------------------------------------------------------------
sep = gr_separate(tab, p)
head(sep)

## ---- message=FALSE, echo=FALSE-----------------------------------------------
vars = gr_summarize(sep)
head(vars)

## -----------------------------------------------------------------------------
gr_plot_sep(sep, 1979) # plot single year
gr_plot_sep(sep, c(1994, 2001)) # plot two years sequentially
gr_plot_sep(sep, 1994:1997, # plot four years on the same page
            layout = matrix(c(1,2,3,4), nrow=2, byrow=TRUE))

## -----------------------------------------------------------------------------
gr_help_vars()

## -----------------------------------------------------------------------------
gr_test_vars(vars, Qmax)

## -----------------------------------------------------------------------------
tests = gr_test_vars(vars, Qygr, date10w1, Wpol3)
tests$pvalues

## -----------------------------------------------------------------------------
tests = gr_test_vars(vars)
tests$year # this is a change year detected for each variable

## -----------------------------------------------------------------------------
tests = gr_test_vars(vars, Qmax, Qygr, change_year = 1987)
tests$ft # Fisher F tests to compare two variances

## ---- collapse=TRUE, message=FALSE--------------------------------------------
gr_plot_vars(vars, Qmax) # plot one selected variable
gr_plot_vars(vars, date10w1, Wpol3) # plot two variables sequentially
gr_plot_vars(vars, Qmax, Qygr, date10w1, Wpol3, # plot four variables in matrix layout
                      layout = matrix(c(1,2,3,4), nrow=2, byrow=TRUE)) 

## ---- collapse=TRUE, message=FALSE--------------------------------------------
gr_plot_vars(vars, date10w1, Wpol3, DaysThawWin, Qmaxpavs,
             tests = gr_test_vars(vars, date10w1, Wpol3, DaysThawWin, Qmaxpavs)) # add test information

## ---- eval=FALSE--------------------------------------------------------------
#  gr_plot_vars(vars, tests = gr_test_vars(vars))

## -----------------------------------------------------------------------------
gr_plot_periods(vars, Qy, year = 1978)
gr_plot_periods(vars, Qy, tests = gr_test_vars(vars, Qy))

## -----------------------------------------------------------------------------
gr_plot_periods(vars, Qy, Qmax, 
                tests = gr_test_vars(vars, Qy, Qmax),
                layout = matrix(c(1,2)))

## ---- eval = FALSE------------------------------------------------------------
#  gr_plot_periods(df, tests = gr_test_vars(df))

## ---- message=FALSE-----------------------------------------------------------
gr_plot_minmonth(vars, year = 1985)

