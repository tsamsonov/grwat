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
library(grwat)

mapviewOptions(fgb = FALSE)

# this is path to sample data installed with grwat
path = system.file("extdata", "spas-zagorye.txt", 
                   package = "grwat")

# for your own data just provide the full path:
# path = /this/is/the/path/to/discharge/discharge.csv

hdata_raw = read_delim(path, 
                   col_names = c('d', 'm', 'y', 'q'), 
                   col_types = 'iiid', delim = ' ') # read gauge data
head(hdata_raw) # see the data

## -----------------------------------------------------------------------------
hdata = hdata_raw %>% 
  transmute(Date = lubridate::make_date(y, m, d), 
            Q = q)
head(hdata)

## -----------------------------------------------------------------------------
gaps = gr_get_gaps(hdata)
gaps

## -----------------------------------------------------------------------------
fhdata = gr_fill_gaps(hdata, autocorr = 0.7)
fhdata = gr_fill_gaps(hdata, nobserv = 10)

## -----------------------------------------------------------------------------
gr_plot_acf(hdata, 0.5)

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

g = basin

box = sf::st_bbox(g)
  lon0 = 0.5 * (box[1] + box[3]) # longitude
  lat0 = 0.5 * (box[2] + box[4]) # latitude
  prj = stringr::str_interp('+proj=aeqd +lat_0=${lat0} +lon_0=${lon0} +x_0=0 +y_0=0 +datum=WGS84')
  
  g %>% 
    sf::st_transform(crs = prj) %>% 
    sf::st_buffer(25000) %>% 
    sf::st_transform(4326)

## ---- dpi = 72----------------------------------------------------------------
gauge_buffer = gr_buffer_geo(gauge, 50000) 
mapview(gauge_buffer, col.regions = 'red') +
  mapview(gauge)

## ---- echo=FALSE, out.width='50%'---------------------------------------------
knitr::include_graphics('img/reanalysis.png')

## ---- echo=FALSE--------------------------------------------------------------
knitr::include_graphics('img/rean_files.png')

## -----------------------------------------------------------------------------
rean = gr_read_rean(
  '/Volumes/Data/Spatial/Reanalysis/grwat/pre_1880-2021.nc',
  '/Volumes/Data/Spatial/Reanalysis/grwat/temp_1880-2021.nc'
) # read reanalysis data

fhdata_rean = gr_join_rean(fhdata, rean, basin_buffer) # join reanalysis data to hydrological series

head(fhdata_rean)

## ---- dpi = 72----------------------------------------------------------------
 # plot spatial configuration
m = mapview(basin_buffer, col.regions = 'red') +
  mapview(basin) +
  mapview(rean$pts[basin_buffer, ], col.regions = 'black') +
  mapview(rean$pts, cex = 1)

box = st_bbox(basin_buffer)
center = st_coordinates(st_centroid(basin_buffer))

m@map %>% leaflet::setView(center[1], center[2], zoom = 7)

## -----------------------------------------------------------------------------
resbase = fhdata %>% 
  mutate(Qbase = gr_baseflow(Q, method = 'lynehollick'))

# quick look at the table
head(resbase, 10)
  
resbase %>% 
  filter(lubridate::year(Date) == 2020) %>% 
  ggplot() +
    geom_area(aes(Date, Q), fill = 'steelblue', 
              color = 'black') +
    geom_area(aes(Date, Qbase), fill = 'orangered', 
              color = 'black')

## -----------------------------------------------------------------------------
resbase = fhdata %>% 
  mutate(Qbase = gr_baseflow(Q, method = 'lynehollick', 
                             a = 0.8, passes = 5))
  
resbase %>% 
  filter(lubridate::year(Date) == 2020) %>% 
  ggplot() +
    geom_area(aes(Date, Q), fill = 'steelblue', 
              color = 'black') +
    geom_area(aes(Date, Qbase), fill = 'orangered', 
              color = 'black')

## ---- fig.height = 12---------------------------------------------------------
methods = c("maxwell",
            "boughton",
            "jakeman",
            "lynehollick",
            "chapman")

plots = lapply(methods, function(m) {
  resbase = fhdata %>% 
    mutate(Qbase = gr_baseflow(Q, method = m))

  resbase %>%
    filter(lubridate::year(Date) == 2020) %>% 
    ggplot() +
      geom_area(aes(Date, Q), fill = 'steelblue', color = 'black') +
      geom_area(aes(Date, Qbase), fill = 'orangered', color = 'black') +
      labs(title = m)
})

patchwork::wrap_plots(plots, ncol = 2)

## -----------------------------------------------------------------------------
View(gr_help_params())

## -----------------------------------------------------------------------------
# Расчленение
p = gr_get_params(reg = 'Midplain')
p$precdays = 5
p$ftrecdays = 85

## -----------------------------------------------------------------------------
sep = gr_separate(fhdata_rean, p)
head(sep)

## ---- message=FALSE, warning=FALSE--------------------------------------------
vars = gr_summarize(sep)
head(vars)

## ---- fig.width=12, fig.height=6----------------------------------------------
gr_plot_sep(sep, 1976) # plot single year
gr_plot_sep(sep, c(2016, 2017)) # plot two years sequentially

## ----fig.width=12, fig.height=12----------------------------------------------
gr_plot_sep(sep, 1976:1979, # plot four years on the same page
            layout = matrix(c(1,2,3,4), ncol=2, byrow = T))

## -----------------------------------------------------------------------------
View(gr_help_vars())

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

## ---- fig.width=12, fig.height=6----------------------------------------------
gr_plot_vars(vars, Qmax) # plot one selected variable
gr_plot_vars(vars, datestart) # plot one selected variable
gr_plot_vars(vars, date10w1, Wpol3) # plot two variables sequentially

## ---- fig.width=12, fig.height=12---------------------------------------------

gr_plot_vars(vars, Qmax, Qygr, date10w1, Wpol3, # plot four variables in matrix layout
             layout = matrix(c(1,2,3,4), nrow=2, byrow=TRUE)) 

## ---- fig.width=12, fig.height=6----------------------------------------------
gr_plot_vars(vars, date10w1, Wpol3, DaysThawWin, Qmaxpavs,
             tests = TRUE) # add test information

## ---- eval = FALSE------------------------------------------------------------
#  gr_plot_vars(vars, date10w1, Wpol3, DaysThawWin, Qmaxpavs,
#               tests = gr_test_vars(vars, date10w1, Wpol3, DaysThawWin, Qmaxpavs, exclude = 1990)) # add test information

## ---- eval=FALSE--------------------------------------------------------------
#  gr_plot_vars(vars, tests = TRUE)

## ---- fig.height = 2.5--------------------------------------------------------
gr_plot_periods(vars, Qy, year = 1978)
gr_plot_periods(vars, Qy, tests = TRUE)
gr_plot_periods(vars, Qy, tests = gr_test_vars(vars, Qy, year = 1985))

## ---- fig.width=12, fig.height=8----------------------------------------------
gr_plot_periods(vars, Qy, Qmax, date10w1, Wpol3,
                tests = TRUE,
                layout = matrix(1:4, nrow = 2))

## ---- eval = FALSE------------------------------------------------------------
#  gr_plot_periods(vars, tests = TRUE)

## ---- fig.height= 12, message=FALSE-------------------------------------------
gr_plot_minmonth(vars, year = 1985)

## ---- fig.width=10, fig.height=5----------------------------------------------
gr_plot_matrix(sep, type = 'runoff')
gr_plot_matrix(sep, type = 'season')
gr_plot_matrix(sep, type = 'component')

## ---- fig.width=10, fig.height=2.5--------------------------------------------
gr_plot_matrix(sep, years = 1980:1995, type = 'component')

## ---- fig.width=8, fig.height=4-----------------------------------------------
library(ggplot2)
library(lubridate)

sep_sel = sep |> 
  filter(Year %in% c(1989, 2012))

ggplot(sep_sel, aes(ymd(20000101) + yday(Date), Q, 
                    fill = factor(Year), group = factor(Year))) + 
  geom_area(alpha = 0.8, position = "identity") +
  geom_line() +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_minimal() +
  labs(x = 'Date', y = 'Discharge', fill = 'Year')

## ---- fig.width=8, fig.height=4-----------------------------------------------
sep_sel = sep |> 
  filter(Year %in% c(1960, 1965, 1989, 2001, 2012))

ggplot(sep_sel, aes(ymd(20000101) + yday(Date), Q, 
                    fill = factor(Year), group = factor(Year))) + 
  geom_area(alpha = 0.8, position = "identity") +
  geom_line() +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_minimal() +
  labs(x = 'Date', y = 'Year')

## ---- fig.width=10, fig.height=5----------------------------------------------
gr_plot_ridge(sep, years = c(1960, 1965, 1989, 2001, 2012))

## ---- fig.width=8, fig.height=4-----------------------------------------------
gr_plot_hori(sep, years = 1960:1980)

## ---- eval = FALSE------------------------------------------------------------
#  report = '~/Spas-Zagorye.html'
#  gr_report(sep, vars, output = report)
#  browseURL(report)

