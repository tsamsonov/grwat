[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)

# grwat

Welcome to grwat, an R package for hydrograph separation and analysis based on water level, temperature and percipitation data. It makes use of geographic data processing to spatially select temperature and precipitation data within the basin of each gauge, average these data and join them to each element in water level series. High-preformance Fortran/C++ computation is used for hydrograph processing that separates water level series into ground, seasonal, thaw, and flood discharge. Interannual and long-term characteristics of each discharge type are derived. Results are visualized in a form of high-quality reports making use of ggplot2 graphics and knitr report generation

## Installing

Currently only development version is available. It can be installed from github with
```r
library(devtools)
install_github("tsamsonov/grwat")
```