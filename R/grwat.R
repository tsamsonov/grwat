#' @useDynLib grwat
#' @importFrom Rcpp sourceCpp
NULL

# General
usethis::use_package('R.utils')
usethis::use_package('Rcpp')
usethis::use_package('progress')
usethis::use_package('crayon')
usethis::use_package('beepr')

# Tidyverse and related
usethis::use_package('magrittr')
usethis::use_package('dplyr')
usethis::use_package('tidyr')
usethis::use_package('lubridate')
usethis::use_package('rlang')
usethis::use_package('ggplot2')
usethis::use_package('ggthemes')
usethis::use_package('GGally')
usethis::use_package('transformr')
usethis::use_package('gganimate')
usethis::use_package('ggridges')
usethis::use_package('ggHoriPlot')
usethis::use_package('knitr')
usethis::use_package('rmarkdown')
usethis::use_package('kableExtra')
usethis::use_package('shiny')
usethis::use_package('grid')

# Statistics and spatial
usethis::use_package('zoo')
usethis::use_package('ncdf4')
usethis::use_package('trend')
usethis::use_package('mblm')
usethis::use_package('scales')
usethis::use_package('sf')

# Specific for pipe
usethis::use_pipe(export = TRUE)

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to grwat package for the automatic hydrograph separation and hydrological time series analysis")
}