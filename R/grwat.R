#' @useDynLib grwat
#' @importFrom Rcpp sourceCpp
#' @importFrom rlang .data
#' @importFrom graphics text
#' @importFrom stats acf coef complete.cases density sd setNames t.test var.test
#' 
NULL


# General
usethis::use_package('Rcpp')
usethis::use_package('progress')
usethis::use_package('crayon')
usethis::use_package('beepr', type = "Suggests")

# Tidyverse and related
usethis::use_package('magrittr')
usethis::use_package('dplyr')
usethis::use_package('tidyr')
usethis::use_package('lubridate')
usethis::use_package('stringr')
usethis::use_package('rlang')
usethis::use_package('grid')
usethis::use_package('ggplot2')
usethis::use_package('knitr')
usethis::use_package('rmarkdown')
usethis::use_package('kableExtra')
usethis::use_package('testthat')
usethis::use_package('readr', type = "Suggests")
# usethis::use_package('ggthemes', type = "Suggests")
usethis::use_package('gganimate', type = "Suggests")
# usethis::use_package('ggridges', type = "Suggests")
# usethis::use_package('ggHoriPlot', type = "Suggests")

# Statistics and spatial
usethis::use_package('zoo')
usethis::use_package('trend')
usethis::use_package('mblm')
usethis::use_package('scales')
# usethis::use_package('sf', type = "Suggests")
usethis::use_package('ncdf4', type = "Suggests")

# Specific for pipe
usethis::use_pipe(export = TRUE)

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to grwat package for the automatic hydrograph separation and hydrological time series analysis")
}