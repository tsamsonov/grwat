#' @useDynLib grwat
#' @importFrom Rcpp sourceCpp
#' @importFrom rlang .data
#' @importFrom graphics text
#' @importFrom stats acf coef complete.cases density sd setNames t.test var.test
#' 
NULL


# General
usethis::use_package('Rcpp')
usethis::use_package('cli')

# Tidyverse and related
usethis::use_package('magrittr')
usethis::use_package('dplyr')
usethis::use_package('tidyr')
usethis::use_package('lubridate')
usethis::use_package('stringr')
usethis::use_package('rlang')
usethis::use_package('grid')
usethis::use_package('ggplot2')
usethis::use_package('R.utils')

# Statistics
usethis::use_package('zoo')
usethis::use_package('trend')
usethis::use_package('mblm')

# Suggested
usethis::use_package('ggridges', type = "suggests")
usethis::use_package('ggHoriPlot', type = "suggests")
usethis::use_package('ggthemes', type = "suggests")
usethis::use_package('ncdf4', type = "suggests")
usethis::use_package('sf', type = "suggests")
usethis::use_package('knitr', type = "suggests")
usethis::use_package('rmarkdown', type = "suggests")
usethis::use_package('kableExtra', type = "suggests")
usethis::use_package('testthat', type = "suggests")
usethis::use_package('stringi', type = "suggests")


# Specific for pipe
usethis::use_pipe(export = TRUE)

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to grwat package for the automatic hydrograph separation and hydrological time series analysis")
}