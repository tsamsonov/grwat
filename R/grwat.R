#' @useDynLib grwat
#' @importFrom Rcpp sourceCpp
NULL

devtools::use_package('Rcpp')
devtools::use_package('sf')
devtools::use_package('grid')
devtools::use_package('ncdf4')
devtools::use_package('lubridate')
devtools::use_package('dplyr')
devtools::use_package('rmarkdown')
devtools::use_package('Cairo')
devtools::use_package('R.utils')
devtools::use_package('writexl')
devtools::use_package('trend')
devtools::use_package('mblm')
devtools::use_package('GGally')
devtools::use_package('kableExtra')
devtools::use_package('scales')
devtools::use_package('readxl')
devtools::use_package('trend')
devtools::use_package('tidyr')
devtools::use_package('progress')
devtools::use_package('grid')
devtools::use_package('ggplot2', 'Depends')
devtools::use_package('magrittr', 'Depends')

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to grwat package for hydrograph separation and analysis")
}