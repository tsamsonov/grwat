#' @useDynLib grwat
#' @importFrom Rcpp sourceCpp
#' @importFrom magrittr "%>%"
NULL

usethis::use_package('Rcpp')
usethis::use_package('sf')
usethis::use_package('grid')
usethis::use_package('ncdf4')
usethis::use_package('lubridate')
usethis::use_package('dplyr')
usethis::use_package('rmarkdown')
usethis::use_package('Cairo')
usethis::use_package('R.utils')
usethis::use_package('writexl')
usethis::use_package('trend')
usethis::use_package('mblm')
usethis::use_package('GGally')
usethis::use_package('kableExtra')
usethis::use_package('scales')
usethis::use_package('readxl')
usethis::use_package('trend')
usethis::use_package('tidyr')
usethis::use_package('progress')
usethis::use_package('grid')
usethis::use_package('rlang')
usethis::use_package('magrittr')
usethis::use_package('ggplot2', 'Depends')

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to grwat package for hydrograph separation and analysis")
}