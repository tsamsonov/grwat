#' @useDynLib grwat
#' @importFrom Rcpp sourceCpp
#' @importFrom rlang .data
#' @importFrom graphics text
#' @importFrom stats acf coef complete.cases density sd setNames t.test var.test
#' 
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to grwat package for the automatic hydrograph separation and hydrological time series analysis")
}