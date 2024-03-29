% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{gr_plot_hori}
\alias{gr_plot_hori}
\title{Horizon hydrograph plot}
\usage{
gr_plot_hori(df, years, pal = "Blues", rev = TRUE, scale = 6, print = TRUE)
}
\arguments{
\item{df}{\code{data.frame} with date (1st) and runoff (2nd) columns.}

\item{years}{Integer vector of years to be plotted.}

\item{pal}{Numeric or character string. Color palette identifier passed to \code{\link[ggplot2:scale_brewer]{ggplot2::scale_fill_distiller()}}.}

\item{rev}{Boolean. Reverse the palette? Defaults to \code{FALSE}.}

\item{scale}{Numeric scale factor passed to \code{\link[ggHoriPlot:geom_horizon]{ggHoriPlot::geom_horizon()}}. Defaults to \code{6}.}

\item{print}{Boolean. Print plot? Defaults to \code{TRUE}. Use \code{FALSE} if you want to tweak the plot aesthetics before plotting.}
}
\value{
\code{ggplot2} object representing multiple river hydrographs at once using the horizon plot approach
}
\description{
A convenient wrapper around \code{\link[ggHoriPlot:geom_horizon]{ggHoriPlot::geom_horizon()}} to visualize multiple river hydrographs at once.
}
\examples{
if (require("ggHoriPlot") && require("ggthemes")) {
  
  library(grwat)
  
  data(spas) # example Spas-Zagorye data is included with grwat package
  
  # separate
  sep = gr_separate(spas, params = gr_get_params(reg = 'center'))
  
  # horizon plot for selected years
  gr_plot_hori(sep, years = 1960:1980)
  
}
}
