% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{gr_plot_minmonth}
\alias{gr_plot_minmonth}
\title{Plot histogram of minimum discharge month for two periods}
\usage{
gr_plot_minmonth(
  df,
  year = NULL,
  exclude = NULL,
  tests = NULL,
  pagebreak = FALSE,
  locale = "EN"
)
}
\arguments{
\item{df}{data.frame produced by description function (read by `read_variables()`)}

\item{year}{change year value to separate two periods}

\item{exclude}{integer vector. A vector of years to be excluded from plotting}

\item{locale}{string locale. Currently only English locale is supported. Defaults to 'EN'.}
}
\value{
ggplot2 objects
}
\description{
Plot histogram of minimum discharge month for two periods
}