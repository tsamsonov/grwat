# Ridgeline hydrograph plot

A convenient wrapper around
[`ggridges::geom_ridgeline()`](https://wilkelab.org/ggridges/reference/geom_ridgeline.html)
to visualize multiple river hydrographs at once.

## Usage

``` r
gr_plot_ridge(
  df,
  years,
  pal = 4,
  rev = FALSE,
  scale = 0.01,
  alpha = 0.8,
  print = TRUE
)
```

## Arguments

- df:

  `data.frame` with date (1st) and runoff (2nd) columns.

- years:

  Integer vector of years to be plotted.

- pal:

  Numeric or character string. Color palette identifier passed to
  [`ggplot2::scale_fill_distiller()`](https://ggplot2.tidyverse.org/reference/scale_brewer.html).

- rev:

  Boolean. Reverse the palette? Defaults to `FALSE`.

- scale:

  Numeric scale factor passed to
  [`ggridges::geom_ridgeline()`](https://wilkelab.org/ggridges/reference/geom_ridgeline.html).
  Defaults to `0.01`.

- alpha:

  Numeric opacity value of the ridgeline plot. Defaults to `0.8`.

- print:

  Boolean. Print plot? Defaults to `TRUE`. Use `FALSE` if you want to
  tweak the plot aesthetics before plotting.

## Value

`ggplot2` object representing the multiple river hydrographs at once
using the ridgeline plot approach

## Examples

``` r
if (require("ggridges")) {
  
  library(grwat)
  
  data(spas) # example Spas-Zagorye data is included with grwat package
  
  # separate
  sep = gr_separate(spas, params = gr_get_params(reg = 'center'))
  
  # ridgline plot for selected years
  gr_plot_ridge(sep, years = c(1960, 1965, 1989, 2001, 2012)) 

}
#> Loading required package: ggridges
#> grwat: data frame is correct
#> grwat: parameters list and types are OK
```
