# Horizon hydrograph plot

A convenient wrapper around
[`ggHoriPlot::geom_horizon()`](https://rivasiker.github.io/ggHoriPlot/reference/geom_horizon.html)
to visualize multiple river hydrographs at once.

## Usage

``` r
gr_plot_hori(df, years, pal = "Blues", rev = TRUE, scale = 6, print = TRUE)
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
  [`ggHoriPlot::geom_horizon()`](https://rivasiker.github.io/ggHoriPlot/reference/geom_horizon.html).
  Defaults to `6`.

- print:

  Boolean. Print plot? Defaults to `TRUE`. Use `FALSE` if you want to
  tweak the plot aesthetics before plotting.

## Value

`ggplot2` object representing multiple river hydrographs at once using
the horizon plot approach

## Examples

``` r
if (require("ggHoriPlot") && require("ggthemes")) {
  
  library(grwat)
  
  data(spas) # example Spas-Zagorye data is included with grwat package
  
  # separate
  sep = gr_separate(spas, params = gr_get_params(reg = 'center'))
  
  # horizon plot for selected years
  gr_plot_hori(sep, years = 1960:1980)
  
}
#> Loading required package: ggHoriPlot
#> Loading required package: ggthemes
#> grwat: data frame is correct
#> grwat: parameters list and types are OK
#> Warning: Using the `size` aesthetic in this geom was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` in the `default_aes` field and elsewhere instead.
#> ℹ The deprecated feature was likely used in the grwat package.
#>   Please report the issue at <https://github.com/tsamsonov/grwat/issues>.
```
