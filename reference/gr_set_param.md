# Set the value of hydrograph separation parameter

The value is set for selected years in parameter list. Such list is
returned by [`gr_separate()`](gr_separate.md) with `debug = TRUE` set.

## Usage

``` r
gr_set_param(params, p, value, years = NULL)
```

## Arguments

- params:

  `list` of `list`s of hydrograph separation parameters as returned in
  `params` attribute by [`gr_separate()`](gr_separate.md) with
  `debug = TRUE`.

- p:

  Name of the parameter.

- value:

  Numeric value to set.

- years:

  Integer vector of years to modify. Defaults to `NULL`, which means
  that all years will be modified.

## Value

`list` of `list`s — a modified version of `params`

## Examples

``` r
library(grwat)

data(spas) # example Spas-Zagorye data is included with grwat package

# Debug mode gives access to additional information
sep = gr_separate(spas, 
                  params = gr_get_params(reg = 'center'), 
                  debug = TRUE)
#> grwat: data frame is correct
#> grwat: parameters list and types are OK
#> Warning: grwat: 1974 years were not separated. Check the input data for possible errors. Use gr_get_gaps() and gr_fill_gaps() functions to detect and fill missing data.
#> Warning: grwat: 2002, 2014, 2019 years were processed with jittered parameters

# Visualize
gr_plot_sep(sep, c(1978, 1989)) 



# actual params used for each year
parlist = attributes(sep)$params

# set the sprecdays parameter for multiple years
parlist = gr_set_param(parlist, sprecdays, 
                       years = c(1978, 1989:1995), 
                       value = 15)

# use the list of parameters for separation
sep_new = gr_separate(spas, params = parlist, debug = TRUE)
#> grwat: data frame is correct
#> grwat: parameters list and types are OK
#> Warning: grwat: 1974 years were not separated. Check the input data for possible errors. Use gr_get_gaps() and gr_fill_gaps() functions to detect and fill missing data.

# Visualize
gr_plot_sep(sep_new, c(1978, 1989))

```
