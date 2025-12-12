# Convert list of parameters to data frame

Convert list of parameters to data frame

## Usage

``` r
gr_to_pardf(params)
```

## Arguments

- params:

  `list` of `list`s of hydrograph separation parameters as returned in
  `params` attribute by [`gr_separate()`](gr_separate.md) with
  `debug = TRUE`.

## Value

`tibble` data frame with tabular representation of hydrograph separation
parameters

## Examples

``` r
library(grwat)

data(spas) # example Spas-Zagorye data is included with grwat package
head(spas)
#> # A tibble: 6 × 4
#>   Date           Q   Temp  Prec
#>   <date>     <dbl>  <dbl> <dbl>
#> 1 1956-01-01  5.18  -6.46 0.453
#> 2 1956-01-02  5.18 -11.4  0.825
#> 3 1956-01-03  5.44 -10.7  0.26 
#> 4 1956-01-04  5.44  -8.05 0.397
#> 5 1956-01-05  5.44 -11.7  0.102
#> 6 1956-01-06  5.58 -20.1  0.032

# separate
sep = gr_separate(spas, params = gr_get_params(reg = 'center'))
#> grwat: data frame is correct
#> grwat: parameters list and types are OK

# Visualize
gr_plot_sep(sep, c(1978, 1989)) 



# Debug mode gives access to additional information
sep_debug = gr_separate(spas, 
                        params = gr_get_params(reg = 'center'), 
                        debug = TRUE)
#> grwat: data frame is correct
#> grwat: parameters list and types are OK
#> Warning: grwat: 1974 years were not separated. Check the input data for possible errors. Use gr_get_gaps() and gr_fill_gaps() functions to detect and fill missing data.
#> Warning: grwat: 2002, 2014, 2019 years were processed with jittered parameters

# a vector of years with jittered params
jit = attributes(sep_debug)$jittered
print(jit)
#> [1] 2002 2014 2019

# actual params used for each year
parlist = attributes(sep_debug)$params

# tweak parameters for selected year
parlist[['1989']]$grad1 = 3
parlist[['1989']]$grad2 = 6

# tabular representation of parameters
partab = gr_to_pardf(parlist)

head(partab)
#> # A tibble: 6 × 40
#>    year winmon grad1 grad2 gratio spmon1 spmon2 sprisedays1 sprisedays2 spdays
#>   <int>  <dbl> <dbl> <dbl>  <dbl>  <dbl>  <dbl>       <dbl>       <dbl>  <dbl>
#> 1  1956     11   1.7     5    400      2      5           8          10     30
#> 2  1957     11   1.7     5    400      2      5           8          10     30
#> 3  1958     11   1.7     5    400      2      5           8          10     30
#> 4  1959     11   1.7     5    400      2      5           8          10     30
#> 5  1960     11   1.7     5    400      2      5           8          10     30
#> 6  1961     11   1.7     5    400      2      5           8          10     30
#> # ℹ 30 more variables: sprise <dbl>, spratio <dbl>, sprecdays <dbl>,
#> #   spcomp <dbl>, precdays <dbl>, frostdays <dbl>, windays <dbl>,
#> #   floodprec <dbl>, floodtemp <dbl>, frosttemp <dbl>, wintemp <dbl>,
#> #   signratio1 <dbl>, signratio2 <dbl>, floodratio <dbl>, gaplen <dbl>,
#> #   snowtemp <dbl>, gradabs <dbl>, mntmode <dbl>, mntgrad <dbl>,
#> #   mntavgdays <dbl>, mntratiodays <dbl>, mntratio <dbl>, niter <dbl>, a <dbl>,
#> #   k <dbl>, C <dbl>, aq <dbl>, padding <dbl>, passes <dbl>, filter <chr>

partab |> 
  dplyr::filter(year == 1989) |> 
  head()
#> # A tibble: 1 × 40
#>    year winmon grad1 grad2 gratio spmon1 spmon2 sprisedays1 sprisedays2 spdays
#>   <int>  <dbl> <dbl> <dbl>  <dbl>  <dbl>  <dbl>       <dbl>       <dbl>  <dbl>
#> 1  1989     11     3     6    400      2      5           8          10     30
#> # ℹ 30 more variables: sprise <dbl>, spratio <dbl>, sprecdays <dbl>,
#> #   spcomp <dbl>, precdays <dbl>, frostdays <dbl>, windays <dbl>,
#> #   floodprec <dbl>, floodtemp <dbl>, frosttemp <dbl>, wintemp <dbl>,
#> #   signratio1 <dbl>, signratio2 <dbl>, floodratio <dbl>, gaplen <dbl>,
#> #   snowtemp <dbl>, gradabs <dbl>, mntmode <dbl>, mntgrad <dbl>,
#> #   mntavgdays <dbl>, mntratiodays <dbl>, mntratio <dbl>, niter <dbl>, a <dbl>,
#> #   k <dbl>, C <dbl>, aq <dbl>, padding <dbl>, passes <dbl>, filter <chr>
```
