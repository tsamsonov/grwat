# Get gaps in the daily data

Use the function to detect periods of missing data. The first column
must be of `Date` type. The data is considered to be a gap if any value
in the row is missing.

## Usage

``` r
gr_get_gaps(hdata)
```

## Arguments

- hdata:

  `data.frame` with at least two columns, where the first column is
  `Date`

## Value

`data.frame` with periods of data and periods of gaps, containing five
columns: number of the period (num), start of the period (start_date),
end of the period (end_date), duration of the period (duration) and type
of the period (type).

## Examples

``` r
library(grwat)
library(dplyr)

# example Spas-Zagorye data is included with grwat package
path = system.file("extdata", "spas-zagorye.txt", 
                   package = "grwat")

hdata_raw = read.delim(path, header = FALSE, 
                       sep = ' ', na.strings = c('-999', '-999.0', '-'),
                       col.names = c('d', 'm', 'y', 'q'))

hdata = hdata_raw |> 
  transmute(Date = lubridate::make_date(y, m, d), 
            Q = q)

head(hdata)
#>         Date    Q
#> 1 1956-01-01 5.18
#> 2 1956-01-02 5.18
#> 3 1956-01-03 5.44
#> 4 1956-01-04 5.44
#> 5 1956-01-05 5.44
#> 6 1956-01-06 5.58

# identify gaps
gr_get_gaps(hdata) 
#> # A tibble: 8 × 5
#>     num start_date end_date   duration   type 
#>   <int> <date>     <date>     <drtn>     <chr>
#> 1     1 1956-01-01 1970-04-09  5213 days data 
#> 2     2 1970-04-10 1970-04-12     3 days gap  
#> 3     3 1970-04-13 1979-09-09  3437 days data 
#> 4     4 1979-09-10 1979-09-11     2 days gap  
#> 5     5 1979-09-12 2011-05-09 11563 days data 
#> 6     6 2011-05-10 2011-05-14     5 days gap  
#> 7     7 2011-05-15 2020-12-26  3514 days data 
#> 8     8 2020-12-27 2020-12-31     5 days gap  

# fill gaps
fhdata = gr_fill_gaps(hdata, autocorr = 0.8)
#> grwat: filled 5 observations using 4 days window for each variable

# check the results
gr_get_gaps(fhdata)
#> # A tibble: 4 × 5
#>     num start_date end_date   duration   type 
#>   <int> <date>     <date>     <drtn>     <chr>
#> 1     1 1956-01-01 2011-05-09 20218 days data 
#> 2     2 2011-05-10 2011-05-14     5 days gap  
#> 3     3 2011-05-15 2020-12-26  3514 days data 
#> 4     4 2020-12-27 2020-12-31     5 days gap  

# fill gaps
fhdata = gr_fill_gaps(hdata, nobserv = 7)
#> grwat: filled 10 observations using 7 days window for each variable

# check the results
gr_get_gaps(fhdata)
#> # A tibble: 2 × 5
#>     num start_date end_date   duration   type 
#>   <int> <date>     <date>     <drtn>     <chr>
#> 1     1 1956-01-01 2020-12-26 23737 days data 
#> 2     2 2020-12-27 2020-12-31     5 days gap  
```
