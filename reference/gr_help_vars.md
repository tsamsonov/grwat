# Hydrograph separation variables

Use this function to learn the meaning of the variables that are
calculated by [`gr_summarize()`](gr_summarize.md).

## Usage

``` r
gr_help_vars()
```

## Value

`data.frame` of hydrograph separation variables

## Examples

``` r
library(grwat)

gr_help_vars()
#> # A tibble: 57 × 21
#>       ID Position Width Source Name_old    Name   Units Unitsua Unitsen Readtype
#>    <dbl>    <dbl> <dbl>  <dbl> <chr>       <chr>  <chr> <chr>   <chr>   <chr>   
#>  1     1        1     7      1 year_number Number NA    NA      NA      integer 
#>  2     2        8    10      1 Year1       Year1  Год   Рок     Year    integer 
#>  3     3       18    10      1 Year2       Year2  Год   Рок     Year    integer 
#>  4     4       28    15      1 datestart   Dspst… Дата  Дата    Date    Date    
#>  5     5       43    15      1 datepolend  Dspend Дата  Дата    Date    Date    
#>  6    57        0     0      0 PolProd     Tsp    Дней  Дней    Days    integer 
#>  7     6       58    10      1 Qy          Qy     м^3/с м^3/с   m^3/s   double  
#>  8     7       68    10      1 Qmax        Qspmax м^3/с м^3/с   m^3/s   double  
#>  9     8       78    15      1 datemax     Dspmax Дата  Дата    Date    Date    
#> 10     9       93    10      1 Qygr        Qygr   м^3/с м^3/с   m^3/s   double  
#> # ℹ 47 more rows
#> # ℹ 11 more variables: Type <chr>, Test <dbl>, Desc <chr>, Descua <chr>,
#> #   Descen <chr>, Group <chr>, Winter <dbl>, Chart <chr>, Color <chr>,
#> #   Order <dbl>, Range <chr>
```
