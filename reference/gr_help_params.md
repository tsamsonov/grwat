# Get the information about parameters used to separate the hydrograph

Get the information about parameters used to separate the hydrograph

## Usage

``` r
gr_help_params()
```

## Value

`data.frame` with description of hydrograph separation parameters that
are used in [`gr_separate()`](gr_separate.md) .

## Examples

``` r
library(grwat)

gr_help_params()
#> # A tibble: 39 × 12
#>        N name_old name       example desc  units formula comments pics  desc_rus
#>    <dbl> <chr>    <chr>      <chr>   <chr> <chr> <chr>   <chr>    <lgl> <chr>   
#>  1     1 mome     winmon     11      the … month  NA     "not us… NA    месяц, …
#>  2     2 grad     grad1      1.5     rate… %      NA     "if cha… NA    интенси…
#>  3     3 grad1    grad2      2       rate… %     "\"\""  "\"\""   NA    интенси…
#>  4     4 kdQgr1   gratio     150     maxi… %      NA     "can be… NA    максима…
#>  5     5 polmon1  spmon1     2       the … month  NA     "can be… NA    cамый р…
#>  6     6 polmon2  spmon2     5       the … month  NA     "can be… NA    самый п…
#>  7     7 polkol1  sptriseda… 15      amou… days   NA      NA      NA    количес…
#>  8     8 polkol2  spriseday… 25      amou… days   NA      NA      NA    количес…
#>  9     9 polkol3  spdays     30      amou… days   NA     "can be… NA    количес…
#> 10    10 polgrad1 sprise     10      mean… %      NA      NA      NA    значени…
#> # ℹ 29 more rows
#> # ℹ 2 more variables: role_1 <chr>, role_2 <chr>
```
