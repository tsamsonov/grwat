# Get hydrograph separation parameters

The function returns the list of parameters that can be used by
[`gr_separate()`](gr_separate.md). Since the parameters are
region-specific, the location must be selected. It can be identified by
region name or geographic coordinates. If both are specified, then
region have a higher priority

## Usage

``` r
gr_get_params(reg = "center", lon = NULL, lat = NULL)
```

## Arguments

- reg:

  Character string — the name of the region. Defaults to `'center'`.

- lon:

  Numeric value of the longitude. Ignored if `reg` is specified.

- lat:

  Numeric value of the latitude. Ignored if `reg` is specified.

## Value

List of separation parameters that can be used in
[`gr_separate()`](gr_separate.md) function.

## Examples

``` r
library(grwat)

params = gr_get_params(reg = 'center')

print(params)
#> $winmon
#> [1] 11
#> 
#> $grad1
#> [1] 1.7
#> 
#> $grad2
#> [1] 5
#> 
#> $gratio
#> [1] 400
#> 
#> $spmon1
#> [1] 2
#> 
#> $spmon2
#> [1] 5
#> 
#> $sprisedays1
#> [1] 8
#> 
#> $sprisedays2
#> [1] 10
#> 
#> $spdays
#> [1] 30
#> 
#> $sprise
#> [1] 10
#> 
#> $spratio
#> [1] 2.5
#> 
#> $sprecdays
#> [1] 35
#> 
#> $spcomp
#> [1] 2
#> 
#> $precdays
#> [1] 5
#> 
#> $frostdays
#> [1] 5
#> 
#> $windays
#> [1] 5
#> 
#> $floodprec
#> [1] 5
#> 
#> $floodtemp
#> [1] 0
#> 
#> $frosttemp
#> [1] -8
#> 
#> $wintemp
#> [1] -1
#> 
#> $signratio1
#> [1] 0.01
#> 
#> $signratio2
#> [1] 0.0015
#> 
#> $floodratio
#> [1] 0.001
#> 
#> $gaplen
#> [1] 15
#> 
#> $snowtemp
#> [1] 2
#> 
#> $gradabs
#> [1] 1000
#> 
#> $mntmode
#> [1] 0
#> 
#> $mntgrad
#> [1] 2
#> 
#> $mntavgdays
#> [1] 30
#> 
#> $mntratiodays
#> [1] 5
#> 
#> $mntratio
#> [1] 1.5
#> 
#> $niter
#> [1] 100
#> 
#> $filter
#> [1] "lynehollick"
#> 
#> $a
#> [1] 0.925
#> 
#> $k
#> [1] 0.93
#> 
#> $C
#> [1] 0.05
#> 
#> $aq
#> [1] -0.5
#> 
#> $padding
#> [1] 0
#> 
#> $passes
#> [1] 3
#> 
```
