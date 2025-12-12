# Read ERA5 daily reanalysis data

The function reads meteorological variables (temperature and
precipitation) from [ERA 5 post-processed daily statistics on single
levels](https://cds.climate.copernicus.eu/datasets/derived-era5-single-levels-daily-statistics?tab=download)
for using with [`gr_join_rean()`](gr_join_rean.md).

## Usage

``` r
gr_read_era5_daily(file_prec, file_temp)
```

## Arguments

- file_prec:

  Character string path to daily sum precipitation NetCDF file.

- file_temp:

  Character string path to daily mean temperature NetCDF file.

## Value

`list` containing time series, precipitation series, temperature series
and spatial points (sf)

## Examples

``` r
if (require("sf") && require("ncdf4")) {
  
  library(grwat)
  
  prec_path = system.file("extdata", "era5_daily_prec.nc",
                          package = "grwat")
  
  temp_path = system.file("extdata", "era5_daily_temp.nc",
                          package = "grwat")
  
  # read reanalysis data
  rean = gr_read_era5_daily(prec_path, temp_path) 
  
  str(rean)
}
#> List of 4
#>  $ vals.full: num [1:365(1d)] 0 1 2 3 4 5 6 7 8 9 ...
#>  $ prate    : num [1:17, 1:9, 1:365] 1.68 1.62 1.5 1.32 1.11 ...
#>  $ temp     : num [1:17, 1:9, 1:365] -18 -18.1 -18.1 -18.4 -18.6 ...
#>  $ pts      :Classes ‘sf’ and 'data.frame':  153 obs. of  3 variables:
#>   ..$ nlon    : int [1:153] 1 2 3 4 5 6 7 8 9 10 ...
#>   ..$ nlat    : int [1:153] 1 1 1 1 1 1 1 1 1 1 ...
#>   ..$ geometry:sfc_POINT of length 153; first list element:  'XY' num [1:2] 34 56
#>   ..- attr(*, "sf_column")= chr "geometry"
#>   ..- attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: NA NA
#>   .. ..- attr(*, "names")= chr [1:2] "nlon" "nlat"
```
