# Read reanalysis data

The function reads meteorological variables (temperature and
precipitation) from [grwat
reanalysis](https://www.dropbox.com/sh/5xjnf620tlwfk7a/AABhTaPEDWLII8rV04dp0MWna?dl=0)
for using with [`gr_join_rean()`](gr_join_rean.md). Reanalysis covers
the East European Plain with 0.75 degrees spatial resolution and is
obtained based on CIRES-DOE (1880-1949) and ERA5 (1950-2021) data.

## Usage

``` r
gr_read_rean(file_prec, file_temp)
```

## Arguments

- file_prec:

  Character string path to precipitation NetCDF file.

- file_temp:

  Character string path to temperature NetCDF file.

## Value

`list` containing time series, precipitation series, temperature series
and spatial points (sf)

## Details

Download the reanalysis archive from
[here](https://www.dropbox.com/sh/5xjnf620tlwfk7a/AABhTaPEDWLII8rV04dp0MWna?dl=0).

## Examples

``` r
if (require("sf") && require("ncdf4")) {
  
  library(grwat)
  
  # read reanalysis data
  if (FALSE) { # \dontrun{
    rean = gr_read_rean(
      '/Volumes/Data/Spatial/Reanalysis/grwat/pre_1880-2021.nc',
      '/Volumes/Data/Spatial/Reanalysis/grwat/temp_1880-2021.nc'
    ) 
    
    str(rean)
  } # }
  
}
```
