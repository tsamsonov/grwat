# Join reanalysis data

The function performs spatial join of meteorological variables
(temperature and precipitation) from [grwat
reanalysis](https://www.dropbox.com/sh/5xjnf620tlwfk7a/AABhTaPEDWLII8rV04dp0MWna?dl=0)
to the daily runoff time series. Reanalysis covers the East European
Plain with 0.75 degrees spatial resolution and is obtained based on
CIRES-DOE (1880-1949) and ERA5 (1950-2021) data. This function is useful
when the data from meteorological stations are missing inside the basin.

## Usage

``` r
gr_join_rean(hdata, rean, buffer)
```

## Arguments

- hdata:

  `data.frame` containing 2 columns: `Date` and runoff

- rean:

  `list` as returned by [`gr_read_rean()`](gr_read_rean.md) or
  [`gr_read_era5_daily()`](gr_read_era5_daily.md)

- buffer:

  `sf` object containing the region to select reanalysis data. Usually a
  river basin is used to select the meteorological data. Use
  [`gr_buffer_geo()`](gr_buffer_geo.md) to buffer the basin by specified
  distance and get more data, if needed.

## Value

`data.frame` with four columns: date, runoff, temperature,
precipitation.

## Details

Download the reanalysis archive from
[here](https://www.dropbox.com/sh/5xjnf620tlwfk7a/AABhTaPEDWLII8rV04dp0MWna?dl=0).

## Examples

``` r
if (require("sf") && require("ncdf4")) {

  library(grwat)
  library(dplyr)

  # example Spas-Zagorye daily runoff data is included with grwat package
  data_path = system.file("extdata", "spas-zagorye.txt",
                          package = "grwat")

  hdata_raw = read.delim(data_path, header = FALSE, 
                         sep = ' ', na.strings = c('-999', '-999.0', '-'),
                         col.names = c('d', 'm', 'y', 'q'))

  hdata = hdata_raw |>
    transmute(Date = lubridate::make_date(y, m, d),
              Q = q)

  head(hdata)

  # read basin
  basin_path = system.file("extdata", "spas-zagorye.gpkg",
                           package = "grwat")
  basin = sf::st_read(basin_path, layer = 'basin') # read basin region
  basin_buffer = gr_buffer_geo(basin, 25000)

  if (FALSE) { # \dontrun{
    # read reanalysis data
    rean = gr_read_rean(
      '/Volumes/Data/Spatial/Reanalysis/grwat/pre_1880-2021.nc',
      '/Volumes/Data/Spatial/Reanalysis/grwat/temp_1880-2021.nc'
    )

    # spatial join of reanalysis data to runoff data
    hdata_rean = gr_join_rean(hdata, rean, basin_buffer)

    head(hdata_rean)
  } # }

}
#> Loading required package: ncdf4
#> Reading layer `basin' from data source 
#>   `/home/runner/work/_temp/Library/grwat/extdata/spas-zagorye.gpkg' 
#>   using driver `GPKG'
#> Simple feature collection with 1 feature and 7 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 35.41204 ymin: 54.88195 xmax: 36.84138 ymax: 55.57005
#> Geodetic CRS:  WGS 84
```
