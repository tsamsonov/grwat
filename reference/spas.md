# Spas-Zagorye daily runoff data

A dataset containing the daily runoff data for
[Spas-Zagorye](https://allrivers.info/gauge/protva-obninsk) gauge on
[Protva](https://en.wikipedia.org/wiki/Protva) river in Central European
plane. The dataset is supplemented by meteorological variables
(temperature and precipitation) obtained from CIRES-DOE (1880-1949) and
ERA5 (1950-2021) data.

## Usage

``` r
spas
```

## Format

A data frame with 23742 rows and 4 variables:

- Date:

  date, in dates

- Q:

  daily runoff, in m3/s

- Temp:

  daily temperature, in Celsius degrees

- Prec:

  daily precipitation, in mm

## Source

<https://allrivers.info/gauge/protva-obninsk>

<https://gmvo.skniivh.ru>

<https://www.ecmwf.int/en/forecasts/dataset/ecmwf-reanalysis-v5>

<https://psl.noaa.gov/data/gridded/data.20thC_ReanV3.html>
