if (require("sf") && require("ncdf4")) {

  library(grwat)
  library(dplyr)

  # example Spas-Zagorye daily runoff data is included with grwat package
  data_path = system.file("extdata", "spas-zagorye.txt",
                          package = "grwat")

  hdata_raw = read.delim(data_path, header = FALSE, 
                         sep = ' ', na.strings = c('-999', '-999.0', '-'),
                         col.names = c('d', 'm', 'y', 'q'))

  hdata = hdata_raw %>%
    transmute(Date = lubridate::make_date(y, m, d),
              Q = q)

  head(hdata)

  # read basin
  basin_path = system.file("extdata", "spas-zagorye.gpkg",
                           package = "grwat")
  basin = sf::st_read(basin_path, layer = 'basin') # read basin region
  basin_buffer = gr_buffer_geo(basin, 25000)

  \dontrun{
    # read reanalysis data
    rean = gr_read_rean(
      '/Volumes/Data/Spatial/Reanalysis/grwat/pre_1880-2021.nc',
      '/Volumes/Data/Spatial/Reanalysis/grwat/temp_1880-2021.nc'
    )

    # spatial join of reanalysis data to runoff data
    hdata_rean = gr_join_rean(hdata, rean, basin_buffer)

    head(hdata_rean)
  }

}
