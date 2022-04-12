library(grwat)
library(ggplot2)

path = system.file("extdata", "spas-zagorye.gpkg", package = "grwat")
basin = sf::st_read(path, layer = 'basin') # read basin region

basin_buffer = gr_buffer_geo(basin, 25000) 

ggplot() +
  geom_sf(data = basin_buffer, fill = 'orangered', color = 'black') +
  geom_sf(data = basin, fill = 'steelblue', color = 'black')
