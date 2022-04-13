library(grwat)

# read reanalysis data
\dontrun{
  rean = gr_read_rean(
    '/Volumes/Data/Spatial/Reanalysis/grwat/pre_1880-2021.nc',
    '/Volumes/Data/Spatial/Reanalysis/grwat/temp_1880-2021.nc'
  ) 
  
  str(rean)
}
