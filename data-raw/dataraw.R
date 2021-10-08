# DATA PREPARATION

params_out = readxl::read_excel('data-raw/params_out.xlsx')
params_in_desc = readxl::read_excel('data-raw/params_in.xlsx', 1)
params_in = readxl::read_excel('data-raw/params_in.xlsx', 2)

# read basin region
basin_buffer = sf::st_read('inst/extdata/spas-zagorye.gpkg', 
                layer = 'basin') |> 
  grwat::gr_buffer_geo(25000) 

# read gauge data
hdata = readr::read_delim(
  'inst/extdata/spas-zagorye-full.txt', 
  col_names = c('d', 'm', 'y', 'q'),
  col_types = 'iiid', delim = ' '
) |> 
  dplyr::transmute(
    Date = lubridate::make_date(y, m, d), 
    Q = q
  )

rean = grwat::gr_read_rean('/Volumes/Data/Spatial/Reanalysis/grwat/pre_1880-2021.nc',
                           '/Volumes/Data/Spatial/Reanalysis/grwat/temp_1880-2021.nc')

# join reanalysis data to hydrological series
spas = grwat::gr_join_rean(hdata, rean, basin_buffer) 

usethis::use_data(params_out,
                  params_in,
                  params_in_desc,
                  internal = TRUE,
                  overwrite = TRUE)

usethis::use_data(spas,
                  overwrite = TRUE)
