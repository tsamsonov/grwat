rivers = sf::st_read('data-raw/ne.gpkg', 'rivers')
rivers_europe = sf::st_read('data-raw/ne.gpkg', 'rivers_europe')
lakes = sf::st_read('data-raw/ne.gpkg', 'lakes')
lakes_europe = sf::st_read('data-raw/ne.gpkg', 'lakes_europe')
ocean = sf::st_read('data-raw/ne.gpkg', 'ocean')

params_out = readxl::read_excel('data-raw/params_out.xlsx')
params_in_desc = readxl::read_excel('data-raw/params_in.xlsx', 1)
params_in = readxl::read_excel('data-raw/params_in.xlsx', 2)

usethis::use_data(rivers, 
                 rivers_europe, 
                 lakes, 
                 lakes_europe,
                 ocean,
                 params_out,
                 params_in,
                 params_in_desc,
                 overwrite = TRUE,
                 internal = TRUE)
