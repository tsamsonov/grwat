rivers = sf::st_read('data-raw/ne.gpkg', 'rivers')
rivers_europe = sf::st_read('data-raw/ne.gpkg', 'rivers_europe')
lakes = sf::st_read('data-raw/ne.gpkg', 'lakes')
lakes_europe = sf::st_read('data-raw/ne.gpkg', 'lakes_europe')
ocean = sf::st_read('data-raw/ne.gpkg', 'ocean')

params_out = readxl::read_excel('data-raw/params_out.xlsx')

usethis::use_data(rivers, 
                   rivers_europe, 
                   lakes, 
                   lakes_europe,
                   ocean,
                   params_out,
                   overwrite = TRUE,
                   internal = TRUE)
