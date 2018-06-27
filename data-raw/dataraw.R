rivers = sf::st_read('data-raw/rivers.gpkg')
rivers_europe = sf::st_read('data-raw/rivers_europe.gpkg')
lakes = sf::st_read('data-raw/lakes.gpkg')
lakes_europe = sf::st_read('data-raw/lakes_europe.gpkg')
ocean = sf::st_read('data-raw/ocean.gpkg')

params_out = readxl::read_excel('data-raw/params_out.xlsx')

devtools::use_data(rivers, 
                   rivers_europe, 
                   lakes, 
                   lakes_europe,
                   ocean,
                   overwrite = TRUE,
                   internal = TRUE)

devtools::use_data(params_out, 
                   overwrite = TRUE)
