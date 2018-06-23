rivers = sf::st_read('data-raw/rivers.gpkg')
rivers_europe = sf::st_read('data-raw/rivers_europe.gpkg')
lakes = sf::st_read('data-raw/lakes.gpkg')
lakes_europe = sf::st_read('data-raw/lakes_europe.gpkg')
ocean = sf::st_read('data-raw/ocean.gpkg')

devtools::use_data(rivers, 
                   rivers_europe, 
                   lakes, 
                   lakes_europe,
                   ocean,
                   internal = TRUE)
