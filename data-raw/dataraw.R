# DATA PREPARATION

params_out = readxl::read_excel('data-raw/params_out.xlsx')
params_in_desc = readxl::read_excel('data-raw/params_in.xlsx', 1)
params_in = readxl::read_excel('data-raw/params_in.xlsx', 2)

spas = readr::read_delim(
  'inst/extdata/spas-zagorye-meteo.txt', 
  col_names = c('Date', 'Q', 'Temp', 'Prec'),
  col_types = 'Dddd', delim = ' '
)

usethis::use_data(params_out,
                  params_in,
                  params_in_desc,
                  internal = TRUE,
                  overwrite = TRUE)

usethis::use_data(spas,
                  overwrite = TRUE)
