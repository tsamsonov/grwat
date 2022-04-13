library(grwat)

# example Spas-Zagorye data is included with grwat package
data(spas)
head(spas)

gr_check_data(spas)

# raw Spas-Zagorye data represents date components
# in columns and does not contain meteorologgical variables
path = system.file("extdata", "spas-zagorye.txt", 
                   package = "grwat")

hdata_raw = readr::read_delim(path, 
                       col_names = c('d', 'm', 'y', 'q'), 
                       col_types = 'iiid', delim = ' ')

print(hdata_raw)

try(gr_check_data(hdata_raw))



