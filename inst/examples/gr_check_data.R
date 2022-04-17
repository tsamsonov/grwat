library(grwat)

# example Spas-Zagorye data is included with grwat package
data(spas)
head(spas)

gr_check_data(spas)

# raw Spas-Zagorye data represents date components
# in columns and does not contain meteorologgical variables
path = system.file("extdata", "spas-zagorye.txt", 
                   package = "grwat")

hdata_raw = read.delim(path, header = FALSE, 
                       sep = ' ', na.strings = c('-999', '-999.0', '-'),
                       col.names = c('d', 'm', 'y', 'q'))

print(hdata_raw)

try(gr_check_data(hdata_raw))



