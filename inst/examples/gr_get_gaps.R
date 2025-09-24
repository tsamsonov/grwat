library(grwat)
library(dplyr)

# example Spas-Zagorye data is included with grwat package
path = system.file("extdata", "spas-zagorye.txt", 
                   package = "grwat")

hdata_raw = read.delim(path, header = FALSE, 
                       sep = ' ', na.strings = c('-999', '-999.0', '-'),
                       col.names = c('d', 'm', 'y', 'q'))

hdata = hdata_raw |> 
  transmute(Date = lubridate::make_date(y, m, d), 
            Q = q)

head(hdata)

# identify gaps
gr_get_gaps(hdata) 

# fill gaps
fhdata = gr_fill_gaps(hdata, autocorr = 0.8)

# check the results
gr_get_gaps(fhdata)

# fill gaps
fhdata = gr_fill_gaps(hdata, nobserv = 7)

# check the results
gr_get_gaps(fhdata)
