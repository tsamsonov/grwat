library(rstudioapi)
library(rmarkdown)
library(stringr)

# wd = "/Volumes/Data/Work/_Kireeva/2018/out/"
wd = "X:/Work/Kireeva/2018/out/"

# Set working directory to current
wd_cur = dirname(getActiveDocumentContext()$path)

# list basins
setwd(wd)
basins = list.dirs(recursive = FALSE, full.names = FALSE)

# Generate reports for each gauge
for (basin in basins) {
  setwd(wd)
  setwd(basin)
  gauges = list.dirs(recursive = FALSE, full.names = FALSE)
  
  for (gauge in gauges){
    setwd(gauge)
    
    fullpath = getwd()
    
    rmarkdown::render(input = str_interp('${wd_cur}/Report.Rmd'), 
                      output_file = 'report.pdf',
                      output_dir = fullpath,
                      knit_root_dir = fullpath,
                      encoding = 'UTF-8',
                      params = list(name = gauge,
                                    namen = gauge,
                                    file_params = str_interp('${wd_cur}/data/params_out.xlsx'),
                                    file_utils = str_interp('${wd_cur}/Utils.R')))
    setwd(wd)
    setwd(basin)
    break
  }
  break
}