if (require("sf") && require("ncdf4")) {
  
  library(grwat)
  
  prec_path = system.file("extdata", "era5_daily_prec.nc",
                          package = "grwat")
  
  temp_path = system.file("extdata", "era5_daily_temp.nc",
                          package = "grwat")
  
  # read reanalysis data
  rean = gr_read_era5_daily(prec_path, temp_path) 
  
  str(rean)
}
