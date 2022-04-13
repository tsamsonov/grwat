if (require("gganimate")) {
  
  library(grwat)
  
  # example Spas-Zagorye data is included with grwat package
  data(spas)
  head(spas)
  
  # select years if needed
  df = spas[spas$Date < as.Date('1960-01-01'), ]
  
  # animate
  gr_animate(df)
  
}
