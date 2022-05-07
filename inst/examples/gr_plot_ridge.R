if (require("ggridges")) {
  
  library(grwat)
  
  data(spas) # example Spas-Zagorye data is included with grwat package
  
  # separate
  sep = gr_separate(spas, params = gr_get_params(reg = 'center'))
  
  # ridgline plot for selected years
  gr_plot_ridge(sep, years = c(1960, 1965, 1989, 2001, 2012)) 

}