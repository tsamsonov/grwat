\donttest{
  library(grwat)
  
  data(spas) # example Spas-Zagorye data is included with grwat package
  
  # separate
  sep = gr_separate(spas, params = gr_get_params(reg = 'center'))
  
  # summarize
  vars = gr_summarize(sep)
  
  # plot minimum runoff month for two periods divided by Pettitt test
  gr_plot_minmonth(vars, tests = gr_test_vars(vars))
  
  # plot minimum runoff month for two periods divided by fixed year
  gr_plot_minmonth(vars, year = 1978)
}
