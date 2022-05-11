\dontrun{
  library(grwat)
  
  data(spas) # example Spas-Zagorye data is included with grwat package
  
  # separate
  sep = gr_separate(spas, params = gr_get_params(reg = 'center'))
  
  # summarize
  vars = gr_summarize(sep)
  
  # test all variables
  tests = gr_test_vars(vars)
  
  # plot change year from Pettitt test
  gr_plot_tests(tests, type = 'year')
}