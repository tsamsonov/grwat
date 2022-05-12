\donttest{
  library(grwat)
  
  data(spas) # example Spas-Zagorye data is included with grwat package
  
  # separate
  sep = gr_separate(spas, params = gr_get_params(reg = 'center'))
  
  # summarize
  vars = gr_summarize(sep)
  
  # plot periods with fixed change year
  gr_plot_periods(vars, Qygr, year = 1978)
  
  # plot periods with change year from Pettitt test
  gr_plot_periods(vars, Qygr, tests = TRUE)
  
  # calculate test beforehand
  tests = gr_test_vars(vars)
  gr_plot_periods(vars, Qspmax, tests = tests)
  
  # use matrix layout to plot multiple variables
  gr_plot_periods(vars, Qygr, Qspmax, D10w1, Wsprngr,
                  layout = matrix(1:4, nrow = 2),
                  tests = tests)
}