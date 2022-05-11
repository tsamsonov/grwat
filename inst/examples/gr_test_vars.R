\dontrun{
  library(grwat)
  
  data(spas) # example Spas-Zagorye data is included with grwat package
  
  # separate
  sep = gr_separate(spas, params = gr_get_params(reg = 'center'))
  
  # summarize
  vars = gr_summarize(sep)
  
  # test all variables
  tests = gr_test_vars(vars)
  
  # view Pettitt test for Qygr
  tests$ptt$Qygr
  
  # view Fisher test for Q30s
  tests$ft$Q30s
  
  # test only Qygr and Q30s using 1978 as fixed year and excluding 1988-1991 yrs
  gr_test_vars(vars, Qygr, Q30s, year = 1978, exclude = 1988:1991)
}