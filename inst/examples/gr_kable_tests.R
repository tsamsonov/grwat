if (require("kableExtra")) {
  
  library(grwat)
  
  data(spas) # example Spas-Zagorye data is included with grwat package
  
  # separate
  sep = gr_separate(spas, params = gr_get_params(reg = 'center'))
  
  # summarize from 1965 to 1990
  vars = gr_summarize(sep, 1965, 1990)
  
  # test all variables
  tests = gr_test_vars(vars)
  
  # kable tests
  gr_kable_tests(tests)

}