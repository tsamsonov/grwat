\dontrun{
  library(grwat)
  
  data(spas) # example Spas-Zagorye data is included with grwat package
  
  # separate
  sep = gr_separate(spas, params = gr_get_params(reg = 'center'))
  
  # summarize
  vars = gr_summarize(sep)
  
  # report
  report = '~/Spas-Zagorye.html'
  
  gr_report(sep, vars, output = report)
  browseURL(report)
}
