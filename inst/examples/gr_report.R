library(grwat)

data(spas) # example Spas-Zagorye data is included with grwat package

# separate
sep = gr_separate(spas, params = gr_get_params(reg = 'Midplain'))

# summarize
vars = gr_summarize(sep)

# report
report = '~/Spas-Zagorye.html'

\dontrun{
  gr_report(sep, vars, output = report)
  browseURL(report)
}
