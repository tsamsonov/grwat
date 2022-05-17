donttest = c('gr_baseflow.Rd', 'gr_kable_tests.Rd', 'gr_plot_minmonth.Rd', 'gr_plot_periods.Rd', 'gr_plot_sep.Rd', 
             'gr_plot_tests.Rd', 'gr_plot_vars.Rd', 'gr_summarize.Rd', 'gr_test_vars.Rd')

for (ex in donttest) {
  path = file.path('man', ex)
  docs = readLines(path)
  docs_1 = sub(pattern = '\\examples{', replacement = '\\examples{\\donttest{', docs, fixed = TRUE)
  docs_1[length(docs_1) - match("}", rev(docs_1)) + 1] = '}}'
  writeLines(docs_1, path)
}