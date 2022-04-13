library(grwat)

data(spas) # example Spas-Zagorye data is included with grwat package

# separate
sep = gr_separate(spas, params = gr_get_params(reg = 'Midplain'))

# summarize
vars = gr_summarize(sep)

head(vars)

gr_plot_vars(vars, Qygr, tests = TRUE)
