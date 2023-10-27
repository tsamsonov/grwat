library(grwat)

data(spas) # example Spas-Zagorye data is included with grwat package

# separate
sep = gr_separate(spas, params = gr_get_params(reg = 'center'))

# summarize from 1965 to 1990
vars = gr_summarize(sep, 1965, 1990)

# plot one selected variable
gr_plot_vars(vars, Qygr) 

# plot two variables sequentially
gr_plot_vars(vars, D10w1, Wsprngr)

# four variables in matrix layout with tests calculated on the fly
gr_plot_vars(vars, Qspmax, Qygr, D10w1, Wsprngr,
             layout = matrix(1:4, nrow = 2, byrow = TRUE),
             tests = TRUE) 