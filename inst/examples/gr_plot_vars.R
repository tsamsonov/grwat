library(grwat)

data(spas) # example Spas-Zagorye data is included with grwat package

# separate
sep = gr_separate(spas, params = gr_get_params(reg = 'Midplain'))

# summarize
vars = gr_summarize(sep)

# plot one selected variable
gr_plot_vars(vars, Qygr) 

# plot one selected variable
gr_plot_vars(vars, datestart) 

# plot two variables sequentially
gr_plot_vars(vars, date10w1, Wpol3)

# plot four variables in matrix layout
gr_plot_vars(vars, Qmax, Qygr, date10w1, Wpol3,
             layout = matrix(1:4, nrow = 2, byrow = TRUE)) 

# add tests calculated on the fly (only plotted variables are tested)
gr_plot_vars(vars, Qmax, Qygr, date10w1, Wpol3,
             layout = matrix(1:4, nrow = 2, byrow = TRUE),
             tests = TRUE) 

# calculate tests beforehand
tests = gr_test_vars(vars)
gr_plot_vars(vars, date10w1, Wpol3, DaysThawWin, Qmaxpavs,
             layout = matrix(1:4, nrow = 2, byrow = TRUE),
             tests = tests) 