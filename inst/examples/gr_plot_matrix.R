library(grwat)

data(spas) # example Spas-Zagorye data is included with grwat package

# separate
sep = gr_separate(spas, params = gr_get_params(reg = 'Midplain'))

# matrix plot for runoff
gr_plot_matrix(sep, type = 'runoff')

# matrix plot for season
gr_plot_matrix(sep, type = 'season')

# matrix plot for genetic component
gr_plot_matrix(sep, type = 'component')