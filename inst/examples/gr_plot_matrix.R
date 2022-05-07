library(grwat)

data(spas) # example Spas-Zagorye data is included with grwat package

# separate
sep = gr_separate(spas, params = gr_get_params(reg = 'center'))

# matrix plot for runoff
gr_plot_matrix(sep, type = 'runoff')

# matrix plot for seasons
gr_plot_matrix(sep, type = 'season')

# matrix plot for genetic components
gr_plot_matrix(sep, type = 'component')