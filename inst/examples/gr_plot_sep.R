library(grwat)

data(spas) # example Spas-Zagorye data is included with grwat package

# separate
sep = gr_separate(spas, params = gr_get_params(reg = 'center'))

# One year
gr_plot_sep(sep, 1978) 

# Two years
gr_plot_sep(sep, c(1978, 1989)) 

# Two years in a matrix layout
gr_plot_sep(sep, 1988:1989, layout = matrix(1:2, nrow = 2, byrow = TRUE)) 

# Add temperature and precipitation
gr_plot_sep(sep, 1991, temp = TRUE, prec = TRUE) 