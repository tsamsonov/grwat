library(grwat)

data(spas) # example Spas-Zagorye data is included with grwat package

# separate
sep = gr_separate(spas, params = gr_get_params(reg = 'center'))

# One year
gr_plot_sep(sep, 1978) 

# Two years
gr_plot_sep(sep, c(1978, 1989)) 

# Four years in a matrix layout
gr_plot_sep(sep, 1988:1991, layout = matrix(1:4, nrow = 2, byrow = TRUE)) 

# Add temperature
gr_plot_sep(sep, 1991, temp = TRUE) 

# Add precipitation
gr_plot_sep(sep, 1991, prec = TRUE) 

# Increase cumulative sum span for precipitation
gr_plot_sep(sep, 1991, prec = TRUE, span = 10) 

# Add both
gr_plot_sep(sep, 1991, temp = TRUE, prec = TRUE) 