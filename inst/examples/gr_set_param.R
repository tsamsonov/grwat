library(grwat)

data(spas) # example Spas-Zagorye data is included with grwat package

# Debug mode gives access to additional information
sep = gr_separate(spas, 
                  params = gr_get_params(reg = 'Midplain'), 
                  debug = TRUE)

# Visualize
gr_plot_sep(sep, c(1978, 1989)) 

# actual params used for each year
parlist = attributes(sep)$params

# set the ftrecdays parameter for multiple years
parlist = gr_set_param(parlist, ftrecdays, 
                       years = c(1978, 1989:1995), 
                       value = 15)

# use the list of parameters for separation
sep_new = gr_separate(spas, params = parlist, debug = TRUE)

# Visualize
gr_plot_sep(sep_new, c(1978, 1989))