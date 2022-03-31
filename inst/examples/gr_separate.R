library(grwat)

data(spas) # example Spas-Zagorye data is included with grwat package

# separate
sep = gr_separate(spas, params = gr_get_params(reg = 'Midplain'))

# Visualize
gr_plot_sep(sep, c(1978, 1989)) 

# Debug mode gives access to additional information
sep_debug = gr_separate(spas, params = gr_get_params(reg = 'Midplain'), debug = TRUE)

# a vector of years with jittered params
jit = attributes(sep_debug)$jittered
jit

# actual params used for each year
parlist = attributes(sep_debug)$params
partab = do.call(dplyr::bind_rows, parlist) # View as table
head(partab)
