library(grwat)

data(spas) # example Spas-Zagorye data is included with grwat package

# separate
sep = gr_separate(spas, params = gr_get_params(reg = 'Midplain'))

# Visualize
gr_plot_sep(sep, c(1978, 1989)) 

# Debug mode gives access to additional information
sep_debug = gr_separate(spas, 
                        params = gr_get_params(reg = 'Midplain'), 
                        debug = TRUE)

# a vector of years with jittered params
jit = attributes(sep_debug)$jittered
print(jit)

# actual params used for each year
parlist = attributes(sep_debug)$params
partab = do.call(dplyr::bind_rows, parlist) # View as table
head(partab)

# extract and tweak parameters for selected year
p = parlist[['1989']]
p$grad1 = 1
p$grad2 = 2.5

# use tweaked parameters for all years
sep_debug = gr_separate(spas, params = p, debug = TRUE)

# Visualize
gr_plot_sep(sep_debug, c(1978, 1989)) 

# actual params used for each year
parlist = attributes(sep_debug)$params

# tweak parameters for selected year
parlist[['1989']]$grad1 = 3
parlist[['1989']]$grad2 = 6

# set the ftrecdays parameter for multiple years
parlist = gr_set_param(parlist, ftrecdays, 
                       years = c(1978, 1989:1995), 
                       value = 15)

# use the list of parameters for separation
sep_debug = gr_separate(spas, params = parlist, debug = TRUE)

# Visualize
gr_plot_sep(sep_debug, c(1978, 1989))



