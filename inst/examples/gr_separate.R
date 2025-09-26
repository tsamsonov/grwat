library(grwat)

data(spas) # example Spas-Zagorye data is included with grwat package
head(spas)

# separate
sep = gr_separate(spas, params = gr_get_params(reg = 'center'))

# Visualize
gr_plot_sep(sep, c(1978, 1989)) 

# Debug mode gives access to additional information
sep_debug = gr_separate(spas, 
                        params = gr_get_params(reg = 'center'), 
                        debug = TRUE)

# a vector of years with jittered params
jit = attributes(sep_debug)$jittered
print(jit)

# actual params used for each year
parlist = attributes(sep_debug)$params

# tabular representation of parameters
partab = gr_to_pardf(parlist)
head(partab)

parlist2 = partab |> 
  dplyr::select(-year) |> 
  apply(1, as.list) |> 
  lapply(\(X) {
    n = length(X)
    X[1:(n - 1)] <- lapply(X[1:(n - 1)], as.numeric)
    return(X)
  }) |> 
  setNames(partab$year)

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

# set the sprecdays parameter for multiple years
parlist = gr_set_param(parlist, sprecdays, 
                       years = c(1978, 1989:1995), 
                       value = 15)

# set the spcomp parameter for all years
parlist = gr_set_param(parlist, spcomp, value = 2.5)

# use the list of parameters for separation
sep_debug = gr_separate(spas, params = parlist, debug = TRUE)

# Visualize
gr_plot_sep(sep_debug, c(1978, 1989))



