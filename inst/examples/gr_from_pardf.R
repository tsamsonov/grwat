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

# tweak parameters for selected year
parlist[['1989']]$grad1 = 3
parlist[['1989']]$grad2 = 6

# tabular representation of parameters
partab = gr_to_pardf(parlist)

head(partab)

partab |> 
  dplyr::filter(year == 1989) |> 
  head()

parlist_back = gr_from_pardf(partab)

head(parlist_back[['1989']])
