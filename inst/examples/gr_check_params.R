library(grwat)

# example Spas-Zagorye data is included with grwat package
data(spas)

params = gr_get_params(reg = 'center')

gr_check_params(params)

# set the unknown parameter
params$new = -2

# use try if you do not want to stop at error
try(gr_check_params(params))

# remove wrong parameter
params$new = NULL

# remove right parameter
params$grad1 = NULL
try(gr_check_params(params))

# reset
params = gr_get_params(reg = 'center')

sep = gr_separate(spas, params, debug = TRUE)
parlist = attributes(sep)$params

parlist[['2002']]$grad1 = 4

# if the parlist is used for separation
# then data frame must be specified
try(gr_check_params(parlist))

gr_check_params(parlist, spas)

# grad parameter is not known
parlist[['2002']]$grad = 4
try(gr_check_params(parlist, spas))

# remove wrong parameter
parlist[['2002']]$grad = NULL

# remove year
parlist[['2002']] = NULL
try(gr_check_params(parlist, spas))

parlist[['2002']] = parlist[['2001']]
gr_check_params(parlist, spas)

