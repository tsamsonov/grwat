library(grwat)

data(spas) # example Spas-Zagorye data is included with grwat package
head(spas)

params = gr_get_params(reg = 'Midplain')
params$filter = 'kudelin'

# for (i in 1:1000)
#   Qbase = gr_baseflow(spas$Q, method = 'lynehollick', 
#                       a = 0.925, passes = 3)

for (i in 1:1000)
  sep = suppressMessages(gr_separate(spas, params)) 
