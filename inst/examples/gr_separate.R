library(grwat)
print(spas) # example Spas-Zagorye data loaded with grwat package

sep = gr_separate(spas, params = gr_get_params(reg = 'Midplain'))
dplyr::filter(sep, Year == 2010) # View water resources year
gr_plot_sep(sep, 2010) # Plot separation
