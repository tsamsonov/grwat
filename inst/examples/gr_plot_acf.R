library(grwat)

# example Spas-Zagorye data is included with grwat package
data(spas)
print(spas)

# plot ACF
gr_plot_acf(spas, 0.65)
