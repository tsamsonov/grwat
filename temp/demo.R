library(grwat)
wd = "/Volumes/Data/Work/_grwat/Mezen_Malonisog/"
setwd(wd)
sep = grwat::read_separation('AllGrWat.txt')

grwat::plot_separation(sep, 1978)

grwat::plot_separation(sep, c(1994, 1995))

grwat::plot_separation(sep, 1994:1997, 
                       layout = matrix(c(1,2,3,4), nrow=2, byrow=TRUE))
