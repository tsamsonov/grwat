library(grwat)
wd = "/Volumes/Data/Work/_grwat/Mezen_Malonisog/"
setwd(wd)

df = data.frame(one = 1:5, two = 6:10)

sel = function(df, ...){
  df %>% dplyr::select(...)
}

sel(df, one, two)

prnt = function(...){
  print(length(...))
}

sel(df, one, two)


prnt(one, two, three)
