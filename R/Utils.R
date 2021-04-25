# Multiple plot function
# 
# Produces matrix layout for ggplot graphics. 
# Taken from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#
#  Layout is a matrix specifying the layout. If present, 'cols' is ignored. 
#  If the layout is something like `matrix(c(1,2,3,3), nrow=2, byrow=TRUE)`, 
#  then plot 1 will go in the upper left, 2 will go in the upper right, and
#  3 will go all the way across the bottom.

multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
    }
  }
}

st_transform_opt <- function(s){
  box = sf::st_bbox(s)
  # TODO: projections over 180 meridian
  L0 = 0.5 * (box[1] + box[3]) # central meridian
  db = box[4] - box[2] # latitude range
  B1 = box[2] + db * 0.25 # southern standard parallel
  B2 = box[2] + db * 0.75 # northern standard parallel
  
  datum = sf::st_crs(s)$datum
  
  p4s = stringr::str_interp('+proj=lcc +lat_1=${B2} +lat_0=${B1} +lon_0=${L0} +datum=${datum} +no_defs')
  
  return(s %>% sf::st_transform(p4s))
}

replace_year <- function(d) {
  dates = sapply(d, function(X) {
    if (!is.na(X)){
      if (lubridate::month(X) < 7) {
        lubridate::year(X) = 2001
      }
      return(X)
    } else return(NA)
  })
  
  return(as.Date(dates, origin = "1970-01-01")) # TODO: may need earlier dates!
  
  # return(do.call(c, dates)) # if simply unlist then dates are killed, so using the c() function
}

get_idx <- function(s, x) {
  stringr::str_locate(s, x)[1, 1]
}

get_col_type = function(s) {
  switch(s,
         Date = readr::col_date(format = "%d%.%m%.%Y"),
         double = readr::col_double(),
         integer = readr::col_integer())
}

#' Get hydrograph parameters list
#'
#' @return data.frame of parameters
#' @export
#'
#' @examples
#' grwat::get_variables()
gr_help_vars <- function(){
  return(params_out)
}