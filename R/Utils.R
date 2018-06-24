#' Multiple plot function
#' 
#' Produces matrix layout for ggplot graphics. 
#' Taken from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#' 
#' @param ... An arbitrary number of ggplot objects. Listed by colon
#' @param plotlist A list of ggplot objects
#' @param cols Integer. Number of columns in layout.
#' @param layout A matrix specifying the layout. If present, 'cols' is ignored. 
#'   If the layout is something like `matrix(c(1,2,3,3), nrow=2, byrow=TRUE)`, 
#'   then plot 1 will go in the upper left, 2 will go in the upper right, and
#'   3 will go all the way across the bottom.
#'
#' @return Plots the passed ggplot objects in a specified matrix layout
#'
#' @examples
#' \dontrun{
#' multiplot(g1, g2, layout=matrix(c(1,2,3,3), nrow=2, byrow=TRUE))
#' }
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
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#' Project data to the best conic projection
#'
#' @param s `sf` (simple features) object
#'
#' @return `sf` (simple features) object projected to automatically selected projection
#'
st_transform_opt <- function(s){
  box = st_bbox(s)
  # TODO: projections over 180 meridian
  L0 = 0.5 * (box[1] + box[3]) # central meridian
  db = box[4] - box[2] # latitude range
  B1 = box[2] + db * 0.25 # southern standard parallel
  B2 = box[2] + db * 0.75 # northern standard parallel
  
  datum = st_crs(s)$datum
  
  p4s = str_interp('+proj=lcc +lat_1=${B2} +lat_0=${B1} +lon_0=${L0} +datum=${datum} +no_defs')
  
  return(s %>% st_transform(p4s))
}