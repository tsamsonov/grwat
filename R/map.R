#' Map reanalysis and basin data
#'
#' @param pts sf object with reanalysis points
#' @param pts_sel sf object with selected reanalysis points
#' @param region sf object with basin region
#' @param buffer sf object with buffered basin region
#'
#' @return plot object
#' @export
grw_map <- function(pts, pts_sel, region, buffer = NULL){
  
  if(!is.null(buffer)){
    plot(buffer %>% st_geometry(), 
         col = rgb(1, 0, 0, 0.2), 
         border = rgb(1, 0, 0), 
         lwd = 0.5,
         axes = TRUE,
         graticule = st_graticule(pts))  
    plot(region, 
         col = rgb(1, 0, 0, 0.5), 
         border = rgb(1, 0, 0), 
         add = TRUE)
  } else {
    plot(region %>% st_geometry(), 
         col = rgb(1, 0, 0, 0.5), 
         border = rgb(1, 0, 0),
         axes = TRUE,
         graticule = st_graticule(pts))
  }
  
  plot(rivers, 
       col = 'steelblue4', 
       lwd = 0.5, 
       add = TRUE)
  
  plot(rivers_europe, 
       col = 'steelblue4', 
       lwd = 0.2, 
       add = TRUE)
  
  plot(lakes, border = 'steelblue4', 
       col = 'skyblue', 
       lwd = 0.2, 
       add = TRUE)
  
  plot(lakes_europe, 
       border = 'steelblue4', 
       col = 'skyblue', 
       lwd = 0.2, 
       add = TRUE)
  
  plot(ocean, 
       border = 'steelblue4', 
       col = 'skyblue', 
       lwd = 0.5, 
       add = TRUE)
  
  plot(pts, 
       pch = 19, 
       col = 'gray30', 
       cex = 0.3, 
       add = TRUE)
  
  plot(pts_sel, 
       pch = 19, 
       col = 'black', 
       cex = 0.7, 
       add = TRUE)
  
  box(lwd = 0.2, 
      col = 'black')
}