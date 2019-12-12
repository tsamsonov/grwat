#' Perform scale-space transform of hydrograph
#'
#' @param df data frame containing Date and Qin columns
#' @param year year to be analyzed (will be used to filter Date)
#' @param sigma maximum sigma used for scale-space smoothing
#'
#' @return Scale4C object with full scale-space transform information
#' @export
#'
#' @examples
ss_transform <- function(df, year = NULL, sigma = NULL){
  
  # select the first year if not provided
  if (is.null(year))
    year = lubridate::year(min(df$Date))
  
  tab = df %>%
    dplyr::filter(lubridate::year(Date) == year) %>% 
    dplyr::select(Date, Qin) %>% 
    dplyr::mutate(Date = as.integer(Date))
  
  N = nrow(tab)
  
  if(is.null(sigma))
    sigma = N
  else if (is.numeric(sigma))
    stopifnot(sigma > 0)
  else stop('sigma must be a positive numeric value')
  
  ranges = GenomicRanges::GRanges(rep(year, N),
                                  IRanges::IRanges(1:N, 1:N),
                                  reads = tab$Qin,
                                  meanPosition = 1:N)
  
  ssdata = Scale4C::Scale4C(rawData = ranges, 
                            viewpoint = 1, 
                            viewpointChromosome = year)
  
  scaleSpace(ssdata) = Scale4C::calculateScaleSpace(ssdata, maxSQSigma = sigma)
  ssdata = Scale4C::calculateFingerprintMap(ssdata, maxSQSigma = sigma)
  singularities(ssdata) = Scale4C::findSingularities(ssdata, 1, guessViewpoint = FALSE)
  
  return(ssdata)
  
}

#' Get scale-space tree data frame from Scale4C object
#'
#' @param ssdata 
#'
#' @return
#' @export
#'
#' @examples
ss_tree = function(ssdata) {
  tree = Scale4C::outputScaleSpaceTree(ssdata, outputPeaks = FALSE, useLog = FALSE)
  
  sstree = tree %>%
    mutate(id = row_number()) %>% 
    rowwise() %>% 
    do({
      tibble(id = .$id,
             position = rep(c('left','center','right'), each = 2),
             idrect = paste(id, position, sep = '_'),
             type = rep(c(.$left_type, .$centre_type, .$right_type), each = 2),
             type_position = paste(type, position, sep = '_'),
             day = as.numeric(c(.$left_leftPos, .$left_rightPos, 
                                .$centre_leftPos, .$centre_rightPos, 
                                .$right_leftPos, .$right_rightPos)),
             smin = as.numeric(c(.$left_minSSQ, .$left_minSSQ, 
                                 .$centre_minSSQ, .$centre_minSSQ, 
                                 .$right_minSSQ, .$right_minSSQ)),
             smax = as.numeric(c(.$left_maxSSQ, .$left_maxSSQ, 
                                 .$centre_maxSSQ, .$centre_maxSSQ, 
                                 .$right_maxSSQ, .$right_maxSSQ)))
    }) %>% 
    ungroup()
  
  return(sstree)
}