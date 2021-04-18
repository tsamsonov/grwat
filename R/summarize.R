#' Calculate various summary stats for separated hydrograph
#'
#' @param tab data frame resulting from `separate()` function
#'
#' @return data frame with one row for each water-resources year and multiple columns of statistics
#' @export
#'
#' @examples
grw_summarize <- function(tab) {
  
  secday = 86400
  kmyr = secday / 10e9
  
  limits = tab %>% 
    dplyr::mutate(Year1 = lubridate::year(Date),
                  Year2 = Year1+1) %>% 
    dplyr::group_by(Year1) %>% 
    dplyr::summarise(datestart = min(Date[which(Qseas>0)]),
                     datepolend = max(Date[which(Qseas>0)]))
  
  startflt = tab$Date %in% limits$datestart
  
  tab %>% 
    mutate(Year = if_else(Date %in% limits$datestart,
                          as.integer(year(Date)),
                          NA_integer_)) %>% 
    tidyr::fill(Year) %>% 
    filter(!is.na(Year)) %>% 
    group_by(Year) %>% 
    summarise(Year1 = min(Year),
              Year2 = max(year(Date)),
              datestart = min(Date),
              datepolend = max(Date[which(Qseas>0)]),
              polprod = datepolend - datestart,
              Qy = mean(Qin, na.rm = TRUE),
              Qmax = max(Qin, na.rm = TRUE),
              datemax = Date[which.max(Qin)[1]],
              Qygr = mean(Qbase, na.rm = TRUE),
              Wy = sum(Qin, na.rm = TRUE) * kmyr * n(),
              Wgr = sum(Qbase, na.rm = TRUE) * kmyr * n(),
              Wpol2 = sum(Qseas, na.rm = TRUE) * kmyr * n(),
              Wpol1 = Wpol2 + sum((Qseas > 0) * Qbase, na.rm = TRUE) * kmyr * n(),
              Wpol3 = Wpol1 + sum((Qseas > 0) * Qrain, na.rm = TRUE) * kmyr * n(),
              Wpavs2 = sum(Qrain, na.rm = TRUE) * kmyr * n(),
              Wpavs1 = Wpavs2 + sum((Qrain > 0) * Qbase, na.rm = TRUE) * kmyr * n(),
              Wpavthaw2 = sum(Qthaw, na.rm = TRUE) * kmyr * n(),
              Wpavthaw1 = Wpavs2 + sum((Qrain > 0) * Qbase, na.rm = TRUE) * kmyr * n(),
              Qmaxpavs = max(Qrain, na.rm = TRUE),
              Qmaxpavthaw = max(Qthaw, na.rm = TRUE),
              datemaxpavs = Date[which.max(Qrain)[1]],
              datemaxpavthaw = Date[which.max(Qthaw)[1]])
}