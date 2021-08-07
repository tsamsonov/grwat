condrollmean = function(values, needed, w) {
  w1 = min(which(needed))
  w2 = max(which(needed))
  idx = 1:length(values)
  
  cleaned = values
  cleaned[idx < w1 | idx > (w2-w)] <- NA
  means = zoo::rollmean(cleaned, w, align = 'left')
  
  min(means, na.rm = TRUE)
}

condrollmeanidx = function(values, needed, w) {
  w1 = min(which(needed))
  w2 = max(which(needed))
  idx = 1:length(values)
  
  cleaned = values
  cleaned[idx < w1 | idx > (w2-w)] <- NA
  means = zoo::rollmean(cleaned, w, align = 'left')
  
  which.min(means)[1]
}

#' Calculate various summary stats for separated hydrograph
#'
#' @param tab data frame resulting from `separate()` function
#'
#' @return data frame with one row for each water-resources year and multiple columns of statistics
#' @export
#'
#' @examples
gr_summarize <- function(tab) {
  
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
                          as.integer(lubridate::year(Date)),
                          NA_integer_)) %>% 
    tidyr::fill(Year) %>% 
    filter(!is.na(Year)) %>% 
    group_by(Year) %>% 
    summarise(Year1 = min(Year),
              Year2 = max(lubridate::year(Date)),
              datestart = min(Date),
              datepolend = max(Date[which(Qseas>0)]),
              PolProd = as.integer(datepolend - datestart),
              Qy = mean(Q, na.rm = TRUE),
              Qmax = max(Q, na.rm = TRUE),
              datemax = Date[which.max(Q)[1]],
              Qygr = mean(Qbase, na.rm = TRUE),
              Qmmsummer = min(Q[Qtype == 1], na.rm = T),
              monmmsummer = lubridate::make_date(lubridate::year(Date[which.min(Q[Qtype == 1])]), 
                                                 lubridate::month(Date[which.min(Q[Qtype == 1])]), 
                                                 01),
              Qmmwin = min(Q[Qtype == 2], na.rm = T),
              nommwin = lubridate::make_date(lubridate::year(Date[which.min(Q[Qtype == 2])]), 
                                             lubridate::month(Date[which.min(Q[Qtype == 2])]), 
                                             01),
              Q30s = condrollmean(Q, Qtype == 1, 30),
              date30s1 = Date[condrollmeanidx(Q, Qtype == 1, 30)],
              date30s2 = date30s1 + 29,
              Q30w = condrollmean(Q, Qtype == 2, 30),
              date30w1 = Date[condrollmeanidx(Q, Qtype == 2, 30)],
              date30w2 = date30w1 + 29,
              Q10s = condrollmean(Q, Qtype == 1, 10),
              date10s1 = Date[condrollmeanidx(Q, Qtype == 1, 10)],
              date10s2 = date10s1 + 9,
              Q10w = condrollmean(Q, Qtype == 2, 10),
              date10w1 = Date[condrollmeanidx(Q, Qtype == 2, 10)],
              date10w2 = date10w1 + 9,
              Q5s = condrollmean(Q, Qtype == 1, 5),
              date5s1 = Date[condrollmeanidx(Q, Qtype == 1, 5)],
              date5s2 = date5s1 + 4,
              Q5w = condrollmean(Q, Qtype == 2, 5),
              date5w1 = Date[condrollmeanidx(Q, Qtype == 2, 5)],
              date5w2 = date5w1 + 4,
              Wy = sum(Q, na.rm = TRUE) * kmyr * n(),
              Wgr = sum(Qbase, na.rm = TRUE) * kmyr * n(),
              Wpol2 = sum(Qseas, na.rm = TRUE) * kmyr * n(),
              Wpol1 = Wpol2 + sum((Qseas > 0) * Qbase, na.rm = TRUE) * kmyr * n(),
              Wpol3 = Wpol1 + sum((Qseas > 0) * Qrain, na.rm = TRUE) * kmyr * n(),
              Wpavs2 = sum(Qrain, na.rm = TRUE) * kmyr * n(),
              Wpavs1 = Wpavs2 + sum((Qrain > 0) * Qbase, na.rm = TRUE) * kmyr * n(),
              Wpavthaw2 = sum(Qthaw, na.rm = TRUE) * kmyr * n(),
              Wpavthaw1 = Wpavs2 + sum((Qrain > 0) * Qbase, na.rm = TRUE) * kmyr * n(),
              WgrS = sum(Qbase * (Qtype == 1), na.rm = TRUE) * kmyr * n(),
              WS = sum(Q * (Qtype == 1), na.rm = TRUE) * kmyr * n(),
              WgrW = sum(Qbase * (Qtype == 2), na.rm = TRUE) * kmyr * n(),
              WW = sum(Q * (Qtype == 2), na.rm = TRUE) * kmyr * n(),
              Qmaxpavs = max(Qrain, na.rm = TRUE),
              Qmaxpavthaw = max(Qthaw, na.rm = TRUE),
              datemaxpavs = Date[which.max(Qrain)[1]],
              datemaxpavthaw = Date[which.max(Qthaw)[1]],
              SumProd = sum(Qtype == 1),
              DaysPavsSum = sum((Qtype == 1) & (Qrain > 0)),
              WinProd = sum(Qtype == 2),
              DaysThawWin = sum((Qtype == 2) & (Qthaw > 0)),
              CvWin = sd(Q[Qtype == 2], na.rm = TRUE) / mean(Q[Qtype == 2], na.rm = TRUE),
              CvSum = sd(Q[Qtype == 1], na.rm = TRUE) / mean(Q[Qtype == 1], na.rm = TRUE),
              CountPavs = sum(rle(Qrain > 0)$values),
              CountThaws = sum(rle(Qthaw > 0)$values))
}