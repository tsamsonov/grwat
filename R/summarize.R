#' Hydrograph separation variables
#' 
#' Use this function to learn the meaning of the variables that are calculated by [grwat::gr_summarize()].
#'
#' @return `data.frame` of hydrograph separation variables
#' @export
#'
#' @example inst/examples/gr_help_vars.R
#' 
gr_help_vars <- function() {
  return(params_out)
}

#' Summarize hydrograph separation
#' 
#' Use this function to get meaningful summary statistics for hydrograph separation. Resulting variables are described by [grwat::gr_help_vars()]. This function is a convenient wrapper around [dplyr](https://dplyr.tidyverse.org)'s `df %>% group_by %>% summarize` idiom.
#'
#' @param df `data.frame` of hydrograph separation resulting from [grwat::gr_separate()] function
#'
#' @return `data.frame` with one row for each water-resources year and multiple columns of statistics.
#' @export
#'
#' @example inst/examples/gr_summarize.R
#' 
gr_summarize <- function(df) {
  
  secday = 86400
  kmyr = secday / 10e9
  
  limits = df %>% 
    dplyr::mutate(Year1 = lubridate::year(.data$Date),
                  Year2 = .data$Year1+1) %>% 
    dplyr::group_by(.data$Year1) %>% 
    dplyr::summarise(datestart = min(.data$Date[which(.data$Qseas>0)]),
                     datepolend = max(.data$Date[which(.data$Qseas>0)]))
  
  startflt = df$Date %in% limits$datestart
  
  df %>% 
    dplyr::mutate(Year = dplyr::if_else(.data$Date %in% limits$datestart,
                          as.integer(lubridate::year(.data$Date)),
                          NA_integer_)) %>% 
    tidyr::fill(.data$Year) %>% 
    dplyr::filter(!is.na(.data$Year)) %>% 
    dplyr::group_by(.data$Year) %>% 
    dplyr::summarise(Year1 = min(.data$Year),
              Year2 = max(lubridate::year(.data$Date)),
              datestart = min(.data$Date),
              datepolend = max(.data$Date[which(.data$Qseas>0)]),
              PolProd = as.integer(.data$datepolend - .data$datestart),
              Qy = mean(.data$Q, na.rm = TRUE),
              Qmax = max(.data$Q, na.rm = TRUE),
              datemax = .data$Date[which.max(.data$Q)[1]],
              Qygr = mean(.data$Qbase, na.rm = TRUE),
              Qmmsummer = min(.data$Q[.data$Type == 1], na.rm = T),
              monmmsummer = lubridate::make_date(lubridate::year(.data$Date[which.min(.data$Q / (.data$Type == 1))]), 
                                                 lubridate::month(.data$Date[which.min(.data$Q / (.data$Type == 1))]), 
                                                 01),
              Qmmwin = min(.data$Q[.data$Type == 2], na.rm = T),
              nommwin = lubridate::make_date(lubridate::year(.data$Date[which.min(.data$Q / (.data$Type == 2))]), 
                                             lubridate::month(.data$Date[which.min(.data$Q / (.data$Type == 2))]), 
                                             01),
              Q30s = condrollmean(.data$Q, .data$Type == 1, 30),
              date30s1 = .data$Date[condrollmeanidx(.data$Q, .data$Type == 1, 30)],
              date30s2 = .data$date30s1 + 29,
              Q30w = condrollmean(.data$Q, .data$Type == 2, 30),
              date30w1 = .data$Date[condrollmeanidx(.data$Q, .data$Type == 2, 30)],
              date30w2 = .data$date30w1 + 29,
              Q10s = condrollmean(.data$Q, .data$Type == 1, 10),
              date10s1 = .data$Date[condrollmeanidx(.data$Q, .data$Type == 1, 10)],
              date10s2 = .data$date10s1 + 9,
              Q10w = condrollmean(.data$Q, .data$Type == 2, 10),
              date10w1 = .data$Date[condrollmeanidx(.data$Q, .data$Type == 2, 10)],
              date10w2 = .data$date10w1 + 9,
              Q5s = condrollmean(.data$Q, .data$Type == 1, 5),
              date5s1 = .data$Date[condrollmeanidx(.data$Q, .data$Type == 1, 5)],
              date5s2 = .data$date5s1 + 4,
              Q5w = condrollmean(.data$Q, .data$Type == 2, 5),
              date5w1 = .data$Date[condrollmeanidx(.data$Q, .data$Type == 2, 5)],
              date5w2 = .data$date5w1 + 4,
              Wy = sum(.data$Q, na.rm = TRUE) * kmyr * dplyr::n(),
              Wgr = sum(.data$Qbase, na.rm = TRUE) * kmyr * dplyr::n(),
              Wpol2 = sum(.data$Qseas, na.rm = TRUE) * kmyr * dplyr::n(),
              Wpol1 = .data$Wpol2 + sum((.data$Qseas > 0) * .data$Qbase, na.rm = TRUE) * kmyr * dplyr::n(),
              Wpol3 = .data$Wpol1 + sum((.data$Qseas > 0) * .data$Qrain, na.rm = TRUE) * kmyr * dplyr::n(),
              Wpavs2 = sum(.data$Qrain, na.rm = TRUE) * kmyr * dplyr::n(),
              Wpavs1 = .data$Wpavs2 + sum((.data$Qrain > 0) * .data$Qbase, na.rm = TRUE) * kmyr * dplyr::n(),
              Wpavthaw2 = sum(.data$Qthaw, na.rm = TRUE) * kmyr * dplyr::n(),
              Wpavthaw1 = .data$Wpavs2 + sum((.data$Qrain > 0) * .data$Qbase, na.rm = TRUE) * kmyr * dplyr::n(),
              WgrS = sum(.data$Qbase * (.data$Type == 1), na.rm = TRUE) * kmyr * dplyr::n(),
              WS = sum(.data$Q * (.data$Type == 1), na.rm = TRUE) * kmyr * dplyr::n(),
              WgrW = sum(.data$Qbase * (.data$Type == 2), na.rm = TRUE) * kmyr * dplyr::n(),
              WW = sum(.data$Q * (.data$Type == 2), na.rm = TRUE) * kmyr * dplyr::n(),
              Qmaxpavs = max(.data$Qrain, na.rm = TRUE),
              Qmaxpavthaw = max(.data$Qthaw, na.rm = TRUE),
              datemaxpavs = .data$Date[which.max(.data$Qrain)[1]],
              datemaxpavthaw = .data$Date[which.max(.data$Qthaw)[1]],
              SumProd = sum(.data$Type == 1),
              DaysPavsSum = sum((.data$Type == 1) & (.data$Qrain > 0)),
              WinProd = sum(.data$Type == 2),
              DaysThawWin = sum((.data$Type == 2) & (.data$Qthaw > 0)),
              CvWin = sd(.data$Q[.data$Type == 2], na.rm = TRUE) / mean(.data$Q[.data$Type == 2], na.rm = TRUE),
              CvSum = sd(.data$Q[.data$Type == 1], na.rm = TRUE) / mean(.data$Q[.data$Type == 1], na.rm = TRUE),
              CountPavs = sum(rle(.data$Qrain > 0)$values),
              CountThaws = sum(rle(.data$Qthaw > 0)$values)) %>% 
    dplyr::ungroup()
}