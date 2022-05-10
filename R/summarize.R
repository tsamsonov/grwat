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
    dplyr::summarise(Dspstart = min(.data$Date[which(.data$Qspri>0)]),
                     Dspend = max(.data$Date[which(.data$Qspri>0)]))
  
  startflt = df$Date %in% limits$Dspstart
  
  df %>% 
    dplyr::filter(!is.na(.data$Year)) %>% 
    dplyr::group_by(.data$Year) %>% 
    dplyr::summarise(Year1 = min(.data$Year),
              Year2 = max(lubridate::year(.data$Date)),
              Dspstart = min(.data$Date),
              Dspend = max(.data$Date[which(.data$Qspri>0)]),
              Tsp = as.integer(.data$Dspend - .data$Dspstart),
              Qy = mean(.data$Q, na.rm = TRUE),
              Qspmax = max(.data$Q, na.rm = TRUE),
              Dspmax = .data$Date[which.max(.data$Q)[1]],
              Qygr = mean(.data$Qbase, na.rm = TRUE),
              Qsmin = min(.data$Q[.data$Type == 1], na.rm = T),
              Dsmin = .data$Date[which.min(.data$Q / (.data$Type == 1))],
              Qwmin = min(.data$Q[.data$Type == 2], na.rm = T),
              Dwmin = .data$Date[which.min(.data$Q / (.data$Type == 2))],
              Q30s = condrollmean(.data$Q, .data$Type == 1, 30),
              D30s1 = .data$Date[condrollmeanidx(.data$Q, .data$Type == 1, 30)],
              D30s2 = .data$D30s1 + 29,
              Q30w = condrollmean(.data$Q, .data$Type == 2, 30),
              D30w1 = .data$Date[condrollmeanidx(.data$Q, .data$Type == 2, 30)],
              D30w2 = .data$D30w1 + 29,
              Q10s = condrollmean(.data$Q, .data$Type == 1, 10),
              D10s1 = .data$Date[condrollmeanidx(.data$Q, .data$Type == 1, 10)],
              D10s2 = .data$D10s1 + 9,
              Q10w = condrollmean(.data$Q, .data$Type == 2, 10),
              D10w1 = .data$Date[condrollmeanidx(.data$Q, .data$Type == 2, 10)],
              D10w2 = .data$D10w1 + 9,
              Q5s = condrollmean(.data$Q, .data$Type == 1, 5),
              D5s1 = .data$Date[condrollmeanidx(.data$Q, .data$Type == 1, 5)],
              D5s2 = .data$D5s1 + 4,
              Q5w = condrollmean(.data$Q, .data$Type == 2, 5),
              D5w1 = .data$Date[condrollmeanidx(.data$Q, .data$Type == 2, 5)],
              D5w2 = .data$D5w1 + 4,
              Wy = sum(.data$Q, na.rm = TRUE) * kmyr,
              Wygr = sum(.data$Qbase, na.rm = TRUE) * kmyr,
              Wsp = sum(.data$Qspri, na.rm = TRUE) * kmyr,
              Wspgr = .data$Wsp + sum((.data$Qspri > 0) * .data$Qbase, na.rm = TRUE) * kmyr,
              Wsprngr = .data$Wspgr + sum((.data$Qspri > 0) * .data$Qrain, na.rm = TRUE) * kmyr,
              Wrn = sum(.data$Qrain, na.rm = TRUE) * kmyr,
              Wrngr = .data$Wrn + sum((.data$Qrain > 0) * .data$Qbase, na.rm = TRUE) * kmyr,
              Wth = sum(.data$Qthaw, na.rm = TRUE) * kmyr,
              Wthgr = .data$Wth + sum((.data$Qthaw > 0) * .data$Qbase, na.rm = TRUE) * kmyr,
              Wgrs = sum(.data$Qbase * (.data$Type == 1), na.rm = TRUE) * kmyr,
              Ws = sum(.data$Q * (.data$Type == 1), na.rm = TRUE) * kmyr,
              Wgrw = sum(.data$Qbase * (.data$Type == 2), na.rm = TRUE) * kmyr,
              Ww = sum(.data$Q * (.data$Type == 2), na.rm = TRUE) * kmyr,
              Qrnmax = max(.data$Qrain, na.rm = TRUE),
              Qthmax = max(.data$Qthaw, na.rm = TRUE),
              Drnmax = .data$Date[which.max(.data$Qrain)[1]],
              Dthmax = .data$Date[which.max(.data$Qthaw)[1]],
              Ts = sum(.data$Type == 1),
              Nrns = sum((.data$Type == 1) & (.data$Qrain > 0)),
              Tw = sum(.data$Type == 2),
              Nthw = sum((.data$Type == 2) & (.data$Qthaw > 0)),
              Cw = sd(.data$Q[.data$Type == 2], na.rm = TRUE) / mean(.data$Q[.data$Type == 2], na.rm = TRUE),
              Cs = sd(.data$Q[.data$Type == 1], na.rm = TRUE) / mean(.data$Q[.data$Type == 1], na.rm = TRUE),
              Nrn = sum(rle(.data$Qrain > 0)$values),
              Nth = sum(rle(.data$Qthaw > 0)$values)) %>% 
    dplyr::ungroup() %>%
    tidyr::complete(Year = tidyr::full_seq(.data$Year, period = 1)) %>%
    dplyr::mutate(Year1 = .data$Year,
                  Year2 = .data$Year+1)
}