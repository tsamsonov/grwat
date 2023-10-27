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
#' @param year_min `integer` first year to summarise 
#' @param year_max `integer` last year to summarise 
#'
#' @return `data.frame` with one row for each water-resources year and multiple columns of statistics explained by [grwat::gr_help_vars()].
#' @export
#'
#' @example inst/examples/gr_summarize.R
#' 
gr_summarize <- function(df, year_min = NULL, year_max = NULL) {
  
  secday = 86400
  kmyr = secday / 10e9
  
  if (nrow(df) == 0)
    stop(cli::col_white(cli::bg_red(cli::style_bold('grwat:'))), 
         ' no data to summarise')
  
  if (!is.null(year_min))
    df = dplyr::filter(df, Year >= year_min)
  
  if (!is.null(year_max))
    df = dplyr::filter(df, Year <= year_max)
  
  if (nrow(df) == 0)
    stop(cli::col_white(cli::bg_red(cli::style_bold('grwat:'))), 
         ' filtering by year resulted in empty data frame')
  
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
              Qsmin = min(.data$Q[.data$Season == 1], na.rm = TRUE),
              Dsmin = .data$Date[which.min(.data$Q / (.data$Season == 1))],
              Qwmin = min(.data$Q[.data$Season == 2], na.rm = TRUE),
              Dwmin = .data$Date[which.min(.data$Q / (.data$Season == 2))],
              Q30s = condrollmean(.data$Q, .data$Season == 1, 30),
              D30s1 = .data$Date[condrollmeanidx(.data$Q, .data$Season == 1, 30)],
              D30s2 = .data$D30s1 + 29,
              Q30w = condrollmean(.data$Q, .data$Season == 2, 30),
              D30w1 = .data$Date[condrollmeanidx(.data$Q, .data$Season == 2, 30)],
              D30w2 = .data$D30w1 + 29,
              Q10s = condrollmean(.data$Q, .data$Season == 1, 10),
              D10s1 = .data$Date[condrollmeanidx(.data$Q, .data$Season == 1, 10)],
              D10s2 = .data$D10s1 + 9,
              Q10w = condrollmean(.data$Q, .data$Season == 2, 10),
              D10w1 = .data$Date[condrollmeanidx(.data$Q, .data$Season == 2, 10)],
              D10w2 = .data$D10w1 + 9,
              Q5s = condrollmean(.data$Q, .data$Season == 1, 5),
              D5s1 = .data$Date[condrollmeanidx(.data$Q, .data$Season == 1, 5)],
              D5s2 = .data$D5s1 + 4,
              Q5w = condrollmean(.data$Q, .data$Season == 2, 5),
              D5w1 = .data$Date[condrollmeanidx(.data$Q, .data$Season == 2, 5)],
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
              Wgrs = sum(.data$Qbase * (.data$Season == 1), na.rm = TRUE) * kmyr,
              Ws = sum(.data$Q * (.data$Season == 1), na.rm = TRUE) * kmyr,
              Wgrw = sum(.data$Qbase * (.data$Season == 2), na.rm = TRUE) * kmyr,
              Ww = sum(.data$Q * (.data$Season == 2), na.rm = TRUE) * kmyr,
              Qrnmax = max(.data$Qrain, na.rm = TRUE),
              Qthmax = max(.data$Qthaw, na.rm = TRUE),
              Drnmax = .data$Date[which.max(.data$Qrain)[1]],
              Dthmax = .data$Date[which.max(.data$Qthaw)[1]],
              Ts = sum(.data$Season == 1),
              Nrns = sum((.data$Season == 1) & (.data$Qrain > 0)),
              Tw = sum(.data$Season == 2),
              Nthw = sum((.data$Season == 2) & (.data$Qthaw > 0)),
              Cw = sd(.data$Q[.data$Season == 2], na.rm = TRUE) / mean(.data$Q[.data$Season == 2], na.rm = TRUE),
              Cs = sd(.data$Q[.data$Season == 1], na.rm = TRUE) / mean(.data$Q[.data$Season == 1], na.rm = TRUE),
              Nrn = sum(rle(.data$Qrain > 0)$values),
              Nth = sum(rle(.data$Qthaw > 0)$values)) %>% 
    dplyr::ungroup() %>%
    tidyr::complete(Year = tidyr::full_seq(.data$Year, period = 1)) %>%
    dplyr::mutate(Year1 = .data$Year,
                  Year2 = .data$Year+1)
}