#' Plot hydrograph separation
#'
#' @param df data.frame with hydrograph separation
#' @param years a vector of years to be plotted
#' @param layout layout matrix
#' @param pagebreak logical. Whether to break page between plots (needed for reporting). Defaults to FALSE
#' @param locale string locale. Currently only English locale is supported. Defaults to 'EN'. 
#'
#' @return ggplot2 objects
#' @export
gr_plot_sep <- function(df, years = NULL, layout = as.matrix(1), pagebreak = FALSE, locale='EN'){
  
  if (locale == 'RU') {
    Sys.setenv(LANGUAGE="ru")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "ru_RU.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "Russian"))
    
  } else {
    Sys.setenv(LANGUAGE="en")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "en_US.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "English"))
  }
  
  df = df %>% dplyr::mutate(Year = lubridate::year(Date))
  
  if(!is.null(years)){
    df = df %>% dplyr::filter(Year %in% unique(c(years, years + 1)))
  } else {
    years = df %>% dplyr::pull(Year) %>% unique() %>% order()
  }
  
  yrs = df %>% dplyr::group_by(Year) %>% 
    dplyr::summarise(nydate = Date[which(Qseas>0)[1]],
                     datepolend = max(Date[which(Qseas>0)])) %>% 
    dplyr::filter(!is.na(nydate))
  
  n = nrow(yrs)
  
  max.runoff = max(df$Q, na.rm = T)
  
  labs = get_plot_labels(locale)
  plotlist = list()
  j = 1
  
  bar = progress::progress_bar$new(total = n)
  bar$tick(0)
  
  for (i in 1:n) {
    
    if (!(yrs$Year[i] %in% years)) 
      next
    
    bar$tick()
    
    begin.date = yrs$nydate[i]
    end.date = lubridate::ceiling_date(begin.date, "year") - lubridate::days(1) # Initialize by the end of the year
    
    clipped.remark = labs$clipped.remark
    year = yrs$Year[i]
    
    datestart = begin.date
    lubridate::year(datestart) = lubridate::year(begin.date)
    
    datepolend = yrs$datepolend[i]
    lubridate::year(datepolend) = lubridate::year(begin.date)
    
    if (i != n){
      nextyear <- yrs$Year[i+1]
      if ((nextyear - year) == 1) {
        end.date = yrs$nydate[i+1] - lubridate::days(1) # Change to the end date of water-resources year
        clipped.remark = ""
      }
    }
    
    graphdata = df %>%  
      dplyr::filter(dplyr::between(Date, begin.date-1, end.date)) %>% 
      tidyr::gather(key="Runtype", value="Runoff", 
             Qthaw, Qrain, Qseas, Qbase,
             factor_key=TRUE) %>% 
      dplyr::mutate(Runoff = ifelse(Runoff < 0, 0, Runoff))
    
    graphdata$Runtype = factor(graphdata$Runtype,
                               levels = c("Qrain", "Qseas", "Qthaw", "Qbase"),
                               labels = c(labs$rain, labs$seasonal, labs$thaw, labs$ground))
    
    g = ggplot2::ggplot(graphdata, ggplot2::aes(x = Date, y = Runoff, fill = Runtype)) + 
      ggplot2::annotate("rect", 
               xmin = datestart-1, xmax = datepolend+1,
               ymax = max.runoff, ymin = 0,
               fill = 'black',
               alpha = 0.1) +
      ggplot2::geom_area(size = 0.1, color = 'black') + 
      # geom_line(aes(group = Runtype), size = 0.1, color = 'black') +
      ggplot2::geom_vline(xintercept = datestart-1, color = "black", size=0.3) +
      ggplot2::geom_vline(xintercept = datepolend+1, color = "black", size=0.3) +
      ggplot2::geom_label(data = data.frame(x = datestart, 
                                            y = 0.9 * max.runoff, 
                                            text = format(datestart, format="%d-%m")),
                          mapping = aes(x, y, label = text, hjust = 1),
                          size = 3, fill = "white", label.padding = unit(0.15, "lines")) +
      ggplot2::geom_label(data = data.frame(x = datepolend, 
                                            y = 0.9 * max.runoff, 
                                            text = format(datepolend, format="%d-%m")),
                          mapping = aes(x, y, label = text, hjust = 0),
                          size = 3, fill = "white", label.padding = unit(0.15, "lines")) +
      ggplot2::coord_cartesian(ylim=c(0, max.runoff), clip = 'off') +
      ggplot2::scale_fill_manual(values=c("coral2", "deepskyblue3", "darkturquoise", "bisque4"), 
                        name = paste0(labs$discharge.type, ':')) +
      ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      ggplot2::labs(title = year,
           subtitle = paste(begin.date, "-", end.date, clipped.remark),
           x = labs$date, y = substitute(d ~ ', ' ~ m, list(d = labs$discharge, m = labs$m3s))) +
      ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.8, face="bold"),
            legend.position="bottom")
    
    plotlist[[j]] = g
    j = j+1
    if (j == length(layout)+1) {
      multiplot(plotlist = plotlist, layout = layout)
      if (pagebreak) cat("\n\n\\pagebreak\n")
      plotlist = list()
      j = 1
    }
  }
  
  if (j > 1) {
    multiplot(plotlist = plotlist, layout = layout)
  }
}

#' Plot interannual parameters
#'
#' @param df  data.frame produced by separation function (read by `read_separation()`)
#' @param ... quoted sequence of variable names
#' @param tests tests list for the same variables (generated by `test_variables()` function)
#' @param exclude integer vector. A vector of years to be excluded from plotting
#' @param smooth logical. If true then local smoothing regression is plotted. Defaults to TRUE.
#' @param layout layout matrix
#' @param pagebreak logical. Whether to break page between plots (needed for reporting). Defaults to FALSE
#' @param locale string locale. Currently only English locale is supported. Defaults to 'EN'. 
#'
#' @return
#' @export
#'
#' @examples
gr_plot_vars <- function(df, ..., tests = NULL, exclude = NULL, smooth = TRUE, layout = as.matrix(1), pagebreak = FALSE, locale='EN'){
  
  if (locale == 'RU') {
    Sys.setenv(LANGUAGE="ru")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "ru_RU.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "Russian"))
    
  } else {
    Sys.setenv(LANGUAGE="en")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "en_US.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "English"))
  }
  
  fields = rlang::exprs(...) %>% as.character()
  
  if(length(fields) == 0)
    fields = params_out %>% 
      dplyr::filter(Order != 0) %>% 
      dplyr::arrange(Order) %>% 
      dplyr::pull(Name)
  
  prms = params_out %>% 
    dplyr::filter(Name %in% fields) %>% 
    dplyr::slice(match(fields, Name))
  
  if (is.logical(tests))
    if (tests == TRUE)
      tests = gr_test_vars(df, ...)
  
  df = df %>% 
    dplyr::mutate_if(params_out$Type == 'Date', function(X) { 
      lubridate::year(X) = 2000 
      return(X)
    }) %>%  
    dplyr::mutate_if(params_out$Winter == 1, replace_year) %>% 
    dplyr::mutate_at(dplyr::vars(-Year1, -Year2), function(X) {
      structure(ifelse(df$Year1 %in% exclude, NA, X), class = class(X))
    }) # EXCLUDE YEARS
  
  minyear = min(df$Year1)
  maxyear = max(df$Year1)
  breaks = scales::fullseq(c(minyear, maxyear), 10)
  minbreaks = scales::fullseq(c(minyear, maxyear), 5)
  
  labs = get_plot_labels(locale)
  
  units = switch(locale,
                 'RU' = prms$Units,
                 'EN' = prms$Unitsen)
  desc = switch(locale,
                'RU' = prms$Desc,
                'EN' = prms$Descen)
  
  plotlist = list()
  j = 1
  
  nn = nrow(prms)
  bar = progress::progress_bar$new(total = nn)
  bar$tick(0)
  
  for (i in 1:nn) {
    
    bar$tick()
    #Sys.sleep(0.01)
    
    # MAIN DATA FOR PLOTTING
    g = ggplot2::ggplot(df, ggplot2::aes_string(x = "Year1", y = prms$Name[i])) + 
      ggplot2::scale_x_continuous(breaks = breaks, minor_breaks = minbreaks) +
      ggplot2::labs(title = stringr::str_wrap(desc[i], width=labs$wraplength),
           x = labs$subtitle, 
           y = parse(text=units[i])) +
      ggplot2::theme(plot.title = ggplot2::element_text(size=12, lineheight=.8, face="bold"),
            panel.background = ggplot2::element_rect(fill = prms$Color[i],
                                            colour = prms$Color[i],
                                            size = 0.5, linetype = "solid"))
    if (!is.null(exclude)) {
      g = g + 
        ggplot2::labs(caption = paste('Excluded years:', paste(exclude, collapse = ', '))) +
        ggplot2::theme(plot.caption = element_text(hjust = 0))
    }
    
    if (smooth) {
      g = g + ggplot2::geom_smooth(method = 'loess', formula = y ~ x)
    }
    
    # TEST
    if (!is.null(tests)) {

      if (!is.null(tests$ptt[[i]])) { # if test for this variable was successfull

        ltype = ifelse(tests$fixed_year, 'dotted',
                       ifelse(tests$ptt[[i]]$p.value > 0.05, 'dashed', 'solid'))
        g = g +
          ggplot2::geom_vline(xintercept = tests$year[i], color = "red",
                     size=0.5, linetype = ltype) +
          ggplot2::annotate("text", label = tests$year[i],
                   x = tests$year[i] + 4, y = tests$maxval[[i]],
                   size = 4, colour = "red")
      }

      if(!is.null(tests$tst[[i]]) && !is.null(tests$ts_fit[[i]])) {
        ts_ltype = ifelse(mblm::summary.mblm(tests$ts_fit[[i]])$coefficients[2, 4] > 0.05, 'dashed', 'solid')
        
        g = g + ggplot2::geom_abline(intercept = coef(tests$ts_fit[[i]])[1], slope = coef(tests$ts_fit[[i]])[2],
                            color = 'red', size=1, linetype = ts_ltype)
      }

      fixedstr = ifelse(tests$fixed_year, '',
                        paste0(labs$pettitt.u, ' = ', ifelse(is.null(tests$ptt[[i]]), 'NA',
                                                             round(tests$ptt[[i]]$statistic, 3)), ', ',
                               labs$label.p, ' = ', ifelse(is.null(tests$ptt[[i]]), 'NA',
                                                           round(tests$ptt[[i]]$p.value, 5))))

      g = g + #labs(subtitle = as.expression(bquote(bold(.(labs$kendall.z))~'=')))
        labs(subtitle = paste0(labs$kendall.z, ' = ', ifelse(is.null(tests$mkt[[i]]), 'NA',
                                                             round(tests$mkt[[i]]$statistic, 3)), ', ',
                               labs$label.p, ' = ', ifelse(is.null(tests$mkt[[i]]), 'NA',
                                                           round(tests$mkt[[i]]$p.value, 5)), '\n',
                               labs$theil.i, ' = ', ifelse(is.null(tests$ts_fit[[i]]), 'NA',
                                                           round(coef(tests$ts_fit[[i]])[2], 5)), ', ',
                               labs$label.p, ' = ', ifelse(is.null(tests$ts_fit[[i]]), 'NA',
                                                           round(mblm::summary.mblm(tests$ts_fit[[i]])$coefficients[2, 4], 5)), '. ',
                               fixedstr))
    }
    
    date_labels = "%d-%b"

    # PLOT TYPE
    if (prms$Chart[i] == 'line') {
      g = g + ggplot2::geom_line() + ggplot2::geom_ribbon(ggplot2::aes_string(ymin = 0, ymax = prms$Name[i]), alpha = 0.3)
    } else if (prms$Chart[i] == 'point') {
      g = g + ggplot2::geom_point(size = 1.5) + ggplot2::geom_line(size = 0.2)
      date_labels = "%b"
    } else if (prms$Chart[i] == 'plate') {
      g = g + ggplot2::geom_point(shape = 15, size = 1.5) + ggplot2::geom_step(size = 0.2)
      date_labels = "%b"
    } else if (prms$Chart[i] == 'step') {
      g = g + ggplot2::geom_step() + ggplot2::geom_rect(ggplot2::aes_string(xmin = "Year1", 
                                                 xmax = "Year2", 
                                                 ymax = prms$Name[i], 
                                                 ymin = 0), 
                                      alpha = 0.4) +
        ggplot2::scale_y_continuous(limits = c(0, NA))
      date_labels = "%m"
    }
    
    # SPECIAL AXES FOR DATES
    if(prms$Unitsen[i] %in% c('Date', 'Month')) {
      if(prms$Winter[i] != 1) {
        g = g +
          ggplot2::scale_y_date(date_labels = date_labels,
                       date_breaks = "2 month",
                       limits = c(lubridate::ymd(20000101), lubridate::ymd(20001231))) +
          ggplot2::expand_limits(y = c(lubridate::ymd(20000101), lubridate::ymd(20001231)))
      } else {
        g = g +
          ggplot2::scale_y_date(date_labels = date_labels,
                       date_breaks = "2 month",
                       limits = c(lubridate::ymd(20000701), lubridate::ymd(20010630))) +
          ggplot2::expand_limits(y = c(lubridate::ymd(20000701), lubridate::ymd(20010630)))
      }
    }
    
    # g = g + labs(subtitle = prms$Unitsen[i])
    
    plotlist[[j]] = g
    j = j+1
    if (j == length(layout)+1) {
      multiplot(plotlist = plotlist, layout = layout)
      if (pagebreak) cat("\n\n\\pagebreak\n")
      plotlist = list()
      j = 1
    }
  }
  
  if (j > 1) {
    multiplot(plotlist = plotlist, layout = layout)
  }
}

#' Plot long-term changes of hydrograph variables for two periods
#'
#' @param df data.frame produced by description function (read by `read_variables()`)
#' @param ... quoted sequence of variable names
#' @param year change year value to separate two periods (overridden by tests if it is supplied)
#' @param exclude integer vector. A vector of years to be excluded from plotting
#' @param tests tests list for the same variables (generated by `test_variables()` function)
#' @param layout layout matrix
#' @param pagebreak logical. Whether to break page between plots (needed for reporting). Defaults to FALSE
#' @param locale string locale. Currently only English locale is supported. Defaults to 'EN'. 
#'
#' @return ggplot2 objects
#' @export
gr_plot_periods <- function(df, ..., year = NULL, exclude = NULL, tests = NULL, layout = as.matrix(1), pagebreak = FALSE, locale='EN'){
  
  if(is.null(year) & is.null(tests))
    stop('You must provide year or tests parameter')
  
  if (locale == 'RU') {
    Sys.setenv(LANGUAGE="ru")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "ru_RU.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "Russian"))
    
  } else {
    Sys.setenv(LANGUAGE="en")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "en_US.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "English"))
  }
  
  fields = rlang::exprs(...) %>% as.character()

  if(length(fields) == 0)
    fields = params_out %>% 
    dplyr::filter(Order != 0) %>% 
    dplyr::arrange(Order) %>% 
    dplyr::pull(Name)
  
  prms = params_out %>% 
    dplyr::filter(Name %in% fields) %>% 
    dplyr::slice(match(fields, Name))
  
  if (is.logical(tests))
    if (tests == TRUE)
      tests = gr_test_vars(df, ...)
  
  df = df %>% 
    dplyr::filter(!(Year1 %in% exclude)) %>% 
    dplyr::mutate_if(params_out$Winter == 1, replace_year)
  
  labs = get_plot_labels(locale)
  
  units = switch(locale,
                 'RU' = prms$Units,
                 'EN' = prms$Unitsen)
  desc = switch(locale,
                'RU' = prms$Desc,
                'EN' = prms$Descen)
  
  plotlist = list()
  j = 1
  
  nn = nrow(prms)
  bar = progress::progress_bar$new(total = nn)
  bar$tick(0)
  
  for (i in 1:nn) {
    
    bar$tick()
    #Sys.sleep(0.01)
    
    if(!is.null(tests)){
      year = tests$year[i]
    }
    
    d = df[prms$Name[i]] %>% 
      as.matrix() %>% 
      as.vector()
    
    is_date = FALSE
    if(prms$Unitsen[i] %in% c('Date', 'Month')){
      
      d = d %>% as.Date() 
      year(d) <- 2000 # fake year
      
      if (prms$Winter[i] == 1)
        d = replace_year(d)
      
      d = as.integer(d)
      
      is_date = TRUE
    }
    
    d1 = d[df$Year1 < year]
    d2 = d[df$Year1 >= year]
    
    n1 = length(d1)
    n2 = length(d2)
    
    periodtitle1 = paste0(labs$beforetitle, year)
    periodtitle2 = paste0(labs$aftertitle, year)
    
    df.plot = data.frame(Value = d, 
                         Period = c(rep(periodtitle1, n1), 
                                   rep(periodtitle2, n2)))
    
    g = ggplot2::ggplot() + 
      ggplot2::geom_boxplot(data = df.plot, ggplot2::aes(x = Period, y = Value)) +
      ggplot2::coord_flip() +
      ggplot2::labs(title = stringr::str_wrap(desc[i], width = labs$wraplength),
           x = NULL, 
           y = parse(text = units[i])) +
      ggplot2::theme(plot.title = ggplot2::element_text(size=12, lineheight=.8, face="bold"),
            panel.background = ggplot2::element_rect(fill = prms$Color[i],
                                            colour = prms$Color[i],
                                            size = 0.5, linetype = "solid"))
    if (!is.null(exclude)) {
      g = g + 
        ggplot2::labs(caption = paste('Excluded years:', paste(exclude, collapse = ', '))) +
        ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0))
    }
    
    if (!is.null(tests)) {
      
      m1 = mean(d1, na.rm = TRUE)
      m2 = mean(d2, na.rm = TRUE)
      
      rsd1 = round(sd(d1, na.rm = TRUE)/m1, 3)
      rsd2 = round(sd(d2, na.rm = TRUE)/m2, 3)
      
      means <- df.plot %>% 
        dplyr::group_by(Period) %>% 
        dplyr::summarise(Value = mean(Value, na.rm = TRUE))
      
      mean1 = ifelse(is_date, 
                     m1 %>% as.integer() %>% as.Date(origin = '1970-01-01') %>% format("%d-%b"),
                     m1 %>% round(3)
      )
      
      mean2 = ifelse(is_date, 
                     m2 %>% as.integer() %>% as.Date(origin = '1970-01-01') %>% format("%d-%b"),
                     m2 %>% round(3)
      )
      
      m1 = ifelse(is_date,
                  m1 %>% as.integer(),
                  m1)
      
      m2 = ifelse(is_date,
                  m2 %>% as.integer(),
                  m2)
      
      pt.df = data.frame(Value = c(m1, m2), 
                         Period = c(periodtitle1, periodtitle2))
      
      g = g + 
        ggplot2::geom_point(data = pt.df, ggplot2::aes(x = Period, y = Value), colour="steelblue", shape=20, size=3) +
        ggplot2::labs(subtitle = paste0(labs$student.t, ' = ', ifelse(is.null(tests$tt[[i]]), 'NA',
                                                             round(tests$tt[[i]]$statistic, 3)), ', ',
                               labs$label.p, ' = ', ifelse(is.null(tests$tt[[i]]), 'NA', 
                                                           round(tests$tt[[i]]$p.value, 5)), ', ',
                               'm1 = ', mean1, ', ', 
                               'm2 = ', mean2,
                               '\n',
                               labs$fisher.f, ' = ', ifelse(is.null(tests$ft[[i]]), 'NA', 
                                                            round(tests$ft[[i]]$statistic, 3)), ', ',
                               labs$label.p, ' = ', ifelse(is.null(tests$ft[[i]]), 'NA',
                                                           round(tests$ft[[i]]$p.value, 5)), ', ',
                               'cv1 = ', rsd1, ', ',
                               'cv2 = ', rsd2))
    }
    
    if(is_date){
      g = g + ggplot2::scale_y_continuous(labels = function(x) {
        return(as.Date(x, origin = "1970-01-01") %>% format(format = '%d-%m'))
      })
    }
    
    plotlist[[j]] = g
    j = j+1
    if (j == length(layout)+1) {
      multiplot(plotlist = plotlist, layout = layout)
      if (pagebreak) cat("\n\n\\pagebreak\n")
      plotlist = list()
      j = 1
    }
  }
  
  if (j > 1) {
    multiplot(plotlist = plotlist, layout = layout)
  }
  
}

#' Plot histogram of minimum discharge month for two periods
#'
#' @param df data.frame produced by description function (read by `read_variables()`)
#' @param year change year value to separate two periods
#' @param exclude integer vector. A vector of years to be excluded from plotting
#' @param locale string locale. Currently only English locale is supported. Defaults to 'EN'. 
#'
#' @return ggplot2 objects
#' @export
gr_plot_minmonth <- function(df, year = NULL, exclude = NULL, tests = NULL, pagebreak = FALSE, locale='EN'){
  
  year_summer = year
  year_winter = year
  
  if(is.null(year)) {
    if (!is.null(tests)) {
      year_summer = tests[['year']]['monmmsummer']
      year_winter = tests[['year']]['nommwin']
    }
    else stop('You must supply either year or tests parameter')
  }
  
  if (locale == 'RU') {
    Sys.setenv(LANGUAGE="ru")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "ru_RU.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "Russian"))
    
  } else {
    Sys.setenv(LANGUAGE="en")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "en_US.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "English"))
  }
  
  labs = get_plot_labels(locale)
  
  periodtitle1_summer = paste0(labs$beforetitle, year_summer)
  periodtitle2_summer = paste0(labs$aftertitle, year_summer)
  
  periodtitle1_winter = paste0(labs$beforetitle, year_winter)
  periodtitle2_winter = paste0(labs$aftertitle, year_winter)
  
  chart.data = df %>% 
    dplyr::filter(!(Year1 %in% exclude)) %>% 
    dplyr::select(monmmsummer, nommwin, Year1) %>% 
    dplyr::filter(!is.na(monmmsummer) & !is.na(nommwin)) %>% 
    dplyr::mutate(summermonth = lubridate::month(monmmsummer),
                  wintermonth = lubridate::month(nommwin),
                  old_summer = as.integer(Year1 >= year_summer),
                  old_winter = as.integer(Year1 >= year_winter))
  
  chart.data$old_summer = factor(chart.data$old_summer, 
                          levels = c(0,1), 
                          labels = c(periodtitle1_summer, periodtitle2_summer))
  
  chart.data$old_winter = factor(chart.data$old_winter, 
                                 levels = c(0,1), 
                                 labels = c(periodtitle1_winter, periodtitle2_winter))
  
  month.names = format(ISOdate(2017,1:12,1),"%m")
  winterlabels = month.names[c(7:12, 1:6)]
  
  chart.data$summermonth = ordered(chart.data$summermonth, 
                                   levels = 1:12, 
                                   labels = month.names)
  
  chart.data$wintermonth = ordered(chart.data$wintermonth, 
                                   levels = c(7:12, 1:6), 
                                   labels = winterlabels)
  
  df.summer = chart.data %>% 
    dplyr::group_by(old_summer, summermonth) %>% 
    dplyr::tally() %>% 
    dplyr::ungroup() %>%
    tidyr::complete(summermonth, tidyr::nesting(old_summer), fill=list(n=0)) %>%  
    dplyr::mutate(perc = 100*n/sum(n))
  
  df.winter = chart.data %>% 
    dplyr::group_by(old_winter, wintermonth) %>% 
    dplyr::tally() %>% 
    dplyr::ungroup() %>%
    tidyr::complete(wintermonth, tidyr::nesting(old_winter), fill=list(n=0)) %>%  
    dplyr::mutate(perc = 100*n/sum(n))
  
  df.summer.all = chart.data %>% 
    dplyr::group_by(summermonth) %>% 
    dplyr::tally() %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/sum(n))
  
  df.winter.all = chart.data %>% 
    dplyr::group_by(wintermonth) %>% 
    dplyr::tally() %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(perc = 100*n/sum(n))
  
  g.summer = ggplot2::ggplot() +
    ggplot2::geom_col(data = df.summer, 
                      ggplot2::aes(x = summermonth, y = perc, fill = old_summer), 
             position = "dodge") +
    ggplot2::geom_col(data = df.summer.all, 
                      ggplot2::aes(x = summermonth, y = perc), 
             colour = "black", 
             fill = NA, 
             size = 1) +
    ggplot2::theme(plot.title = ggplot2::element_text(face="bold")) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::scale_fill_manual(values=c("indianred1", "deepskyblue4"), 
                      name = labs$periodtitle) +
    ggplot2::labs(title = labs$bartitle.sum,
         x = labs$monthtitle, 
         y = "%")
  
  if (!is.null(exclude)) {
    g.summer = g.summer + 
      ggplot2::labs(caption = paste('Excluded years: ', paste(exclude, collapse = ', '))) +
      ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0))
  }
  
  g.winter = ggplot2::ggplot() +
    ggplot2::geom_col(data = df.winter, 
                      ggplot2::aes(x = wintermonth, y = perc, fill = old_winter), 
             position = "dodge") +
    ggplot2::geom_col(data = df.winter.all, 
                      ggplot2::aes(x = wintermonth, y = perc), 
             colour="black", 
             fill = NA, 
             size = 1) +
    ggplot2::theme(plot.title = ggplot2::element_text(face="bold")) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::scale_fill_manual(values=c("indianred1", "deepskyblue4"), 
                      name = labs$periodtitle) +
    ggplot2::labs(title = labs$bartitle.win,
         x = labs$monthtitle, 
         y = "%")
  
  if (!is.null(exclude)) {
    g.winter = g.winter + 
      ggplot2::labs(caption = paste('Excluded years:', paste(exclude, collapse = ', '))) +
      ggplot2::theme(plot.caption = element_text(hjust = 0))
  }
  
  multiplot(plotlist = list(g.summer, g.winter))
  if (pagebreak) cat("\n\n\\pagebreak\n")
}


#' Plot change year density
#'
#' @param tests result of `test_variables()`
#' @param locale 
#'
#' @return plots density plot for change year
#' @export
#'
#' @examples
gr_plot_tests <- function(tests, locale = 'EN') {
  
  labs = get_plot_labels(locale)
  
  years = tests$year[!is.na(tests$year)]
  dens = density(years, from = min(years), to = max(years), n = max(years) - min(years) + 1)
  ddf = tibble::tibble(year = dens$x, dens = dens$y)
  
  modeval = as.integer(dens$x[which.max(dens$y)])
  
  ggplot2::ggplot() +
    # geom_density(data = tibble::tibble(year = tests$year), 
    #              aes(x = year), color = 'black', fill = 'gray75', alpha = 0.5) +       
    ggplot2::geom_line(data = ddf, ggplot2::aes(x = year, y = dens), color = 'black') +
    ggplot2::geom_area(data = ddf, ggplot2::aes(x = year, y = dens), alpha = 0.2) +
    ggplot2::geom_vline(xintercept = modeval, color = "steelblue4", size = 1) +
    ggplot2::annotate("text", label = modeval, 
             x = modeval + 0.05 * (max(dens$x) - min(dens$x)), y = 0.01, 
             size = 5, colour = "steelblue4") +
    ggplot2::labs(title = labs$year.density,
         x = labs$subtitle,
         y = NULL) +
    ggplot2::theme_minimal()
  
}

#' Plot the autocorrelation function (ACF) for hydrograph time series
#'
#' @param hdata a data frame with two columns: date and discharge
#' @param autocorr 
#'
#' @return ACF plot
#' @export
#'
#' @examples
gr_plot_acf <- function(hdata, autocorr = 0.7, max_lag = 30) {
  
  max_period = hdata %>% 
    gr_get_gaps() %>% 
    dplyr::filter(type == 'data', duration == max(duration))
  
  acf_data = hdata %>% 
    dplyr::filter(between(.[[1]], max_period$start_date, max_period$end_date)) %>% 
    pull(2)
  
  afun = acf(acf_data, lag.max = max_lag, plot = FALSE)
    
  tab = tibble::tibble(Days = seq_along(afun$acf), ACF = afun$acf)
  
  days = purrr::detect_index(afun$acf, ~ .x < autocorr) - 1
  
  ggplot2::ggplot(tab, ggplot2::aes(Days, ACF)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(aes(yintercept = autocorr), 
               color = 'red') +
    ggplot2::annotate("text", label = autocorr,
             x = 0, y = autocorr + 0.05,
             size = 4, colour = "red") +
    ggplot2::geom_vline(aes(xintercept = days), 
                   color = 'blue', size = 1) +
    ggplot2::annotate("text", label = days,
             x = days + 1, y = 1,
             size = 4, colour = "blue") +
    ggplot2::labs(title = 'Autocorrelation function (ACF)') +
    ggplot2::theme(plot.title = ggplot2::element_text(size=12, lineheight=.8, face="bold"))
}

#' Animate discharge through years
#'
#' @param df data.framewith discarge values in Q variable
#' @param locale 
#'
#' @return
#' @export
#'
#' @examples
gr_animate <- function(df, locale = 'EN') {
  tab = df %>% 
    mutate(Date = lubridate::make_date(Year, Month, Day),
           yDate = Date)
  
  year(tab$yDate) <- 2000 # fake year for animations
  
  anim = ggplot2::ggplot(tab, mapping = ggplot2::aes(x = yDate, y = Q)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = 0, ymax = Q), alpha = 0.5) +
    ggplot2::geom_line() +
    ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    ggplot2::labs(title = "Discharge animation",
                  subtitle = 'Year: {closest_state}') +
    ggplot2::xlab('Date') +
    ggplot2::ylab('m3/s') +
    ggplot2::theme(text = ggplot2::element_text(size = 18, family = 'Open Sans')) +
    gganimate::transition_states(Year, state_length = 0) +
    gganimate::view_follow(fixed_y = TRUE)
  
  gganimate::animate(anim, 
                     fps = 20,                                  
                     nframes = 10 * length(unique(tab$Year)),
                     width = 800, 
                     height = 600)
}