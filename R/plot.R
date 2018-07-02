get_plot_labels <- function(locale = 'EN'){
  switch(locale,
     'EN' = list(
       subtitle = 'Water-resources year',
       bartitle.win = "Month of a minimum monthly discharge during winter",
       bartitle.sum = "Month of a minimum monthly discharge during summer",
       monthtitle = "Month",
       periodtitle = "Period",
       beforetitle = "before ",
       aftertitle = "after ",
       label.p = 'p',
       student.t = 'Student (mean): t',
       fisher.f = 'Fisher (variance): F',
       pettitt.u = 'Pettitt (change-point): U*',
       kendall.z = 'Mann-Kendall (trend): z',
       theil.i = 'Theil-Sen (trend): i',
       student.df = 'df',
       clipped.remark = "(clipped by the end of the year)",
       discharge = "Total discharge",
       date = "Date",
       m3s = bquote(m^3/s),
       wraplength = 60 # row wrap length for long titles
     ),
     'RU' = list(
       subtitle = 'Начало водохозяйственного года',
       bartitle.win = "Месяц минимального месячного расхода за зиму",
       bartitle.sum = "Месяц минимального месячного расхода за лето",
       monthtitle = "Месяц",
       periodtitle = "Период",
       beforetitle = "до ",
       aftertitle = "с ",
       label.p = 'p',
       student.t = 'Стьюдент (средние): t',
       fisher.f = 'Фишер (дисперсии): F',
       pettitt.u = 'Петтитт (перелом): U*',
       kendall.z = 'Манн-Кендалл (тренд): z',
       theil.i = 'Тейл-Сен (тренд): i',
       student.df = 'df',
       clipped.remark = "(обрезано по концу календарного года)",
       discharge = "Суммарный расход",
       date = "Дата",
       m3s = bquote(м^3/с),
       wraplength = 50 # row wrap length for long titles
     )
  )
}

#' Plot hydrograph separation
#'
#' @param df 
#' @param locale 
#'
#' @return
#' @export
plot_separation <- function(df, yrs = NULL, layout = as.matrix(1), pagebreak = FALSE, locale='EN'){
  
  if (locale == 'RU') {
    Sys.setenv(LANGUAGE="ru")
    Sys.setlocale("LC_ALL", "RU")
  } else {
    Sys.setenv(LANGUAGE="en")
    Sys.setlocale("LC_ALL", "en_US.UTF-8")
  }
  
  df = df %>% dplyr::mutate(Year = lubridate::year(Date))
  
  if(!is.null(yrs)){
    df = df %>% dplyr::filter(Year %in% yrs)
  }
  
  years = df %>% dplyr::group_by(Year) %>% 
    dplyr::summarise(nydate = Date[which(Qpol>0)[1]],
                     datepolend = max(Date[which(Qpol>0)])) %>% 
    dplyr::filter(!is.na(nydate))
  
  n = nrow(years)
  
  max.runoff = max(df$Qin)
  
  labs = get_plot_labels(locale)
  plotlist = list()
  j = 1
  
  for (i in 1:n) {
    begin.date = years$nydate[i]
    end.date = lubridate::ceiling_date(begin.date, "year") - lubridate::days(1) # Initialize by the end of the year
    
    clipped.remark = labs$clipped.remark
    year = years$Year[i]
    
    datestart = begin.date
    lubridate::year(datestart) = lubridate::year(begin.date)
    
    datepolend = years$datepolend[i]
    lubridate::year(datepolend) = lubridate::year(begin.date)
    
    if (i != n){
      nextyear <- years$Year[i+1]
      if ((nextyear - year) == 1) {
        end.date = years$nydate[i+1] - lubridate::days(1) # Change to the end date of water-resources year
        clipped.remark = ""
      }
    }
    
    graphdata = df %>%  
      dplyr::filter(dplyr::between(Date, begin.date, end.date)) %>% 
      tidyr::gather(key="Runtype", value="Runoff", 
             Qthaw, Qpav, Qpol, Qgr,
             factor_key=TRUE)
    
    graphdata$Runtype = factor(graphdata$Runtype,
                               levels = c("Qthaw", "Qpav", "Qpol", "Qgr"),
                               labels = c("Thaw", "Rain", "Seasonal", "Ground"))
    
    g = ggplot(graphdata, aes(x=Date, y=Runoff, fill=Runtype)) + 
      annotate("rect", 
               xmin = datestart, xmax = datepolend,
               ymax = max.runoff, ymin = 0,
               fill = 'black',
               alpha = 0.1) +
      geom_area() + 
      geom_vline(xintercept = datestart, color = "black", size=0.3) +
      geom_vline(xintercept = datepolend, color = "black", size=0.3) +
      annotate("text", label = format(datestart, format="%d-%m"),
               x = datestart + 20, y = 0.95 * max.runoff,
               size = 3, colour = "black") +
      annotate("text", label = format(datepolend, format="%d-%m"), 
               x = datepolend + 20, y = 0.95 * max.runoff, 
               size = 3, colour = "black") +
      coord_cartesian(ylim=c(0, max.runoff)) +
      scale_fill_manual(values=c("blue", "darkgreen", "steelblue", "violetred"), 
                        name = "Discharge:") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      labs(title = year,
           subtitle = paste(begin.date, "—", end.date, clipped.remark),
           x = labs$date, y = substitute(d ~ ', ' ~ m, list(d = labs$discharge, m = labs$m3s))) +
      theme(plot.title = element_text(lineheight=.8, face="bold"),
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
#' @param df  data.frame produced by separation function
#' @param tests 
#' @param locale 
#'
#' @return
#' @export
#'
#' @examples
plot_parameters <- function(df, ..., tests = NULL, layout = as.matrix(1), pagebreak = FALSE, locale='EN'){
  
  if (locale == 'RU') {
    Sys.setenv(LANGUAGE="ru")
    Sys.setlocale("LC_ALL", "Russian")
  } else {
    Sys.setenv(LANGUAGE="en")
    Sys.setlocale("LC_ALL", "English")
  }
  
  df = df %>% 
    dplyr::mutate_if(params_out$Winter == 1, replace_year)
  
  fields = ifelse(length(...) > 0,
                  as.character(rlang::exprs(...)),
                  params_out %>% 
                    dplyr::filter(Order != 0) %>% 
                    dplyr::arrange(Order) %>% 
                    dplyr::select(Name) %>% 
                    as.matrix() %>% 
                    as.vector())
  
  prms = params_out %>% 
            dplyr::filter(Name %in% fields) %>% 
            dplyr::slice(match(Name, fields))
  
  if(is.null(tests)){
    tests = do.call(grwat::run_tests, 
                    c(list(df), lapply(fields, as.name)))
  }
  
  nn = length(prms)
  
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
  
  for (i in 1:nn) {
    
    ltype="solid"
    if(tests$ptt[[i]]$p.value > 0.05) ltype = 'dashed'
    
    ts_ltype="solid"
    if(tests$tst[[i]]$p.value > 0.05) ts_ltype = 'dashed'
    
    # MAIN DATA FOR PLOTTING
    g = ggplot(df, aes_string(x = "Year1", y = prms$Name[i])) + 
      geom_smooth() +
      geom_abline(intercept = coef(tests$ts_fit[[i]])[1], slope = coef(tests$ts_fit[[i]])[2], 
                  color = 'red', size=1, linetype = ts_ltype) +
      geom_vline(xintercept = tests$change_year[i], color = "red", 
                 size=0.5, linetype = ltype) +
      annotate("text", label = tests$change_year[i], 
               x = tests$change_year[i] + 4, y = tests$maxval[[i]], 
               size = 4, colour = "red") +
      scale_x_continuous(breaks = breaks, minor_breaks = minbreaks) +
      labs(title = stringr::str_wrap(desc[i], width=labs$wraplength),
           subtitle = paste0(labs$pettitt.u, ' = ',  round(tests$ptt[[i]]$statistic, 3), ', ',
                             labs$label.p, ' = ', round(tests$ptt[[i]]$p.value, 5), '\n',
                             labs$kendall.z, ' = ', round(tests$mkt[[i]]$statistic, 3), ', ',
                             labs$label.p, ' = ', round(tests$mkt[[i]]$p.value, 5), '. ',
                             labs$theil.i, ' = ', round(tests$coef(ts_fit[[i]])[2], 5)),
           x = subtitle, 
           y = parse(text=units[i])) +
      theme(plot.title = element_text(size=12, lineheight=.8, face="bold"),
            panel.background = element_rect(fill = prms$Color[i],
                                            colour = prms$Color[i],
                                            size = 0.5, linetype = "solid"))
    
    date_labels = "%d-%b"
    
    # PLOT TYPE
    if (prms$Chart[i] == 'line') {
      g = g + geom_line() + geom_area(alpha = 0.3)
    } else if (prms$Chart[i] == 'point') {
      g = g + geom_point(size = 1.5) + geom_line(size = 0.2)
      date_labels = "%b"
    } else if (prms$Chart[i] == 'plate') {
      g = g + geom_point(shape = 15, size = 1.5) + geom_step(size = 0.2)
      date_labels = "%b"
    } else if (prms$Chart[i] == 'step') {
      g = g + geom_step() + geom_rect(aes_string(xmin = "Year1", 
                                                 xmax = "Year2", 
                                                 ymax = prms$Name[i], 
                                                 ymin = 0), 
                                      alpha = 0.4) +
        scale_y_continuous(limits = c(0, NA))
      date_labels = "%m"
    }
    
    # SPECIAL AXES FOR DATES
    if(prms$Units[i] %in% c('Date', 'Month')){
      if(prms$Winter[i] != 1) {
        g = g +
          scale_y_date(date_labels = date_labels, 
                       date_breaks = "2 month", 
                       limits = c(lubridate::ymd(20000101), lubridate::ymd(20001231))) +
          expand_limits(y = c(lubridate::ymd(20000101), lubridate::ymd(20001231)))
      } else {
        g = g +
          scale_y_date(date_labels = date_labels, 
                       date_breaks = "2 month", 
                       limits = c(lubridate::ymd(20000701), lubridate::ymd(20010630))) +
          expand_limits(y = c(lubridate::ymd(20000701), lubridate::ymd(20010630)))
      }
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
#' @param df 
#' @param locale 
#'
#' @return
#' @export
#'
#' @examples
plot_minmonth <- function(df, locale='EN'){
  
  if (locale == 'RU') {
    Sys.setenv(LANGUAGE="ru")
    Sys.setlocale("LC_ALL", "Russian")
  } else {
    Sys.setenv(LANGUAGE="en")
    Sys.setlocale("LC_ALL", "English")
  }
  
  year = params$year
  periodtitle1 = paste0(beforetitle, year)
  periodtitle2 = paste0(aftertitle, year)
  
  chart.data = df %>% 
    select(monmmsummer, nommwin, Year1) %>% 
    filter(!is.na(monmmsummer) & !is.na(nommwin)) %>% 
    mutate(summermonth = month(monmmsummer),
           wintermonth = month(nommwin),
           old = as.integer(Year1 >= year))
  
  chart.data$old = factor(chart.data$old, 
                          levels = c(0,1), 
                          labels = c(periodtitle1, periodtitle2))
  
  month.names = format(ISOdate(2017,1:12,1),"%m")
  winterlabels = month.names[c(7:12, 1:6)]
  
  chart.data$summermonth = ordered(chart.data$summermonth, 
                                   levels = 1:12, 
                                   labels = month.names)
  
  chart.data$wintermonth = ordered(chart.data$wintermonth, 
                                   levels = c(7:12, 1:6), 
                                   labels = winterlabels)
  
  df.summer = chart.data %>% 
    group_by(old, summermonth) %>% 
    tally() %>% 
    complete(summermonth, nesting(old), fill=list(n=0)) %>%  
    mutate(perc = 100*n/sum(n))
  
  df.winter = chart.data %>% 
    group_by(old, wintermonth) %>% 
    tally() %>% 
    complete(wintermonth, nesting(old), fill=list(n=0)) %>%  
    mutate(perc = 100*n/sum(n))
  
  df.summer.all = chart.data %>% 
    group_by(summermonth) %>% 
    tally() %>% 
    mutate(perc = 100*n/sum(n))
  
  df.winter.all = chart.data %>% 
    group_by(wintermonth) %>% 
    tally() %>% 
    mutate(perc = 100*n/sum(n))
  
  labs = grwat::get_plot_labels(locale)
  g.summer = ggplot() +
    geom_col(data = df.summer, 
             aes(x = summermonth, y = perc, fill = old), 
             position = "dodge") +
    geom_col(data = df.summer.all, 
             aes(x = summermonth, y = perc), 
             colour="black", 
             fill = NA, 
             size=1) +
    theme(plot.title = element_text(face="bold")) +
    scale_x_discrete(drop = FALSE) +
    scale_fill_manual(values=c("indianred1", "deepskyblue4"), 
                      name = labs$periodtitle) +
    labs(title = labs$bartitle.sum,
         x = labs$monthtitle, 
         y = "%")
  
  g.winter = ggplot() +
    geom_col(data = df.winter, 
             aes(x = wintermonth, y = perc, fill = old), 
             position = "dodge") +
    geom_col(data = df.winter.all, 
             aes(x = wintermonth, y = perc), 
             colour="black", 
             fill = NA, 
             size=1) +
    theme(plot.title = element_text(face="bold")) +
    scale_x_discrete(drop = FALSE) +
    scale_fill_manual(values=c("indianred1", "deepskyblue4"), 
                      name = labs$periodtitle) +
    labs(title = labs$bartitle.win,
         x = labs$monthtitle, 
         y = "%")
  multiplot(plotlist = list(g.summer, g.winter))
}

#' Plot long-term characteristics for two periods
#'
#' @param df 
#' @param year 
#' @param locale 
#'
#' @return
#' @export
#'
#' @examples
plot_periods <- function(df, year = 1900, change_year = NULL, fixedyear = TRUE, locale='EN'){
  
  if (locale == 'RU') {
    Sys.setenv(LANGUAGE="ru")
    Sys.setlocale("LC_ALL", "Russian")
  } else {
    Sys.setenv(LANGUAGE="en")
    Sys.setlocale("LC_ALL", "English")
  }
  
  df = df %>% 
    dplyr::mutate_if(params_out$Winter == 1, grwat::replace_year)
  
  prms = params_out %>% 
    dplyr::filter(Order != 0) %>% 
    dplyr::arrange(Order)
  
  labs = grwat::get_plot_labels(locale)
  plotlist = list()
  j = 1
  
  for (i in 1:nn) {
    
    if(!fixedyear){
      year = change_year[i]
    }
    
    d = df[names[i]] %>% 
      as.matrix() %>% 
      as.vector()
    
    is_date = FALSE
    if(units[i] %in% c('Date', 'Month', 'Дата', 'Месяц')){
      d = d %>% as.Date() %>% as.integer()
      is_date = TRUE
    }
    
    d1 = d[df$Year1 < year]
    d2 = d[df$Year1 >= year]
    
    n1 = length(d1)
    n2 = length(d2)
    
    m1 = mean(d1)
    m2 = mean(d2)
    
    rsd1 = round(sd(d1)/m1, 3)
    rsd2 = round(sd(d2)/m2, 3)
    
    periodtitle1 = paste0(labs$beforetitle, year)
    periodtitle2 = paste0(labs$aftertitle, year)
    
    df.plot = data.frame(Value = d, 
                    Period = c(rep(periodtitle1, n1), 
                               rep(periodtitle2, n2)))
    
    means <- df.plot %>% group_by(Period) %>% summarise(Value = mean(Value))
    
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
    
    g = ggplot() + 
      geom_boxplot(data = df.plot, aes(x = Period, y = Value)) +
      geom_point(data = pt.df, aes(x = Period, y = Value), colour="steelblue", shape=20, size=3) +
      coord_flip() +
      labs(title = str_wrap(desc[i], width=labs$wraplength),
           subtitle = paste0(labs$student.t, ' = ', round(tt[[i]]$statistic, 3), ', ',
                             labs$label.p, ' = ', round(tt[[i]]$p.value, 5), ', ',
                             'm1 = ', mean1, ', ', 
                             'm2 = ', mean2,
                             '\n',
                             labs$fisher.f, ' = ', round(ft[[i]]$statistic, 3), ', ',
                             labs$label.p, ' = ', round(ft[[i]]$p.value, 5), ', ',
                             'cv1 = ', rsd1, ', ',
                             'cv2 = ', rsd2),
           x = NULL, 
           y = parse(text=units[i])) +
      theme(plot.title = element_text(size=12, lineheight=.8, face="bold"),
            panel.background = element_rect(fill = colors[i],
                                            colour = colors[i],
                                            size = 0.5, linetype = "solid"))
    
    if(is_date){
      g = g + scale_y_continuous(labels = function(x) {
        return(as.Date(x, origin = "1970-01-01") %>% format(format = '%d-%m'))
      })
    }
    
    plotlist[[j]] = g
    j = j+1
    if (j == 5) {
      multiplot(plotlist = plotlist, cols = 2)
      cat("\n \n")
      plotlist = list()
      j = 1
    }
  }
}