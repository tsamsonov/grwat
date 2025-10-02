utils::globalVariables(c(
  'ACF', 'Date', 'Datefake', 'Dsmin', 'Dwmin', 'Name', 'nydate', 'Period', 'Prec', 'Preccum', 'Q', 'Qbase', 'Qfake',
  'Qrain', 'Qspri', 'Runoff', 'Runtype', 'Temp', 'Typefake', 'Value', 'Year', 'Year1', 'Yearfake',
  'duration', 'n', 'old_summer', 'old_winter', 'perc', 'summermonth', 'wintermonth', 'y', 'year')
)

#' Plot hydrograph separation
#' 
#' The function plots river hydrograph by filling the different flow types using colors. Matrix layouts can be used if multiple plots are needed. Temperature and precipitation can be overlaid.
#'
#' @param df `data.frame` of hydrograph separation as produced by [grwat::gr_separate()].
#' @param years Integer vector of years to be plotted.
#' @param layout `matrix` that encodes the order of plotting.
#' @param pagebreak Logical. Whether to break page between plots (used by [grwat::gr_report()]). Defaults to `FALSE`.
#' @param temp Boolean. Add temperature curve to the plot? Defaults to `FALSE`. If both `temp = TRUE` and `prec = TRUE`, then the axis is drawn for precipitation.
#' @param prec Boolean. Add precipitation curve to the plot? Defaults to `FALSE`. If both `temp = TRUE` and `prec = TRUE`, then the axis is drawn for precipitation.
#' @param span Integer number of days to accumulate precipitation for plotting.
#' @param print Boolean. Print plot? Defaults to `TRUE`. Use `FALSE` if you want to tweak the plot aesthetics before plotting.
#' @param yrange Boolean. Y range for each plot. Defaults to `'uniform'` which means that the highest value of Y axis is the same on all plots and is determined by the highest discharge for all years to be plotted. Use `'each'` if you want each plot to have its own maximum along Y axis determined by the highest discharge for this year. You can also use any numeric value `ymax` in a form `yrange = ymax` to determine your own Y scale maximum for all plots.
#'
#' @return `list` of `ggplot2` objects, one for each year, representing the hydrograph separation
#' 
#' @export
#' 
#' @example inst/examples/gr_plot_sep.R
#' 
gr_plot_sep <- function(df, years = NULL, layout = as.matrix(1), 
                        pagebreak = FALSE, temp = FALSE, prec = FALSE, 
                        span = 5, print = TRUE, yrange = 'uniform'){
  
  if (grenv$loc == 'RU') {
    Sys.setenv(LANGUAGE="ru")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "ru_RU.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "Russian"))
    
  } else if (grenv$loc == 'UA') {
    Sys.setenv(LANGUAGE="uk")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "uk_UA.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "Ukrainian"))
    
  } else {
    Sys.setenv(LANGUAGE="en")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "en_US.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "English"))
  }
  
  labs = grlabs[[grenv$loc]]
  
  if (grenv$loc != 'EN')
    labs = gr_unescape(labs)
  
  df = df |> dplyr::mutate(Year = lubridate::year(Date))
  
  if(!is.null(years)){
    df = df |> dplyr::filter(Year %in% unique(c(years, years + 1)))
  } else {
    years = df |> dplyr::pull(Year) |> unique() |> sort() 
  }
  
  yrs = df |> 
    dplyr::group_by(.data$Year) |> 
    dplyr::summarise(nydate = Date[which(Qspri>0)[1]],
                     dspend = max(Date[which(Qspri>0)])) |> 
    dplyr::filter(!is.na(nydate))
  
  n = nrow(yrs)
  
  max.runoff = ifelse(is.numeric(yrange), yrange, max(df$Q, na.rm = TRUE))
  max.temp = max(df$Temp, na.rm = TRUE)
  min.temp = min(df$Temp, na.rm = TRUE)
  
  max.prec = max(df$Prec, na.rm = TRUE)
  if (prec && (span > 1)) {
    df = dplyr::mutate(df, Preccum = zoo::rollapply(Prec, span, sum, align = 'right', fill = NA))
    max.prec = max(df$Preccum, na.rm = TRUE)
  }
  
  coef_temp = (max.temp - min.temp) / max.runoff
  coef_prec = max.prec / max.runoff
  
  allplotlist = list()
  plotlist = list()
  j = 1
  
  cli::cli_progress_bar('Plotting separation', total = n)  
  
  for (i in 1:n) {
    
    if (!(yrs$Year[i] %in% years)) {
      cli::cli_progress_update()
      next
    }
    
    # bar$tick()
    
    begin.date = yrs$nydate[i]
    
    # Initialize by the end of the year
    end.date = lubridate::ceiling_date(begin.date, "year") - lubridate::days(1) 
    
    clipped.remark = labs$clipped.remark
    year = yrs$Year[i]
    
    dspstart = begin.date
    lubridate::year(dspstart) = lubridate::year(begin.date)
    
    dspend = yrs$dspend[i]
    lubridate::year(dspend) = lubridate::year(begin.date)
    
    if (i != n){
      nextyear <- yrs$Year[i+1]
      if ((nextyear - year) == 1) {
        # Change to the end date of water-resources year
        end.date = yrs$nydate[i+1] - lubridate::days(1) 
        clipped.remark = ""
      }
    }
    
    yeardata = df |>   
      dplyr::filter(dplyr::between(.data$Date, begin.date-1, end.date))
    
    if (yrange == 'each') {
      max.runoff = max(yeardata$Q, na.rm = TRUE)
      max.temp = max(yeardata$Temp, na.rm = TRUE)
      max.prec = max(yeardata$Prec, na.rm = TRUE)
      coef_temp = (max.temp - min.temp) / max.runoff
      coef_prec = max.prec / max.runoff
    } 
    
    graphdata = yeardata |>  
      tidyr::pivot_longer(c(.data$Qthaw,Qrain,Qspri,Qbase),
                          names_to = "Runtype", values_to = "Runoff") |> 
                          # factor_key=TRUE) |> 
      dplyr::mutate(Runoff = ifelse(.data$Runoff < 0, 0,Runoff))
    
    graphdata$Runtype = factor(graphdata$Runtype,
                               levels = c("Qrain", "Qspri", "Qthaw", "Qbase"),
                               labels = c(labs$rain, labs$seasonal, labs$thaw, labs$ground))
    
    g = ggplot2::ggplot() + 
      ggplot2::annotate("rect", 
               xmin = dspstart-1, xmax = dspend+1,
               ymax = max.runoff, ymin = 0,
               fill = 'black',
               alpha = 0.1) +
      ggplot2::geom_area(data = graphdata, mapping = ggplot2::aes(x =Date, y =Runoff, fill =Runtype),
                         linewidth = 0.1, color = 'black', na.rm = TRUE) + 
      # geom_line(aes(group = Runtype), size = 0.1, color = 'black') +
      ggplot2::coord_cartesian(ylim = c(0, max.runoff), clip = 'off') +
      ggplot2::scale_fill_manual(values = c("coral2", "deepskyblue3", "darkturquoise", "bisque4"), 
                        name = paste0(labs$discharge.type, ':')) +
      ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      ggplot2::labs(title = year,
           subtitle = paste(begin.date, "-", end.date, clipped.remark),
           x = labs$date, y = substitute(d ~ ', ' ~ m, list(d = labs$discharge, m = labs$m3s))) +
      ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.8, face="bold"),
            legend.position="bottom")
    
    if (prec) {
        if (span > 0) {
          g = g +
            ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~ . * coef_prec, 
                                                                     name = paste0(labs$preccum, ' (', span, ' ',  labs$day, ')'))) +
            ggplot2::geom_line(data = yeardata,
                               mapping = ggplot2::aes(x =Date, y =Preccum / coef_prec),
                               color = 'springgreen1', linewidth = 0.5, na.rm = TRUE)
        } else {
          g = g +
            ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~ . * coef_prec, name = labs$prec)) +
            ggplot2::geom_line(data = yeardata,
                               mapping = ggplot2::aes(x =Date, y =Prec / coef_prec),
                               color = 'springgreen1', linewidth = 0.5, na.rm = TRUE)
        }
    }
    
    if (temp) {
        yeardata = dplyr::mutate(yeardata, 
                                 Negat = ifelse(.data$Temp < 0,Temp, NA),
                                 Posit = ifelse(.data$Temp > 0,Temp, NA))
        g = g +
          ggplot2::geom_line(data = yeardata,
                             mapping = ggplot2::aes(x =Date, 
                                                    y = (.data$Temp - min.temp) / coef_temp),
                             color = 'purple',
                             linewidth = 0.5, 
                             na.rm = TRUE) +
          ggplot2::geom_line(data = yeardata,
                             mapping = ggplot2::aes(x =Date, 
                                                    y = (.data$Posit - min.temp) / coef_temp),
                             color = 'red',
                             linewidth = 0.5,
                             na.rm = TRUE) +
          ggplot2::geom_line(data = yeardata,
                             mapping = ggplot2::aes(x =Date, 
                                                    y = (.data$Negat - min.temp) / coef_temp),
                             color = 'blue',
                             linewidth = 0.5,
                             na.rm = TRUE) +
          ggplot2::geom_hline(yintercept = - min.temp / coef_temp, color = "purple", linewidth = 0.3, linetype = "dotted") +
          ggplot2::labs(color = NULL)
        
        if (!prec)
          g = g + ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~ . * coef_temp + min.temp, name = labs$temp))
    }
    
    g = g + 
      ggplot2::geom_vline(xintercept = dspstart-1, color = "black", linewidth=0.3) +
      ggplot2::geom_vline(xintercept = dspend+1, color = "black", linewidth=0.3) +
      ggplot2::geom_label(data = data.frame(x = dspstart, 
                                            y = 0.9 * max.runoff, 
                                            text = format(dspstart, format="%d-%m")),
                          mapping = ggplot2::aes(.data$x,y, label =text, hjust = 1),
                          size = 3, fill = "white", label.padding = ggplot2::unit(0.15, "lines")) +
      ggplot2::geom_label(data = data.frame(x = dspend, 
                                            y = 0.9 * max.runoff, 
                                            text = format(dspend, format="%d-%m")),
                          mapping = ggplot2::aes(.data$x,y, label =text, hjust = 0),
                          size = 3, fill = "white", label.padding = ggplot2::unit(0.15, "lines"))
    
    plotlist[[j]] = g
    j = j+1
    if (j == length(layout)+1) {
      if (print) multiplot(plotlist = plotlist, layout = layout)
      if (pagebreak) cat("\n\n\\pagebreak\n")
      allplotlist = c(allplotlist, plotlist)
      plotlist = list()
      j = 1
    }
    
    cli::cli_progress_update()
    
  }
  
  if (j > 1) {
    if (print) multiplot(plotlist = plotlist, layout = layout)
    allplotlist = c(allplotlist, plotlist)
    cli::cli_progress_update()
  }
  
  cli::cli_progress_done()
  
  invisible(allplotlist)
}

#' Plot interannual hydrograph variable changes
#' 
#' This function plots the hydrograph separation variables produced by [grwat::gr_summarize()]. Different background fill colors and line types are used to differentiate seasons and variable types. 
#'
#' @param df `data.frame` of hydrograph and meteorological variables produced by [grwat::gr_summarize()].
#' @param ... Quoted sequence of variable names.
#' @param tests `list` of tests for the same variables (generated by [grwat::gr_test_vars()] function). If tests are specified, then they are added to the plot.
#' @param exclude Integer vector of years to be excluded from plotting.
#' @param smooth Logical. If `TRUE` then local smoothing regression is plotted. Defaults to `TRUE`.
#' @param layout `matrix` that encodes the order of plotting.
#' @param pagebreak Logical. Whether to break page between plots ([grwat::gr_report()]). Defaults to `FALSE`.
#' @param print Boolean. Print plot? Defaults to `TRUE`. Use `FALSE` if you want to tweak the plot aesthetics before plotting.
#'
#' @return `list` of `ggplot2` objects, one for each variable, representing its interannual changes 
#' 
#' @export
#'
#' @example inst/examples/gr_plot_vars.R
#' 
gr_plot_vars <- function(df, ..., tests = NULL, exclude = NULL, smooth = TRUE, 
                         layout = as.matrix(1), pagebreak = FALSE, print = TRUE){
  
  if (grenv$loc == 'RU') {
    Sys.setenv(LANGUAGE="ru")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "ru_RU.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "Russian"))
    
  } else if (grenv$loc == 'UA') {
    Sys.setenv(LANGUAGE="uk")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "uk_UA.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "Ukrainian"))
    
  } else {
    Sys.setenv(LANGUAGE="en")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "en_US.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "English"))
  }
  
  labs = grlabs[[grenv$loc]]
  
  if (grenv$loc != 'EN')
    labs = gr_unescape(labs)
  
  fields = rlang::exprs(...) |> as.character()
  
  if(length(fields) == 0)
    fields = params_out |>  
      dplyr::filter(.data$Order != 0) |>  
      dplyr::arrange(.data$Order) |>  
      dplyr::pull(.data$Name)
  
  prms = params_out |> 
    dplyr::filter(.data$Name %in% fields) |> 
    dplyr::slice(match(fields,Name))
  
  if (is.logical(tests))
    if (tests == TRUE)
      tests = gr_test_vars(df, ...)
  
  df = df |> 
    dplyr::mutate_if(params_out$Type == 'Date', function(X) { 
      lubridate::year(X) = 2000 
      return(X)
    }) |> 
    dplyr::mutate_if(params_out$Winter == 1, replace_year) |> 
    dplyr::mutate_at(dplyr::vars(-.data$Year1, -.data$Year2), function(X) {
      structure(ifelse(df$Year1 %in% exclude, NA, X), class = class(X))
    }) # EXCLUDE YEARS
  
  minyear = min(df$Year1)
  maxyear = max(df$Year1)
  breaks = gr_fullseq(c(minyear, maxyear), 10)
  minbreaks = gr_fullseq(c(minyear, maxyear), 5)
  
  units = switch(grenv$loc,
                 'RU' = prms$Units,
                 'UA' = prms$Unitsua,
                 'EN' = prms$Unitsen)
  desc = switch(grenv$loc,
                'RU' = prms$Desc,
                'UA' = prms$Descua,
                'EN' = prms$Descen)
  
  allplotlist = list()
  plotlist = list()
  j = 1
  
  
  nn = nrow(prms)
  cli::cli_progress_bar('Plotting variables', total = nn) 
  
  for (i in 1:nn) {
    
    cli::cli_progress_update()
    
    # MAIN DATA FOR PLOTTING
    g = ggplot2::ggplot(df, ggplot2::aes_string(x = "Year1", y = prms$Name[i])) + 
      ggplot2::scale_x_continuous(breaks = breaks, minor_breaks = minbreaks) +
      ggplot2::labs(title = stringr::str_wrap(desc[i], width=labs$wraplength),
           x = labs$subtitle, 
           y = parse(text=units[i])) +
      ggplot2::theme(plot.title = ggplot2::element_text(size=12, lineheight=.8, face="bold"),
            panel.background = ggplot2::element_rect(fill = prms$Color[i],
                                            colour = prms$Color[i],
                                            linewidth = 0.5, linetype = "solid"))
    if (!is.null(exclude)) {
      g = g + 
        ggplot2::labs(caption = paste('Excluded years:', paste(exclude, collapse = ', '))) +
        ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0))
    }
    
    if (smooth) {
      g = g + ggplot2::geom_smooth(method = 'loess', formula = y ~ x)
    }
    
    # TEST
    if (!is.null(tests)) {
      
      varname = prms$Name[i]

      if (!is.null(tests$ptt[[varname]])) { # if test for this variable was successfull

        ltype = ifelse(tests$fixed_year, 'dotted',
                       ifelse(tests$ptt[[varname]]$p.value > 0.05, 'dashed', 'solid'))
        g = g +
          ggplot2::geom_vline(xintercept = tests$year[varname], color = "red",
                              linewidth=0.5, linetype = ltype) +
          ggplot2::annotate("text", label = tests$year[varname],
                   x = tests$year[varname] + 4, y = tests$maxval[[varname]],
                   size = 4, colour = "red")
      }

      if(!is.null(tests$tst[[varname]]) && !is.null(tests$ts_fit[[varname]])) {
        ts_ltype = ifelse(mblm::summary.mblm(tests$ts_fit[[varname]])$coefficients[2, 4] > 0.05, 'dashed', 'solid')
        
        g = g + ggplot2::geom_abline(intercept = coef(tests$ts_fit[[varname]])[1], slope = coef(tests$ts_fit[[varname]])[2],
                            color = 'red', linewidth = 1, linetype = ts_ltype)
      }

      fixedstr = ifelse(tests$fixed_year, '',
                        paste0(labs$pettitt.u, ' = ', ifelse(is.null(tests$ptt[[varname]]), 'NA',
                                                             round(tests$ptt[[varname]]$statistic, 3)), ', ',
                               labs$label.p, ' = ', ifelse(is.null(tests$ptt[[varname]]), 'NA',
                                                           round(tests$ptt[[varname]]$p.value, 5))))

      g = g + #labs(subtitle = as.expression(bquote(bold(.(labs$kendall.z))~'=')))
        ggplot2::labs(subtitle = paste0(labs$kendall.z, ' = ', ifelse(is.null(tests$mkt[[varname]]), 'NA',
                                                             round(tests$mkt[[varname]]$statistic, 3)), ', ',
                               labs$label.p, ' = ', ifelse(is.null(tests$mkt[[varname]]), 'NA',
                                                           round(tests$mkt[[varname]]$p.value, 5)), '\n',
                               labs$theil.i, ' = ', ifelse(is.null(tests$ts_fit[[varname]]), 'NA',
                                                           round(coef(tests$ts_fit[[varname]])[2], 5)), ', ',
                               labs$label.p, ' = ', ifelse(is.null(tests$ts_fit[[varname]]), 'NA',
                                                           round(mblm::summary.mblm(tests$ts_fit[[varname]])$coefficients[2, 4], 5)), '. ',
                               fixedstr))
    }
    
    date_labels = "%d-%b"

    # PLOT TYPE
    if (prms$Chart[i] == 'line') {
      g = g + ggplot2::geom_line() + ggplot2::geom_ribbon(ggplot2::aes_string(ymin = 0, ymax = prms$Name[i]), alpha = 0.3)
    } else if (prms$Chart[i] == 'point') {
      g = g + ggplot2::geom_point(size = 1.5) + ggplot2::geom_line(linewidth = 0.2)
      date_labels = "%b"
    } else if (prms$Chart[i] == 'plate') {
      g = g + ggplot2::geom_point(shape = 15, size = 1.5) + ggplot2::geom_step(linewidth = 0.2)
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
      if (print) multiplot(plotlist = plotlist, layout = layout)
      if (pagebreak) cat("\n\n\\pagebreak\n")
      allplotlist = c(allplotlist, plotlist)
      plotlist = list()
      j = 1
    }
  }
  
  if (j > 1) {
    if (print) multiplot(plotlist = plotlist, layout = layout)
    allplotlist = c(allplotlist, plotlist)
  }
  
  cli::cli_progress_done()
  
  invisible(allplotlist)
}

#' Plot long-term hydrograph variable changes
#' 
#' This function generates boxplots of the hydrograph separation variables produced by [grwat::gr_summarize()]. The data for each variable is divided into two samples: before and after the change year either set by `year` parameter or extracted from `tests` (statistically estimated). Different background fill colors are used to differentiate seasons types. 
#'
#' @param df `data.frame` of hydrograph and meteorological variables produced by [grwat::gr_summarize()].
#' @param ... Quoted sequence of variable names.
#' @param year Integer. Change year value to separate two periods (overridden by tests if it is supplied).
#' @param exclude Integer vector of years to be excluded from plotting.
#' @param tests Tests list for the same variables (generated by [grwat::gr_test_vars()] function)
#' @param layout `matrix` that encodes the order of plotting.
#' @param pagebreak Logical. Whether to break page between plots (needed for reporting). Defaults to `FALSE`.
#' @param print Boolean. Print plot? Defaults to `TRUE`. Use `FALSE` if you want to tweak the plot aesthetics before plotting.
#'
#' @return `list` of `ggplot2`  objects, one for each variable, representing its long-term changes
#' 
#' @export
#' 
#' @example inst/examples/gr_plot_periods.R
#' 
gr_plot_periods <- function(df, ..., year = NULL, exclude = NULL, tests = NULL, 
                            layout = as.matrix(1), pagebreak = FALSE, print = TRUE){
  
  if(is.null(year) & is.null(tests))
    stop('You must provide year or tests parameter')
  
  if (grenv$loc == 'RU') {
    Sys.setenv(LANGUAGE="ru")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "ru_RU.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "Russian"))
    
  } else if (grenv$loc == 'UA') {
    Sys.setenv(LANGUAGE="uk")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "uk_UA.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "Ukrainian"))
    
  } else {
    Sys.setenv(LANGUAGE="en")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "en_US.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "English"))
  }
  
  labs = grlabs[[grenv$loc]]
  
  if (grenv$loc != 'EN')
    labs = gr_unescape(labs)
  
  fields = rlang::exprs(...) |> as.character()

  if(length(fields) == 0)
    fields = params_out |> 
    dplyr::filter(.data$Order != 0) |> 
    dplyr::arrange(.data$Order) |> 
    dplyr::pull(.data$Name)
  
  prms = params_out |> 
    dplyr::filter(.data$Name %in% fields) |> 
    dplyr::slice(match(fields,Name))
  
  if (is.logical(tests))
    if (tests == TRUE)
      tests = gr_test_vars(df, ...)
  
  df = df |>  
    dplyr::filter(!(.data$Year1 %in% exclude)) |>  
    dplyr::mutate_if(params_out$Winter == 1, replace_year)
  
  units = switch(grenv$loc,
                 'RU' = prms$Units,
                 'UA' = prms$Unitsua,
                 'EN' = prms$Unitsen)
  desc = switch(grenv$loc,
                'RU' = prms$Desc,
                'UA' = prms$Descua,
                'EN' = prms$Descen)
  
  allplotlist = list()
  plotlist = list()
  j = 1
  
  nn = nrow(prms)
  cli::cli_progress_bar('Plotting periods', total = nn)
  
  for (i in 1:nn) {
    
    cli::cli_progress_update()
    
    if(!is.null(tests)){
      year = tests$year[i]
    }
    
    d = df[prms$Name[i]] |> 
      as.matrix() |> 
      as.vector()
    
    is_date = FALSE
    if(prms$Unitsen[i] %in% c('Date', 'Month')){
      
      d = d |> as.Date() 
      lubridate::year(d) <- 2000 # fake year
      
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
      ggplot2::geom_boxplot(data = df.plot, ggplot2::aes(x =Period, y =Value)) +
      ggplot2::coord_flip() +
      ggplot2::labs(title = stringr::str_wrap(desc[i], width = labs$wraplength),
           x = NULL, 
           y = parse(text = units[i])) +
      ggplot2::theme(plot.title = ggplot2::element_text(size=12, lineheight=.8, face="bold"),
            panel.background = ggplot2::element_rect(fill = prms$Color[i],
                                            colour = prms$Color[i],
                                            linewidth = 0.5, linetype = "solid"))
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
      
      means <- df.plot |> 
        dplyr::group_by(Period) |> 
        dplyr::summarise(Value = mean(Value, na.rm = TRUE))
      
      mean1 = ifelse(is_date, 
                     m1 |> as.integer() |> as.Date(origin = '1970-01-01') |> format("%d-%b"),
                     m1 |> round(3)
      )
      
      mean2 = ifelse(is_date, 
                     m2 |> as.integer() |> as.Date(origin = '1970-01-01') |> format("%d-%b"),
                     m2 |> round(3)
      )
      
      m1 = ifelse(is_date,
                  m1 |> as.integer(),
                  m1)
      
      m2 = ifelse(is_date,
                  m2 |> as.integer(),
                  m2)
      
      pt.df = data.frame(Value = c(m1, m2), 
                         Period = c(periodtitle1, periodtitle2))
      
      g = g + 
        ggplot2::geom_point(data = pt.df, ggplot2::aes(x =Period, y =Value), colour="steelblue", shape=20, size=3) +
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
        return(as.Date(x, origin = "1970-01-01") |> format(format = '%d-%m'))
      })
    }
    
    plotlist[[j]] = g
    j = j+1
    if (j == length(layout)+1) {
      if (print) multiplot(plotlist = plotlist, layout = layout)
      if (pagebreak) cat("\n\n\\pagebreak\n")
      allplotlist = c(allplotlist, plotlist)
      plotlist = list()
      j = 1
    }
  }
  
  if (j > 1) {
    if (print) multiplot(plotlist = plotlist, layout = layout)
    allplotlist = c(allplotlist, plotlist)
  }
  
  cli::cli_progress_done()
  
  invisible(allplotlist)
  
}

#' Plot minimum runoff month
#' 
#' Generate a histogram of a minimum runoff month for two periods: before and after the change year set by `year` parameter.
#'
#' @param df `data.frame` of hydrograph and meteorological variables as produced by [grwat::gr_summarize()].
#' @param year Integer. Change year value to separate two periods.
#' @param exclude Integer vector of years to be excluded from plotting.
#' @param tests Tests list for the same variables (generated by [grwat::gr_test_vars()] function)
#' @param pagebreak Logical. Whether to break page between plots (needed for reporting). Defaults to `FALSE`.
#' @param print Boolean. Print plot? Defaults to `TRUE`. Use `FALSE` if you want to tweak the plot aesthetics before plotting.
#'
#' @return `list` of two `ggplot2` objects, representing the histogram of a minimum runoff month for two periods: before and after the change year
#' 
#' @export
#' 
#' @example inst/examples/gr_plot_minmonth.R
#' 
gr_plot_minmonth <- function(df, year = NULL, exclude = NULL, tests = NULL, pagebreak = FALSE, print = TRUE){
  
  year_summer = year
  year_winter = year
  
  if(is.null(year)) {
    if (!is.null(tests)) {
      year_summer = tests[['year']]['Dsmin']
      year_winter = tests[['year']]['Dwmin']
    }
    else stop('You must supply either year or tests parameter')
  }
  
  if (grenv$loc == 'RU') {
    Sys.setenv(LANGUAGE="ru")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "ru_RU.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "Russian"))
    
  } else if (grenv$loc == 'UA') {
    Sys.setenv(LANGUAGE="uk")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "uk_UA.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "Ukrainian"))
    
  } else {
    Sys.setenv(LANGUAGE="en")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "en_US.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "English"))
  }
  
  labs = grlabs[[grenv$loc]]
  
  if (grenv$loc != 'EN')
    labs = gr_unescape(labs)
  
  periodtitle1_summer = paste0(labs$beforetitle, year_summer)
  periodtitle2_summer = paste0(labs$aftertitle, year_summer)
  
  periodtitle1_winter = paste0(labs$beforetitle, year_winter)
  periodtitle2_winter = paste0(labs$aftertitle, year_winter)
  
  chart.data = df |> 
    dplyr::filter(!(Year1 %in% exclude)) |> 
    dplyr::select(Dsmin, Dwmin, Year1) |> 
    dplyr::filter(!is.na(Dsmin) & !is.na(Dwmin)) |> 
    dplyr::mutate(summermonth = lubridate::month(Dsmin),
                  wintermonth = lubridate::month(Dwmin),
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
  
  df.summer = chart.data |> 
    dplyr::group_by(old_summer, summermonth) |> 
    dplyr::tally() |> 
    dplyr::ungroup() |> 
    tidyr::complete(summermonth, old_summer, fill = list(n=0)) |>  
    dplyr::mutate(perc = 100*.data$n/sum(.data$n))
  
  df.winter = chart.data |> 
    dplyr::group_by(old_winter, wintermonth) |> 
    dplyr::tally() |> 
    dplyr::ungroup() |> 
    tidyr::complete(wintermonth, old_winter, fill = list(n=0)) |> 
    dplyr::mutate(perc = 100*n/sum(n))
  
  df.summer.all = chart.data |> 
    dplyr::group_by(summermonth) |> 
    dplyr::tally() |> 
    dplyr::ungroup() |>
    dplyr::mutate(perc = 100*n/sum(n))
  
  df.winter.all = chart.data |> 
    dplyr::group_by(.data$wintermonth) |> 
    dplyr::tally() |> 
    dplyr::ungroup() |>
    dplyr::mutate(perc = 100*.data$n/sum(.data$n))
  
  g.summer = ggplot2::ggplot() +
    ggplot2::geom_col(data = df.summer, 
                      ggplot2::aes(x = summermonth, y = perc, fill = old_summer), 
             position = "dodge") +
    ggplot2::geom_col(data = df.summer.all, 
                      ggplot2::aes(x = summermonth, y = perc), 
             colour = "black", 
             fill = NA, 
             linewidth = 1) +
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
                      ggplot2::aes(x =wintermonth, y =perc), 
             colour="black", 
             fill = NA, 
             linewidth = 1) +
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
      ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0))
  }
  
  plist = list(g.summer, g.winter)
  
  if (print) multiplot(plotlist = plist)
  
  if (pagebreak) cat("\n\n\\pagebreak\n")
  
  invisible(plist)
}


#' Plot change year density
#' 
#' The function extracts change years from results of [grwat::gr_test_vars()] and plots their probability density. Since for every variable the change year is individual, this procedure allows finding the one most probable year, which is the mode of the distribution. This year is highlighted by the line and labeled on the plot.
#'
#' @param tests `list` of tests generated by [grwat::gr_test_vars()].
#' @param type Character string type of the plot. Currently only `'year'` is supported, which means that the distribution density of the change year detected by Pettitt test is visualized. Ignored until other types are implemented.
#' @param print Boolean. Print plot? Defaults to `TRUE`. Use `FALSE` if you want to tweak the plot aesthetics before plotting.
#'
#' @return `ggplot2` object representing the selected type of the tested variable
#' 
#' @export
#'
#' @example inst/examples/gr_plot_tests.R
#' 
gr_plot_tests <- function(tests, type = 'year', print = TRUE) {
  
  if (grenv$loc == 'RU') {
    Sys.setenv(LANGUAGE="ru")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "ru_RU.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "Russian"))
    
  } else if (grenv$loc == 'UA') {
    Sys.setenv(LANGUAGE="uk")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "uk_UA.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "Ukrainian"))
    
  } else {
    Sys.setenv(LANGUAGE="en")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "en_US.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "English"))
  }
  
  labs = grlabs[[grenv$loc]]
  
  if (grenv$loc != 'EN')
    labs = gr_unescape(labs)
  
  years = tests$year[!is.na(tests$year)]
  dens = density(years, from = min(years), to = max(years), n = max(years) - min(years) + 1)
  ddf = data.frame(year = dens$x, dens = dens$y)
  
  modeval = as.integer(dens$x[which.max(dens$y)])
  
  plt = ggplot2::ggplot() +    
    ggplot2::geom_line(data = ddf, ggplot2::aes(x =year, y = dens), color = 'black') +
    ggplot2::geom_area(data = ddf, ggplot2::aes(x =year, y = dens), alpha = 0.2) +
    ggplot2::geom_vline(xintercept = modeval, color = "steelblue4", linewidth = 1) +
    ggplot2::annotate("text", label = modeval, 
             x = modeval + 0.05 * (max(dens$x) - min(dens$x)), y = 0.01, 
             size = 5, colour = "steelblue4") +
    ggplot2::labs(title = labs$year.density,
         x = labs$subtitle,
         y = NULL) +
    ggplot2::scale_x_continuous(expand = c(0,0)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .1))) +
    ggplot2::theme_bw()
  
  if (print) print(plt)
  
  invisible(plt)
  
}

#' Plot runoff ACF
#'
#' The function plots the autocorrelation function (ACF) for daily runoff time series. A number of days corresponding to the specified `autocorr` value is highlighted.
#'
#' @param hdata `data.frame` with first column as `Date` and the second column as runoff
#' @param autocorr Numeric value of the autocorrelation for which the time period will be highlighted. Defaults to `0.7`.
#' @param maxlag Integer value of the maximum daily lag used to calculate the correlation. Defaults to `30`.
#' @param print Boolean. Print plot? Defaults to `TRUE`. Use `FALSE` if you want to tweak the plot aesthetics before plotting.
#'
#' @return `ggplot2` object representing the autocorrelation function (ACF) for daily runoff time series
#' 
#' @export
#'
#' @example inst/examples/gr_plot_acf.R
#' 
gr_plot_acf <- function(hdata, autocorr = 0.7, maxlag = 30, print = TRUE) {
  
  max_period = hdata |> 
    grwat::gr_get_gaps() |> 
    dplyr::filter(.data$type == 'data',duration == max(.data$duration))
  
  acf_data = hdata |> 
    dplyr::rename(Date = 1) |> 
    dplyr::filter(dplyr::between(.data$Date, max_period$start_date, max_period$end_date)) |> 
    dplyr::pull(2)
  
  afun = acf(acf_data, lag.max = maxlag, plot = FALSE)
    
  tab = data.frame(Days = seq_along(afun$acf), ACF = afun$acf)
  
  days = min(which(afun$acf[,1,1] < autocorr, arr.ind = TRUE)) - 1
  
  plt = ggplot2::ggplot(tab, ggplot2::aes(.data$Days,ACF)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(ggplot2::aes(yintercept = autocorr), 
               color = 'red') +
    ggplot2::annotate("text", label = autocorr,
             x = 0, y = autocorr + 0.05,
             size = 4, colour = "red") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = days), 
                   color = 'blue', linewidth = 1) +
    ggplot2::annotate("text", label = days,
             x = days + 1, y = 1,
             size = 4, colour = "blue") +
    ggplot2::labs(title = 'Autocorrelation function (ACF)') +
    ggplot2::theme(plot.title = ggplot2::element_text(size=12, lineheight=.8, 
                                                      face="bold"))
  
  if (print) print(plt)
  
  invisible(plt)
  
}

#' Runoff matrix plot
#' 
#' The function plots runoff values, components and seasons using the matrix-based approach. The X axis corresponds to the day of the year, and the Y axis corresponds to the year. The function is useful when the whole picture of river runoff needs to be assessed.
#'
#' @param df `data.frame` of hydrograph separation produced by [grwat::gr_separate()].
#' @param years Integer vector of years to be plotted. Defaults to `NULL`.
#' @param type Character string. Supported options are `'runoff'`, `'component'`, and `'season'`. Defaults to `'runoff'`.
#' @param print Boolean. Print plot? Defaults to `TRUE`. Use `FALSE` if you want to tweak the plot aesthetics before plotting.
#'
#' @return `ggplot2` object representing the runoff values, components or seasons using the matrix-based approach
#' 
#' @export
#'
#' @example inst/examples/gr_plot_matrix.R
#' 
gr_plot_matrix <- function(df, years = NULL, type = 'runoff', print = TRUE) {
  
  if (grenv$loc == 'RU') {
    Sys.setenv(LANGUAGE="ru")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "ru_RU.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "Russian"))
    
  } else if (grenv$loc == 'UA') {
    Sys.setenv(LANGUAGE="uk")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "uk_UA.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "Ukrainian"))
    
  } else {
    Sys.setenv(LANGUAGE="en")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "en_US.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "English"))
  }
  
  labs = grlabs[[grenv$loc]]
  
  if (grenv$loc != 'EN')
    labs = gr_unescape(labs)
  
  tab = df
  
  if (!is.null(years)) {
    tab = tab[lubridate::year(tab[[1]]) %in% years, ]
  }
  
  tab$Datefake = tab[[1]]
  tab$Yearfake = lubridate::year(tab[[1]])
  lubridate::year(tab$Datefake) <- 2001
  
  date_labels = "%b"
  
  if (type == 'runoff') {
    
    plt = ggplot2::ggplot(tab, ggplot2::aes(.data$Datefake,Yearfake)) +
      ggplot2::geom_raster(ggplot2::aes(fill =Q)) +
      ggplot2::scale_fill_distiller(palette = "Blues", direction = 1, 
                                    na.value = "white") +
      ggplot2::scale_x_date(date_labels = date_labels,
                            date_breaks = "1 month",
                            expand = c(0,0),
                            limits = c(lubridate::ymd(20010101), 
                                       lubridate::ymd(20011231))) +
      ggplot2::scale_y_continuous(expand = c(0,0), 
                                  breaks = gr_fullseq(range(tab$Yearfake), 5)) +
      ggplot2::labs(title = labs$discharge.value,
                    x = labs$date, 
                    y = labs$subtitle,
                    fill = labs$m3s) +
      ggplot2::theme_bw()
    
    if (print) print(plt)
    
    invisible(plt)
    
  } else if (type == 'season') {
    tab$Typefake = factor(dplyr::case_when(tab$Season == 0 ~ 'Spring',
                                           tab$Season == 1 ~ 'Summer',
                                           tab$Season == 2 ~ 'Winter'))
    
    plt = ggplot2::ggplot(tab, ggplot2::aes(.data$Datefake,Yearfake)) +
      ggplot2::geom_raster(ggplot2::aes(fill =Typefake)) +
      ggplot2::scale_fill_manual(values = c('cadetblue1', 'coral', 'steelblue'),
                                 na.value = "white") +
      ggplot2::scale_x_date(date_labels = date_labels,
                            date_breaks = "1 month",
                            expand = c(0,0),
                            limits = c(lubridate::ymd(20010101), 
                                       lubridate::ymd(20011231))) +
      ggplot2::scale_y_continuous(expand = c(0,0), 
                                  breaks = gr_fullseq(range(tab$Yearfake), 5)) +
      ggplot2::labs(title = labs$season,
                    x = labs$date, 
                    y = labs$subtitle,
                    fill = NULL) +
      ggplot2::theme_bw()
    
    if (print) print(plt)
    
    invisible(plt)
    
  } else if (type == 'component') {
    tab$Qfake = apply(cbind(tab$Qspri, 
                            tab$Qrain, 
                            tab$Qthaw), MARGIN = 1, FUN = which.max)
    tab$Qfake = ifelse(tab$Quick == 0, 4, tab$Qfake)
    tab$Qfake = factor(dplyr::case_when(tab$Qfake == 1 ~ 'Spring',
                                        tab$Qfake == 2 ~ 'Rain',
                                        tab$Qfake == 3 ~ 'Thaw',
                                        tab$Qfake == 4 ~ 'Ground'), 
                       levels = c('Spring', 'Rain', 'Thaw', 'Ground'))
    
    plt = ggplot2::ggplot(tab, ggplot2::aes(.data$Datefake,Yearfake)) +
      ggplot2::geom_raster(ggplot2::aes(fill =Qfake)) +
      ggplot2::scale_fill_manual(values=c("deepskyblue3", "coral2", 
                                          "darkturquoise", "bisque4"),
                                 na.value = "white") +
      ggplot2::scale_x_date(date_labels = date_labels,
                            date_breaks = "1 month",
                            expand = c(0,0),
                            limits = c(lubridate::ymd(20010101), 
                                       lubridate::ymd(20011231))) +
      ggplot2::scale_y_continuous(expand = c(0,0), 
                                  breaks = gr_fullseq(range(tab$Yearfake), 5)) +
      ggplot2::labs(title = labs$components,
                    x = labs$date, 
                    y = labs$subtitle,
                    fill = NULL) +
      ggplot2::theme_bw()
    
    if (print) print(plt)
    
    invisible(plt)
    
  } else {
    stop(cli::col_white(cli::bg_red(cli::style_bold('grwat:'))), ' ', 
         cli::style_italic(type), ' - unknown plot type')
  }
}

#' Ridgeline hydrograph plot
#' 
#' A convenient wrapper around [ggridges::geom_ridgeline()] to visualize multiple river hydrographs at once.
#'
#' @param df `data.frame` with date (1st) and runoff (2nd) columns.
#' @param years Integer vector of years to be plotted.
#' @param pal Numeric or character string. Color palette identifier passed to [ggplot2::scale_fill_distiller()].
#' @param rev Boolean. Reverse the palette? Defaults to `FALSE`.
#' @param scale Numeric scale factor passed to [ggridges::geom_ridgeline()]. Defaults to `0.01`.
#' @param alpha Numeric opacity value of the ridgeline plot. Defaults to `0.8`.
#' @param print Boolean. Print plot? Defaults to `TRUE`. Use `FALSE` if you want to tweak the plot aesthetics before plotting.
#'
#' @return `ggplot2` object representing the multiple river hydrographs at once using the ridgeline plot approach
#' 
#' @export
#'
#' @example inst/examples/gr_plot_ridge.R
#' 
gr_plot_ridge <- function(df, years, pal = 4, rev = FALSE, scale = 0.01, alpha = 0.8, print = TRUE) {
  
  rlang::check_installed("ggridges", reason = "to use `gr_plot_ridge()`")
  
  if (grenv$loc == 'RU') {
    Sys.setenv(LANGUAGE="ru")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "ru_RU.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "Russian"))
    
  } else if (grenv$loc == 'UA') {
    Sys.setenv(LANGUAGE="uk")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "uk_UA.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "Ukrainian"))
    
  } else {
    Sys.setenv(LANGUAGE="en")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "en_US.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "English"))
  }
  
  labs = grlabs[[grenv$loc]]
  
  if (grenv$loc != 'EN')
    labs = gr_unescape(labs)
  
  df_sel = df |>
    dplyr::rename(Date = 1, Q = 2) |>
    dplyr::mutate(Year = lubridate::year(.data$Date),
                  Datefake = lubridate::ymd(20000101) + lubridate::yday(.data$Date)) |>
    dplyr::filter(.data$Year %in% years)

  plt = ggplot2::ggplot(df_sel, 
                  ggplot2::aes(
                    x =Datefake, 
                    y = factor(.data$Year),
                    height =Q, 
                    group = factor(.data$Year), 
                    fill =Year
                  )
                ) + 
    ggridges::geom_ridgeline(scale = scale, alpha = alpha) +
    ggplot2::scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    ggplot2::scale_fill_distiller(palette = pal, direction = 1 - 2*rev) +
    ggridges::theme_ridges() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = labs$date, y = labs$year)
  
  if (print) print(plt)
  
  invisible(plt)
}

#' Horizon hydrograph plot
#' 
#' A convenient wrapper around [ggHoriPlot::geom_horizon()] to visualize multiple river hydrographs at once.
#'
#' @param df `data.frame` with date (1st) and runoff (2nd) columns.
#' @param years Integer vector of years to be plotted.
#' @param pal Numeric or character string. Color palette identifier passed to [ggplot2::scale_fill_distiller()].
#' @param rev Boolean. Reverse the palette? Defaults to `FALSE`.
#' @param scale Numeric scale factor passed to [ggHoriPlot::geom_horizon()]. Defaults to `6`.
#' @param print Boolean. Print plot? Defaults to `TRUE`. Use `FALSE` if you want to tweak the plot aesthetics before plotting.
#'
#' @return `ggplot2` object representing multiple river hydrographs at once using the horizon plot approach
#' 
#' @export
#'
#' @example inst/examples/gr_plot_hori.R
#' 
gr_plot_hori <- function(df, years, pal = 'Blues', rev = TRUE, scale = 6, print = TRUE) {
  
  rlang::check_installed(c("ggHoriPlot", "ggthemes"), reason = "to use `gr_plot_hori()`")
  
  if (grenv$loc == 'RU') {
    Sys.setenv(LANGUAGE="ru")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "ru_RU.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "Russian"))
    
  } else if (grenv$loc == 'UA') {
    Sys.setenv(LANGUAGE="uk")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "uk_UA.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "Ukrainian"))
    
  } else {
    Sys.setenv(LANGUAGE="en")
    switch(.Platform$OS.type,
           'unix' = Sys.setlocale("LC_ALL", "en_US.UTF-8"),
           'windows' = Sys.setlocale("LC_ALL", "English"))
  }
  
  labs = grlabs[[grenv$loc]]
  
  if (grenv$loc != 'EN')
    labs = gr_unescape(labs)
  
  df_sel = df |>
    dplyr::rename(Date = 1, Q = 2) |>
    dplyr::mutate(Year = lubridate::year(.data$Date),
                  Datefake = lubridate::ymd(20000101) + lubridate::yday(.data$Date)) |>
    dplyr::filter(.data$Year %in% years)
  
  plt = ggplot2::ggplot(df_sel,  ggplot2::aes(.data$Datefake,Q)) +
    ggHoriPlot::geom_horizon(origin = 'min', horizonscale = scale) +
    ggplot2::facet_wrap(~factor(.data$Year), ncol = 1, strip.position = 'left') +
    ggplot2::scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    ggplot2::scale_fill_brewer(palette = pal, direction = 1 - 2*rev) +
    ggthemes::theme_few() +
    ggplot2::theme(
      panel.spacing.y = ggplot2::unit(0, "lines"),
      strip.text.y = ggplot2::element_text(size = 7, angle = 0, hjust = 0),
      legend.position = 'none',
      strip.text.y.left = ggplot2::element_text(angle = 0),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank()
    )+
    ggplot2::labs(x = labs$date, y = labs$year)
  
  if (print) print(plt)
  
  invisible(plt)
  
}