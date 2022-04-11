#' Report hydrograph separation and variables
#' 
#' This function generates a graphical HTML report that summarizes separation of hydrograph, its variables and their statistical properties. 
#'
#' @param sep `data.frame` of hydrograph separation as returned by [grwat::gr_separate()] function. 
#' @param vars `data.frame` of hydrograph variables as returned by [grwat::gr_summarize()] function. 
#' @param output Character string path to the output file. Must have `.html` extension.
#' @param year Integer value of year used to divide series in two samples compared by Student and Fisher tests. Defaults to `NULL` which means that the year is calculated automatically by Pettitt test. Defaults to `NULL`.
#' @param exclude Integer vector of years to be excluded from reporting. Defaults to `NULL`.
#' @param locale Character string locale. Currently only English locale is supported. Defaults to `'EN'`.
#' @param temp Boolean. Plot temperature on the top of hydrograph? Defaults to `FALSE`. If both `temp = TRUE` and `prec = TRUE`, then the axis is drawn for precipitation.
#' @param prec Boolean. Plot precipitation on the top of hydrograph? Defaults to `FALSE`. If both `temp = TRUE` and `prec = TRUE`, then the axis is drawn for precipitation.
#' @param span Integer number of days to accumulate precipitation for plotting. Defaults to `5`.
#'
#' @export
#'
#' @example inst/examples/gr_report.R
#' 
gr_report <- function(sep, vars, output = 'Report.html', year = NULL, exclude = NULL, temp = FALSE, prec = FALSE, span = 5, locale = 'EN') {
  t1 = Sys.time()
  
  rmarkdown::render(input = system.file('reports', 'Report_HTML.Rmd', package = 'grwat'), 
                    output_file = output,
                    encoding = 'UTF-8',
                    quiet = TRUE,
                    params = list(name = basename(output),
                                  sep = sep,
                                  vars = vars,
                                  fixedyear = !is.null(year),
                                  year = year,
                                  exclude = exclude,
                                  prec = prec,
                                  temp = temp, 
                                  span = span,
                                  locale = locale))
  t2 = Sys.time()
  
  message('Elapsed time: ', format(.POSIXct(difftime(t2, t1, units = "secs"), tz = "GMT"), "%H:%M:%S"))
}

#' Tabular representation of tests
#' 
#' This function is used to represent the results of [grwat::gr_test_vars()] in a tabular form. Used mainly in [grwat::gr_report()], but can be used for your own purposes.
#'
#' @param tests `list` of tests as returned by [grwat::gr_test_vars()] function.
#' @param locale Character string locale. Currently only English locale is supported. Defaults to `'EN'`.
#' @param format Character string encoding the type of output. Currently `'html'` only is supported.
#'
#' @return HTML table as returned by [knitr::kable()] function.
#' @export
#' 
#' @example inst/examples/gr_kable_tests.R
#' 
gr_kable_tests <- function(tests, format = 'html', locale = 'EN'){
  gcolor = '#99cc00' # green
  ycolor = '#e6e600' # yellow
  rcolor = '#ff9966' # red
  ncolor = '#FFFFFF' # no
  ucolor = '#FFC0CB' # up
  dcolor = '#ADD8E6' # down
  zcolor = '#D3D3D3' # zero
  
  labs = get_plot_labels(locale)
  
  pvalues = tests$pvalues %>% dplyr::mutate(
    Trend = dplyr::case_when(!is.na(Trend) ~ kableExtra::cell_spec(Trend, format,
            background = ifelse(is.na(Trend), ncolor,
                         ifelse(abs(Trend) < 1e-4, zcolor,
                         ifelse(Trend < 0, dcolor, ucolor))))),
    MeanRatio = dplyr::case_when(!is.na(MeanRatio) ~ kableExtra::cell_spec(MeanRatio, format,
                background = ifelse(is.na(MeanRatio), ncolor,
                             ifelse(abs(MeanRatio) < 5, zcolor,
                             ifelse(MeanRatio < 0, dcolor, ucolor))))),
    sdRatio = dplyr::case_when(!is.na(sdRatio) ~ kableExtra::cell_spec(sdRatio, format,
               background = ifelse(is.na(sdRatio), ncolor,
                            ifelse(abs(sdRatio) < 5, zcolor,
                            ifelse(sdRatio < 0, dcolor, ucolor))))),
    Mann.Kendall = dplyr::case_when(!is.na(Mann.Kendall) ~ kableExtra::cell_spec(Mann.Kendall, format, 
                   background = ifelse(is.na(Mann.Kendall), ncolor,
                                ifelse(Mann.Kendall < 0.01, gcolor, 
                                ifelse(Mann.Kendall < 0.05, ycolor, rcolor))))),
    Pettitt = dplyr::case_when(!is.na(Pettitt) ~ kableExtra::cell_spec(Pettitt, format, 
              background = ifelse(is.na(Pettitt), ncolor,
                           ifelse(Pettitt < 0.01, gcolor, 
                           ifelse(Pettitt < 0.05, ycolor, rcolor))))),
    Student = dplyr::case_when(!is.na(Student) ~ kableExtra::cell_spec(Student, format, 
              background = ifelse(is.na(Student), ncolor,
                           ifelse(Student < 0.01, gcolor, 
                           ifelse(Student < 0.05, ycolor, rcolor))))),
    Fisher = dplyr::case_when(!is.na(Fisher) ~ kableExtra::cell_spec(Fisher, format, 
             background = ifelse(is.na(Fisher), ncolor,
                          ifelse(Fisher < 0.01, gcolor, 
                          ifelse(Fisher < 0.05, ycolor, rcolor)))))
  )
  
  if (locale == 'RU')
    pvalues = pvalues %>%
      dplyr::rename(Переменная = Variable,
                   `Переломный год` = Change.Year,
                    Тренд = Trend,
                   `M,%` = MeanRatio,
                   `sd,%` = sdRatio,
                   `Манн-Кендалл` = Mann.Kendall,
                    Петтитт = Pettitt,
                    Стьюдент = Student,
                    Фишер = Fisher)
    
  tab = knitr::kable(pvalues, booktabs = T, longtable = T, escape = FALSE, format = format,
                     caption = labs$pheader)
  if (format == 'latex')
     kableExtra::kable_styling(tab, font_size = 11,
                               repeat_header_text = "",
                               latex_options = c("striped", "repeat_header"))
  else
    kableExtra::kable_styling(tab,
                              bootstrap_options = "striped")
}