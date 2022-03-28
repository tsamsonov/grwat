#' Generates HTML report based on separation and variables
#'
#' @param sep 
#' @param vars 
#' @param output 
#' @param year 
#' @param exclude 
#' @param locale 
#'
#' @return
#' @export
#'
#' @examples
gr_report <- function(sep, vars, output = 'Report.html', year = NULL, exclude = NULL, prec = FALSE, temp = FALSE, span = 5, locale = 'EN') {
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

#' Kable p-values table by coloring it using green-yellow-red palette
#'
#' @param tests Test result returned by `test_variables()` function
#' @param locale character string. Currently English (`'EN'`) and Russian (`'RU'`) locales are supported for HTML output. PDF is always rendered in English
#' @param format character string. Currently `'latex'` or `'html'` are supported
#'
#' @return kabled version of p-values table coloured
#' @export
gr_kable_tests <- function(tests, locale = 'EN', format = 'latex'){
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