#' Generate reports with detailed hydrograph analysis
#'
#' @param wd character string. A path to a working directory with specified structure
#' @param year integer. A fixed value for separation year 
#' @param map TRUE/FALSE. If `map = TRUE` then map of source data is included in report. Default value is FALSE
#' @param locale character string. Currently English (`'EN'`) and Russian (`'RU'`) locales are supported for HTML output. PDF is always rendered in English 
#' @param pdf TRUE/FALSE. If `pdf = TRUE` then ouput is rendered as a single PDF file. Otherwise the output is rendered as single HTML file. Default is FALSE 
#'
#' @return Generates a new out working directory with detailed reports
#' @export
report_basins <- function(wd, year = NULL, map = FALSE, locale = 'EN', pdf = FALSE){
  
  t1 = Sys.time()
  
  # list basins
  old = setwd(wd)
  on.exit(setwd(old))
  
  basins = list.dirs(recursive = FALSE, full.names = FALSE)
  
  # Generate reports for each gauge
  for (basin in basins) {
    setwd(wd)
    setwd(basin)
    gauges = list.dirs(recursive = FALSE, full.names = FALSE)
    
    for (gauge in gauges)
      grwat::report_gauge(gauge, year, map, locale, pdf)
  } 
  
  t2 = Sys.time()
  
  message('Total elapsed time: ', format(.POSIXct(difftime(t2, t1, units = "secs"), tz = "GMT"), "%H:%M:%S"))
}

#' Generate report for the specified gauge folder
#'
#' @param wd A path to a gauge directory
#' @param year integer. A fixed value for separation year 
#' @param map TRUE/FALSE. If `map = TRUE` then map of source data is included in report. Default value is FALSE
#' @param locale character string. Currently English (`'EN'`) and Russian (`'RU'`) locales are supported for HTML output. PDF is always rendered in English 
#' @param pdf TRUE/FALSE. If `pdf = TRUE` then ouput is rendered as a single PDF file. Otherwise the output is rendered as single HTML file. Default is FALSE 
#'
#' @return
#' @export
report_gauge <- function(wd, year = NULL, map = FALSE, locale = 'EN', pdf = FALSE){
  
  t1 = Sys.time()
  
  oldwd = setwd(wd)
  on.exit(setwd(oldwd))
  
  fullpath = getwd()
  
  template = ifelse(pdf, 'Report_PDF.Rmd', 'Report_HTML.Rmd')
  outfile = ifelse(pdf, 'report.pdf', 'report.html')
  
  rmarkdown::render(input = system.file('reports', template, package = 'grwat'), 
                    output_file = outfile,
                    output_dir = fullpath,
                    knit_root_dir = fullpath,
                    encoding = 'UTF-8',
                    quiet = TRUE,
                    params = list(name = basename(fullpath),
                                  fixedyear = !is.null(year),
                                  year = year,
                                  map = map,
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
kable_tests <- function(tests, locale = 'EN', format = 'latex'){
  gcolor = '#99cc00'
  ycolor = '#e6e600'
  rcolor = '#ff9966'
  ncolor = '#999999'
  
  labs = get_plot_labels(locale)
  
  pvalues = tests$pvalues %>% dplyr::mutate(
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