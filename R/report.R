#' Generate reports with detailed hydrograph analysis
#'
#' @param wd Character. A path to a working directory with specified structure
#'
#' @return Generates a new out working directory with detailed reports
#' @export
report_basins <- function(wd, year = NULL, map = FALSE){
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
      grwat::report_gauge(gauge, year, map)
  } 
}

#' Generate report for the specified gauge folder
#'
#' @param wd A path to a gauge directory
#'
#' @return
#' @export
report_gauge <- function(wd, year = NULL, map = FALSE){
  oldwd = setwd(wd)
  on.exit(setwd(oldwd))
  
  fullpath = getwd()
  
  rmarkdown::render(input = system.file('reports', 'Report.Rmd', package = 'grwat'), 
                    output_file = 'report.pdf',
                    output_dir = fullpath,
                    knit_root_dir = fullpath,
                    encoding = 'UTF-8',
                    quiet = TRUE,
                    params = list(name = basename(fullpath),
                                  fixedyear = !is.null(year),
                                  year = year,
                                  map = map))
}

#' Run various tests on interannual characteristics
#'
#' @param df data.frame produced by separation function
#' @param fixedyear use fixed year value for long-term changes stimation. Default is FALSE
#' @param year fixed year value for long-term changes estimation
#' @param locale locale for p-value table
#' @param ... fields (quoted)
#'
#' @return List of testing results
#' @export
test_variables <- function(df, ..., change_year = NULL, locale='EN'){

  fields = rlang::exprs(...) %>% as.character()
  if(length(fields) == 0)
    fields = params_out %>% 
                dplyr::filter(Order != 0) %>% 
                dplyr::arrange(Order) %>% 
                dplyr::select(Name) %>% 
                as.matrix() %>% 
                as.vector()
  
  prms = params_out %>% 
    dplyr::filter(Name %in% fields) %>% 
    dplyr::slice(match(Name, fields))
  
  desc = switch(locale,
                'RU' = prms$Desc,
                'EN' = prms$Descen)
  
  nn = nrow(prms)
  
  ch_year = vector(mode = 'integer', length = nn) # change years
  maxval = vector(mode = 'list', length = nn) # maximum values
  
  ptt = vector(mode = 'list', length = nn) # Pettitt test
  mkt = vector(mode = 'list', length = nn) # Mann-Kendall test
  tst = vector(mode = 'list', length = nn) # Theil-Sen slope estimation
  ts_fit = vector(mode = 'list', length = nn) # Theil-Sen regression
  
  tt = vector(mode = 'list', length = nn) # Student t test for periods
  ft = vector(mode = 'list', length = nn) # Fisher F test for periods
  
  df = df %>% 
    dplyr::mutate_if(params_out$Winter == 1, 
                     replace_year)
  
  for (i in 1:nn) {
    
    # PETTITT TEST FOR CHANGE DETECTION
    
    vl = df[, prms$Name[i]] %>% as.matrix() %>% as.vector()
    
    isdate = FALSE
    if(prms$Unitsen[i] %in% c('Date', 'Month')) {
      isdate = TRUE
      vl = as.Date(vl)
    }
    
    vl_cmp = !is.na(vl)
    vl_cmp_sum = cumsum(vl_cmp)
    
    ptt[[i]] = trend::pettitt.test(vl[vl_cmp])
    nyear = match(ptt[[i]]$estimate, vl_cmp_sum)
    
    ch_year[i] = ifelse(is.numeric(change_year), 
                        change_year, 
                        as.vector(as.matrix(df[nyear, "Year1"]))[1])
    
    maxval[[i]] = max(vl)
    
    # MANN-KENDALL TEST FOR TREND SIGNIFICANCE
    
    if(isdate){
      mkt[[i]] = trend::mk.test(vl[vl_cmp] %>% as.integer())
    } else {
      mkt[[i]] = trend::mk.test(vl[vl_cmp])
    }
    
    # THEIL-SEN SLOPE ESTIMATION
    
    df.theil = df %>% dplyr::select_('Year1', prms$Name[i]) %>% na.omit
    
    values = df.theil[prms$Name[i]] %>% 
      as.matrix() %>%
      as.vector()
    if(isdate){
      values = values %>% 
        as.Date() %>% 
        as.integer()
      df.theil[prms$Name[i]] = values
    }
    
    frml = substitute(y ~ x,
                      list(y = as.name(prms$Name[i]),
                           x = as.name('Year1')))
    ts_fit[[i]] <- mblm::mblm(eval(frml),
                        data = df.theil)
    
    tst[[i]] = trend::sens.slope(values)
    
    # PERIOD TESTS
    
    vl_int = vl
    if(isdate) vl_int = as.integer(vl)
    
    d1 = vl_int[df$Year1 < ch_year[i]]
    d2 = vl_int[df$Year1 >= ch_year[i]]
    
    tt[[i]] = t.test(d1, d2)
    ft[[i]] = var.test(d1, d2)
    
  }
  
  pvalues = data.frame(
    N = 1:nn,
    Value = desc,
    Mann.Kendall = sapply(mkt, function(X) X$p.value %>% round(5)),
    Pettitt = sapply(ptt, function(X) X$p.value %>% round(5)),
    Student = sapply(tt, function(X) X$p.value %>% round(5)),
    Fisher = sapply(ft, function(X) X$p.value %>% round(5))
  )
  
  row.names(pvalues) = 1:nn
  
  return(list(ptt = ptt,
              mkt = mkt,
              tst = tst,
              ts_fit = ts_fit,
              tt = tt,
              ft = ft,
              change_year = ch_year,
              maxval = maxval,
              pvalues = pvalues))
}

#' Kable p-values table by coloring it using green-yellow-red palette
#'
#' @param tests Test result returned by `test_variables()` function
#'
#' @return kabled version of p-values table coloured
#' @export
kable_tests <- function(tests, locale = 'EN'){
  gcolor = '#99cc00'
  ycolor = '#e6e600'
  rcolor = '#ff9966'
  
  labs = get_plot_labels(locale)
  
  pvalues = tests$pvalues %>% dplyr::mutate(
    Mann.Kendall = kableExtra::cell_spec(Mann.Kendall, "latex", 
                             background = ifelse(Mann.Kendall < 0.01, gcolor, 
                                                 ifelse(Mann.Kendall < 0.05, ycolor, rcolor))),
    Pettitt = kableExtra::cell_spec(Pettitt, "latex", 
                        background = ifelse(Pettitt < 0.01, gcolor, 
                                            ifelse(Pettitt < 0.05, ycolor, rcolor))),
    Student = kableExtra::cell_spec(Student, "latex", 
                        background = ifelse(Student < 0.01, gcolor, 
                                            ifelse(Student < 0.05, ycolor, rcolor))),
    Fisher = kableExtra::cell_spec(Fisher, "latex", 
                       background = ifelse(Fisher < 0.01, gcolor, 
                                           ifelse(Fisher < 0.05, ycolor, rcolor)))
  )
  
  knitr::kable(pvalues, booktabs = T, longtable = T, escape = FALSE, format = "latex",
               caption = labs$pheader) %>% 
               kableExtra::kable_styling(font_size = 11,
                                        repeat_header_text = "",
                                        latex_options = c("striped", "repeat_header"))
}

#' Gete hydrograph parameters list
#'
#' @return data.frame of parameters
#' @export
#'
#' @examples
#' grwat::get_variables()
get_variables <- function(){
  return(params_out)
}