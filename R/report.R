#' Generate reports with detailed hydrograph analysis
#'
#' @param wd Character. A working directory with specified structure
#'
#' @return Generates a new out working directory with detailed reports
#' @export
#'
#' @examples
report <- function(wd){
  # list basins
  old = setwd(wd)
  on.exit(setwd(old), add = TRUE)
  
  basins = list.dirs(recursive = FALSE, full.names = FALSE)
  
  # Generate reports for each gauge
  for (basin in basins) {
    setwd(wd)
    setwd(basin)
    gauges = list.dirs(recursive = FALSE, full.names = FALSE)
    
    for (gauge in gauges){
      setwd(gauge)
      
      fullpath = getwd()
      
      rmarkdown::render(input = system.file('reports', 'Report.Rmd', package = 'grwat'), 
                        output_file = 'report.pdf',
                        output_dir = fullpath,
                        knit_root_dir = fullpath,
                        encoding = 'UTF-8',
                        params = list(name = gauge, namen = gauge))
      setwd(wd)
      setwd(basin)
      break
    }
    break
  } 
}

#' Run various tests on interannual characteristics
#'
#' @param df data.frame produced by separation function
#' @param fixedyear use fixed year value for long-term changes stimation. Default is FALSE
#' @param year fixed year value for long-term changes estimation
#' @param locale locale for p-value table
#'
#' @return List of testing results
#' @export
run_tests <- function(df, fixedyear = FALSE, year = NULL, locale='EN'){
  
  df = df %>% 
    dplyr::mutate_if(params_out$Winter == 1, 
                     grwat::replace_year)
  
  desc = switch(locale,
                'RU' = params_out$Desc,
                'EN' = params_out$Descen)
  
  nn = nrow(params_out)
  
  change_year = vector(mode = 'integer', length = nn) # change years
  maxval = vector(mode = 'list', length = nn) # maximum values
  
  ptt = vector(mode = 'list', length = nn) # Pettitt test
  mkt = vector(mode = 'list', length = nn) # Mann-Kendall test
  tst = vector(mode = 'list', length = nn) # Theil-Sen slope estimation
  ts_fit = vector(mode = 'list', length = nn) # Theil-Sen regression
  
  tt = vector(mode = 'list', length = nn) # Student t test for periods
  ft = vector(mode = 'list', length = nn) # Fisher F test for periods
  
  for (i in 1:nn) {
    
    # PETTITT TEST FOR CHANGE DETECTION
    
    vl = df[i] %>% as.matrix() %>% as.vector()
    
    isdate = FALSE
    if(params_out$Unitsen[i] %in% c('Date', 'Month')) {
      isdate = TRUE
      vl = as.Date(vl)
    }
    
    vl_cmp = !is.na(vl)
    vl_cmp_sum = cumsum(vl_cmp)
    
    ptt[[i]] = pettitt.test(vl[vl_cmp])
    nyear = match(ptt[[i]]$estimate, vl_cmp_sum)
    
    change_year[i] = as.vector(as.matrix(df[nyear, "Year1"]))[1]
    
    maxval[[i]] = max(vl)
    
    # MANN-KENDALL TEST FOR TREND SIGNIFICANCE
    
    if(isdate){
      mkt[[i]] = mk.test(vl[vl_cmp] %>% as.integer())
    } else {
      mkt[[i]] = mk.test(vl[vl_cmp])
    }
    
    # THEIL-SEN SLOPE ESTIMATION
    
    df.theil = df %>% select_('Year1', params_out$Name[i]) %>% na.omit
    
    values = df.theil[names[i]] %>% 
      as.matrix() %>%
      as.vector()
    if(isdate){
      values = values %>% 
        as.Date() %>% 
        as.integer()
      df.theil[params_out$Name[i]] = values
    }
    
    frml = substitute(y ~ x,
                      list(y = as.name(params_out$Name[i]),
                           x = as.name('Year1')))
    ts_fit[[i]] <- mblm(eval(frml),
                        data = df.theil)
    
    tst[[i]] = sens.slope(values)
    
    # PERIOD TESTS
    
    if (!fixedyear) {
      year = change_year[i]
    } else if (is.null(year)) {
      stop('Fixed year value is not given')
    }
    
    vl_int = as.integer(vl_cmp)
    
    d1 = vl_int[df$Year1 < year]
    d2 = vl_int[df$Year1 >= year]
    
    tt[[i]] = t.test(d1, d2)
    ft[[i]] = var.test(d1, d2)
    
  }
  
  stable = data.frame(
    N = 1:nn,
    Value = desc,
    Mann.Kendall = sapply(mkt, function(X) X$p.value %>% round(5)),
    Pettitt = sapply(ptt, function(X) X$p.value %>% round(5)),
    Student = sapply(tt, function(X) X$p.value %>% round(5)),
    Fisher = sapply(ft, function(X) X$p.value %>% round(5))
  )
  
  row.names(stable) = 1:nn
  
  return(list(ptt = ptt,
              mkt = mkt,
              tst = tst,
              ts_fit = ts_fit,
              tt = tt,
              ft = ft,
              change_year = change_year,
              maxval = maxval,
              stable = stable))
}

#' Kable p-values table by coloring it with green-yellow-red palette
#'
#' @param tests Test result returned by run_tests
#'
#' @return kabled version of p-values table coloured
#' @export
#'
#' @examples
kable_tests <- function(tests){
  gcolor = '#99cc00'
  ycolor = '#e6e600'
  rcolor = '#ff9966'
  
  stable = tests$stable %>% mutate(
    Mann.Kendall = cell_spec(Mann.Kendall, "latex", 
                             background = ifelse(Mann.Kendall < 0.01, gcolor, 
                                                 ifelse(Mann.Kendall < 0.05, ycolor, rcolor))),
    Pettitt = cell_spec(Pettitt, "latex", 
                        background = ifelse(Pettitt < 0.01, gcolor, 
                                            ifelse(Pettitt < 0.05, ycolor, rcolor))),
    Student = cell_spec(Student, "latex", 
                        background = ifelse(Student < 0.01, gcolor, 
                                            ifelse(Student < 0.05, ycolor, rcolor))),
    Fisher = cell_spec(Fisher, "latex", 
                       background = ifelse(Fisher < 0.01, gcolor, 
                                           ifelse(Fisher < 0.05, ycolor, rcolor)))
  )
  
  kable(stable, booktabs = T, longtable = T, escape = FALSE, format = "latex",
        caption = 'p-values of statistical criteria') %>% 
        kable_styling(font_size = 11,
                      repeat_header_text = "",
                      latex_options = c("striped", "repeat_header"))
}