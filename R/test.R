#' Test hydrograph changes
#' 
#' Use this function to test interannual changes or hydrograph separation variables returned by [grwat::gr_summarize()]. Pettitt test is used to detect the change year — i.e. the year which divides the time series into the statistically most differing samples. Student (Welch) and Fisher tests are used to estimate the significance of mean and variance differences of these samples. Theil-Sen test calculates the trend slope value. Mann-Kendall test is performed to reveal the significance of the trend. 
#' 
#' Number of observations formally required for various tests: Pettitt > 0, Mann-Kendall > 2, Theil-Sen > 1, Student > 1, Fisher > 1.
#' 
#' @param df `data.frame` as produced by [grwat::gr_summarize()] function.
#' @param ... Names of the tested variables (quoted).
#' @param year Integer value of year used to divide series in two samples compared by Student and Fisher tests. Defaults to `NULL` which means that the year is calculated automatically by Pettitt test.
#' @param exclude Integer vector of years to be excluded from tests.
#' @param locale Character string locale. Currently only English locale is supported. Defaults to `'EN'`.
#'
#' @return `list` of testing results
#' @export
#' 
#' @example inst/examples/gr_test_vars.R
#' 
gr_test_vars <- function(df, ..., year = NULL, exclude = NULL, locale='EN'){
  
  fields = rlang::exprs(...) %>% as.character()
  
  if(length(fields) == 0)
    fields = params_out %>% 
      dplyr::filter(Order != 0) %>% 
      dplyr::arrange(Order) %>% 
      dplyr::pull(Name)
  
  prms = params_out %>% 
    dplyr::filter(Name %in% fields) %>% 
    dplyr::slice(match(fields, Name))
  
  fixed = !is.null(year)
  
  desc = switch(locale,
                'RU' = prms$Desc,
                'EN' = prms$Descen)
  
  nn = nrow(prms)
  
  ch_year = setNames(rep(NA, nn), prms$Name) # change years
  
  mean1 = setNames(vector(mode = 'list', length = nn), prms$Name) # means for first period
  mean2 = setNames(vector(mode = 'list', length = nn), prms$Name) # means for second period
  
  mratio = setNames(rep(NA, nn), prms$Name) # variance for first period
  
  sd1 = setNames(rep(NA, nn), prms$Name) # variance for first period
  sd2 = setNames(rep(NA, nn), prms$Name) # variance for first period
  
  maxval = setNames(vector(mode = 'list', length = nn), prms$Name) # maximum values
  
  ptt = setNames(vector(mode = 'list', length = nn), prms$Name) # Pettitt test
  mkt = setNames(vector(mode = 'list', length = nn), prms$Name) # Mann-Kendall test
  tst = setNames(vector(mode = 'list', length = nn), prms$Name) # Theil-Sen slope estimation
  ts_fit = setNames(vector(mode = 'list', length = nn), prms$Name) # Theil-Sen regression
  
  tt = setNames(vector(mode = 'list', length = nn), prms$Name) # Student t test for periods
  ft = setNames(vector(mode = 'list', length = nn), prms$Name) # Fisher F test for periods
  
  df = df %>% 
    dplyr::filter(!(Year1 %in% exclude)) %>% 
    dplyr::mutate_if(params_out$Winter == 1, 
                     replace_year)
  
  bar = progress::progress_bar$new(total = nn)
  bar$tick(0)
  
  for (i in 1:nn) {
    
    bar$tick()
    
    # PETTITT TEST FOR CHANGE DETECTION
    
    vl = dplyr::pull(df, prms$Name[i])
    
    uvals = unique(vl)
    
    if (length(uvals[!is.na(uvals)]) < 3)
      next
    
    isdate = FALSE
    if(prms$Unitsen[i] %in% c('Date', 'Month')) {
      isdate = TRUE
      lubridate::year(vl) = 2000
      if (prms$Winter[i] == 1) {
        vl = replace_year(vl)
      }
    }
    
    vl_cmp = !is.na(vl)
    vl_cmp_sum = cumsum(vl_cmp)
    
    ptt[[i]] = trend::pettitt.test(vl[vl_cmp])
    nyear = match(ptt[[i]]$estimate, vl_cmp_sum)
    
    ch_year[i] = ifelse(is.numeric(year), 
                        year, 
                        as.vector(as.matrix(df[nyear, "Year1"]))[1])
    
    maxval[[i]] = max(vl, na.rm = TRUE)
    
    # MANN-KENDALL TEST FOR TREND SIGNIFICANCE
    
    if(length(vl_cmp) > 2) { # mk.test requires at least 3 observations
      if(isdate){
        mkt[[i]] = trend::mk.test(vl[vl_cmp] %>% as.integer())
      } else {
        mkt[[i]] = trend::mk.test(vl[vl_cmp])
      }
    }
    
    
    # THEIL-SEN SLOPE ESTIMATION
    
    df.theil = df %>% 
      dplyr::select('Year1', prms$Name[i])
    
    # values = df.theil[[prms$Name[i]]] %>% 
      # as.matrix() %>%
      # as.vector()
    
    if(isdate){
      # values = values %>% 
      #   as.Date() %>% 
      #   as.integer()
      df.theil[2] = as.integer(vl)
    }
    
    frml = substitute(y ~ x,
                      list(y = as.name(prms$Name[i]),
                           x = as.name('Year1')))
    
    if (nrow(df.theil) > 1) { # slope testing requires at least two observations
      fltr = ! (is.infinite(df.theil[[2]]) | is.nan(df.theil[[2]]) | is.na(df.theil[[2]]))
      ts_fit[[i]]= mblm::mblm(eval(frml), data = df.theil[fltr, ], repeated = FALSE)
      tst[[i]] = trend::sens.slope(df.theil[[2]][fltr])
    }
    
    
    # PERIOD TESTS
    
    vl_int = vl
    if(isdate) vl_int = as.integer(vl)
    
    d1 = vl_int[df$Year1 < ch_year[i]]
    d2 = vl_int[df$Year1 >= ch_year[i]]
    
    d1 = d1[!(is.infinite(d1) | is.na(d1) | is.nan(d1))]
    d2 = d2[!(is.infinite(d2) | is.na(d2) | is.nan(d2))]
    
    mean1[[i]] = round(mean(d1, na.rm = TRUE), 5)
    mean2[[i]] = round(mean(d2, na.rm = TRUE), 5)
    
    sd1[i] = round(sd(d1, na.rm = TRUE), 5)
    sd2[i] = round(sd(d2, na.rm = TRUE), 5)
    
    if(isdate) {
      
      mean1[[i]] = mean1[[i]] %>% as.integer() %>% as.Date(origin = '1970-01-01')
      mean2[[i]] = mean2[[i]] %>% as.integer() %>% as.Date(origin = '1970-01-01')
      
      mratio[i] = lubridate::yday(mean2[[i]]) - lubridate::yday(mean1[[i]])
      
      mean1[[i]] = mean1[[i]] %>% format("%d-%b")
      mean2[[i]] = mean2[[i]] %>% format("%d-%b")
      
      sd1[i] = as.integer(sd1[i])
      sd2[i] = as.integer(sd2[i])
      
    } else {
      mratio[i] = round(100 * (mean2[[i]] - mean1[[i]]) / mean1[[i]], 1)
    }
    
    # Student and Fisher tests requre at least two observations in each sample
    if (length(d1) > 1 & length(d2) > 1) { 
      if ((sum(abs(diff(d1)), na.rm = T) != 0) && (sum(abs(diff(d2)), na.rm = T) != 0)) {
        tt[[i]] = t.test(d1, d2)
        ft[[i]] = var.test(d1, d2)
      }
    }
  }
  
  pvalues = data.frame(
    N = 1:nn,
    Variable = desc,
    Change.Year = ch_year,
    Trend = sapply(ts_fit, function(X) { if(is.null(X)) NA else round(coef(X)[2], 5) }),
    M1 = sapply(mean1, function(X) { if(is.null(X)) NA else X }),
    M2 = sapply(mean2, function(X) { if(is.null(X)) NA else X }),
    MeanRatio = mratio,
    sd1 = sd1,
    sd2 = sd2,
    sdRatio = round(100 * (sd2 - sd1) / sd1, 1),
    Mann.Kendall = sapply(mkt, function(X) { if(is.null(X)) NA else round(X$p.value, 5) }),
    Pettitt = sapply(ptt, function(X) { if(is.null(X)) NA else round(X$p.value, 5) }),
    Student = sapply(tt, function(X) { if(is.null(X)) NA else round(X$p.value, 5) }),
    Fisher = sapply(ft, function(X) { if(is.null(X)) NA else round(X$p.value, 5) })
  )
  
  row.names(pvalues) = 1:nn
  
  tests = list(ptt = ptt,
               mkt = mkt,
               tst = tst,
               ts_fit = ts_fit,
               tt = tt,
               ft = ft,
               year = ch_year,
               maxval = maxval,
               fixed_year = fixed,
               pvalues = pvalues)
  
  return(tests)
}