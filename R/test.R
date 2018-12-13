#' Run various tests on interannual characteristics.
#' Required number of observations for various tests: 
#' Pettitt > 0, Mann.Kendall > 2, Theil-Sen slope > 1, Student > 1, 
#' 
#' @param df data.frame produced by separation function
#' @param year fixed year value for long-term changes estimation
#' @param locale locale for p-value table
#' @param ... fields (quoted)
#'
#' @return List of testing results
#' @export
test_variables <- function(df, ..., year = NULL, locale='EN'){
  
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
      vl = as.Date(vl)
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
    
    if(isdate){
      mkt[[i]] = trend::mk.test(vl[vl_cmp] %>% as.integer())
    } else {
      mkt[[i]] = trend::mk.test(vl[vl_cmp])
    }
    
    # THEIL-SEN SLOPE ESTIMATION
    
    df.theil = df %>% dplyr::select_('Year1', prms$Name[i]) %>% na.omit()
    
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
    Variable = desc,
    Change.Year = ch_year,
    Trend = sapply(ts_fit, function(X) { if(is.null(X)) NA else round(coef(X)[2], 5) }), 
    Mann.Kendall = sapply(mkt, function(X) { if(is.null(X)) NA else round(X$p.value, 5) }),
    Pettitt = sapply(ptt, function(X) { if(is.null(X)) NA else round(X$p.value, 5) }),
    Student = sapply(tt, function(X) { if(is.null(X)) NA else round(X$p.value, 5) }),
    Fisher = sapply(ft, function(X) { if(is.null(X)) NA else round(X$p.value, 5) })
  )
  
  row.names(pvalues) = 1:nn
  
  return(list(ptt = ptt,
              mkt = mkt,
              tst = tst,
              ts_fit = ts_fit,
              tt = tt,
              ft = ft,
              fixed_year = fixed,
              year = ch_year,
              maxval = maxval,
              pvalues = pvalues))
}