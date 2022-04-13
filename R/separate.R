# This is a technical function to update grwat core from a separate project
# Please fork grwat-core repository to contribute
update_core <- function() {
  file.copy('../grwat-core/grwat_core.cpp', 
            'src/grwat_core.cpp', overwrite = TRUE)
}

#' Check the correctness of data frame for separating
#' 
#' This function is called inside [gr_separate()], but can be used explicitly inside your code.
#'
#' @param df `data.frame` with four columns: date, discharge, temperature, precipitation, as required by [gr_separate()].
#'
#' @return stops execution if `df` contains the wrong number of columns, or the columns have the wrong types, or the data in columns is incorrect (e.g. discharge or precipitation are negative).
#' @export
#'
#' @example inst/examples/gr_check_data.R
#' 
gr_check_data <- function(df) {
  if (length(df) != 4)
    stop(crayon::white$bgRed$bold('grwat:'), 
         ' the number of columns in data frame (', length(df), ') is not equal to 4')
  
  if (!lubridate::is.Date(df[[1]]) || !is.numeric(df[[2]]) || !is.numeric(df[[3]]) || !is.numeric(df[[4]]))
    stop(crayon::white$bgRed$bold('grwat:'), 
         ' the four columns of input data frame (date, discharge, temperature, precipitation) must be of ', 
         crayon::white$italic('Date, numeric, numeric, numeric'), ' data types')
  
  if (sum(df[[2]] < 0, na.rm = T) > 0)
    stop(crayon::white$bgRed$bold('grwat:'), 
         ' there are negative values in discharge (2nd) column, please fix the data before proceeding')
  
  if (sum(df[[4]] < 0, na.rm = T) > 0)
    stop(crayon::white$bgRed$bold('grwat:'), 
         ' there are negative values in precipitation (4th) column, please fix the data before proceeding')
  
  message(crayon::white$bold('grwat:'), ' data frame is correct')
}

#' Check the correctness of parameters list for separating
#'
#' @param params `list` of separation parameters, as returned by [grwat::gr_get_params()] function
#' @param df `data.frame` with four columns: date, discharge, temperature, precipitation, as required by [gr_separate()]. Required when params is a `list` of parameter `list`s. Defaults to `NULL`.
#'
#' @return stops the execution if anything is wrong and prints the exact reason of the error. Otherwise prints the message that everything is OK
#' @export
#'
#' @example inst/examples/gr_check_params.R
#' 
gr_check_params <- function(params, df = NULL) {
  
  template = gr_get_params()
  
  n = 1
  listed = FALSE
  
  if (is.list(params[[1]])) {
    
    if (is.null(df)) {
      stop(crayon::white$bgRed$bold('grwat:'), ' ',
           crayon::white$italic('df'), ' parameter is needed, because ', 
           crayon::white$italic('params'), ' is a list of lists.')
    }
    
    gr_check_data(df)
    
    n = length(params)
    listed = TRUE
    
    if (length(unique(lubridate::year(df[[1]]))) != n) {
      stop(crayon::white$bgRed$bold('grwat:'), ' the length of parameters list must be equal to 1 or to the number of years in the data')
    }
    
  }
  
  for (i in 1:n) {
    
    par = params
    
    if (listed) {
      par = params[[i]]
    }
    
    d = setdiff(names(par), names(template))
    if (length(d) > 0) {
      stop(crayon::white$bgRed$bold('grwat:'), ' ',
           crayon::white$italic(paste(d, collapse = ', ')), 
           ' parameter(s) not known. Please use ', crayon::cyan$italic('gr_get_params()'), ' result as a template.')
    }
    
    d = setdiff(names(template), names(par))
    if (length(d) > 0) {
      stop(crayon::white$bgRed$bold('grwat:'), ' ',
           crayon::white$italic(paste(d, collapse = ', ')), 
           ' parameter(s) needed. Please use ', crayon::cyan$italic('gr_get_params()'), ' result as a template.')
    }
    
    d = intersect(names(template), names(par))
    types1 = sapply(par[d], class)
    types2 = sapply(template[d], class)
    
    td = which(types1 != types2)
    if(length(td) > 0) {
      stop(crayon::white$bgRed$bold('grwat:'), ' ',
           crayon::white$italic(paste(d[td], collapse = ', ' )), 
           ' parameter(s) must be of ',
           crayon::cyan$italic(paste(types2[td], collapse = ', ')), 
           ' type(s) ')
    }
  }
  
  
  
  message(crayon::white$bold('grwat:'), ' parameters list and types are OK')
    
}

#' Advanced hydrograph separation
#' 
#' Separates the runoff into genetic components: groundwater, thaw, rain and spring.
#'
#' @param df `data.frame` with four columns: date, runoff, temperature, precipitation.
#' @param params `list` of separation parameters, as returned by [grwat::gr_get_params()] function. Can also be a `list` of such `list`s if modified parameters are required for some years. In this case the length of `params` must be equal to the number of calendar years in `df` or be equal to `1`.
#' @param debug Boolean. If `TRUE` then additional attributes `jittered` and `params` are written to the output `data.frame`. `jittered` is an integer vector of years for which the separation parameters were randomly jittered. `params` is a list of separation parameter lists used for each year (some o those may have been jittered). Defaults to `FALSE`.
#'
#' @return A `data.frame` with 11 columns: 
#' 
#' | __Column__ | __Description__ |
#' | ------ | ----------- |
#' | `Date`   | date |
#' | `Q`      | total runoff |
#' | `Qbase`  | baseflow |
#' | `Quick`  | quickflow |
#' | `Qspri`  | spring flood |
#' | `Qrain`  | rain floods | 
#' | `Qthaw`  | thaw floods |
#' | `Type`   | a combination of flow types |
#' | `Year`   | a water-resources year |
#' | `Temp`   | temperature |
#' | `Prec`   | precipitation |
#' 
#' @export
#'
#' @example inst/examples/gr_separate.R
#' 
gr_separate <- function(df, params = gr_get_params(), debug = FALSE) {
  
  if (!is.list(params[[1]]))
    gr_check_data(df)
  
  gr_check_params(params, df)
  
  df = df %>% 
    dplyr::rename(Date = 1) %>% 
    dplyr::filter(!is.na(.data$Date)) %>% 
    tidyr::complete(Date = seq(min(.data$Date, na.rm = T), max(.data$Date, na.rm = T), by = 'day'))
  
  if (!is.list(params[[1]]))
    params = list(params)
  
  sepraw = separate_cpp(lubridate::year(df[[1]]), 
               lubridate::month(df[[1]]),
               lubridate::day(df[[1]]),
               df[[2]], df[[3]], df[[4]],
               params, debug) 
  
  sep = dplyr::bind_cols(df, sepraw) %>%
    dplyr::rename(Date = 1, Q = 2, Temp = 3, Prec = 4) %>%
    dplyr::relocate(.data$Temp, .data$Prec, .after = dplyr::last_col()) %>%
    dplyr::mutate(dplyr::across(3:9, ~ replace(.x, .x < 0, NA)))
  
  if (debug) {
    
    attributes(sep)['jittered'] = attributes(sepraw)['jittered'];
    attributes(sep)['params'] = attributes(sepraw)['params'];
    names(attributes(sep)$params) = unique(lubridate::year(sep[[1]]))
    
    problem_years = setdiff(unique(lubridate::year(sep$Date)), unique(sep$Year))
    if (length(problem_years) > 0)
      warning(crayon::white$bgBlue$bold('grwat:'), ' ',
              crayon::white$italic(paste(problem_years, collapse = ', ')),
              ' years were not separated. Check the input data for possible errors. Use ', 
              crayon::cyan$italic('gr_get_gaps()'), ' and ', crayon::cyan$italic('gr_fill_gaps()'), 
              ' functions to detect and fill missing data.')
    
    jittered_years = attributes(sep)$jittered
    if (length(jittered_years) > 0)
      warning(crayon::white$bgBlue$bold('grwat:'), ' ',
              crayon::white$italic(paste(jittered_years, collapse = ', ')),
              ' years were processed with jittered parameters')
  }
  
  return(sep)
}

#' Extract baseflow
#' 
#' Extract baseflow from hydrological series using the filtering approach
#'
#' @param Q Numeric runoff vector.
#' @param padding Integer number of elements padded at the beginning and ending of runoff vector to reduce boundary effects. Defaults to `30`.
#' @param passes Integer number of filtering iterations. The first iteration is forward, second is backward, third is forward and so on. Defaults to `3`.
#' @param method Character string to set baseflow filtering method. Available methods are `'boughton'`, `'chapman'`, `'furey'`, `'jakeman'`, `'lynehollick'` and `'maxwell'`. Default is `'lynehollick'`, which corresponds to Lyne-Hollick (1979) hydrograph separation method.
#' @param a Numeric value of a filtering parameter used in `'chapman'`, `'jakeman'` and `'lynehollick'` methods. Defaults to `0.925`.
#' @param k Numeric value of a filtering parameter used in `'boughton'` and `'maxwell'` methods. Defaults to `0.975`.
#' @param C Numeric value of a separation shape parameter used in `'boughton'`, `'jakeman'` and `'maxwell'` methods
#' @param aq Numeric value of a filtering parameter used in `'jakeman'` method. Defaults to `-0.5`.
#'
#' @return Numeric baseflow vector
#' @export
#'
#' @example inst/examples/gr_baseflow.R
#' 
gr_baseflow <- function(Q, a = 0.925, k = 0.975, C = 0.05, aq = -0.5, 
                        passes = 3, padding = 30, method = 'lynehollick') {
  get_baseflow_cpp(Q, a, k, C, aq, passes, padding, method)
}

#' Get hydrograph separation parameters
#' 
#' The function returns the list of parameters that can be used by [grwat::gr_separate()]. Since the parameters are region-specific, the location must be selected. It can be identified by region name or geographic coordinates. If both are specified, then region have a higher priority
#'
#' @param reg Character string — the name of the region. Defaults to `'Midplain'`.
#' @param lon Numeric value of the longitude. Ignored if `reg` is specified.
#' @param lat Numeric value of the latitude. Ignored if `reg` is specified.
#'
#' @return List of separation parameters that can be used in [grwat::gr_separate()]  function.
#' @export
#'
#' @example inst/examples/gr_get_params.R
#' 
gr_get_params <- function(reg = 'Midplain', lon = NULL, lat = NULL) {
  params_in %>% 
    dplyr::filter(.data$region == reg) %>% 
    dplyr::select(-1) %>%
    as.list()
}

#' Get the information about parameters used to separate the hydrograph
#'
#' @return `data.frame` with description of hydrograph separation parameters that are used in [grwat::gr_separate()] .
#' @export
#'
#' @example inst/examples/gr_help_params.R
#' 
gr_help_params <- function() {
  return(params_in_desc)
}


#' Set the value of hydrograph separation parameter
#' 
#' The value is set for selected years in parameter list. Such list is returned by [grwat::gr_separate()]  with `debug = TRUE` set.
#'
#' @param params `list` of `list`s of hydrograph separation parameters as returned in `params` attribute by [grwat::gr_separate()]  with `debug = TRUE`.
#' @param p Name of the parameter.
#' @param value Numeric value to set.
#' @param years Integer vector of years to modify. Defaults to `NULL`, which means that all years will be modified.
#'
#' @return `list` of `list`s — a modified version of `params`
#' @export
#'
#' @example inst/examples/gr_set_param.R
#' 
gr_set_param <- function(params, p, value, years = NULL) {
  par = as.character(substitute(p))
  
  if (is.null(years)) {
    for (i in seq_along(params))
      params[[i]][[par]] = value
  } else {
    for (year in years)
      params[[as.character(year)]][[par]] = value
  }
  
  return(params)
}