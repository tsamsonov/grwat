# This is a technical function to update grwat core from a separate project
update_core <- function() {
  file.copy('../grwat-core/grwat_core.cpp', 
            'src/grwat_core.cpp', overwrite = TRUE)
}

#' Check the correctness of data frame for separating
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
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
}

#' Check the correctness of parameters list for separating
#'
#' @param params `list` of separation parameters, as returned by [grwat::gr_get_params()] function
#' @param df `data.frame` or `tibble` with four columns: date, discharge, temperature, precipitation
#'
#' @return stops the execution if anything is wrong and prints the exact reason of the error. Otherwise prins the message that everything is OK
#' @export
#'
#' @examples 
gr_check_params <- function(df, params) {
  
  template = gr_get_params()
  
  n = 1
  listed = FALSE
  
  if (is.list(params[[1]])) {
    n = length(params)
    listed = TRUE
    
    if (length(unique(lubridate::year(df[[1]]))) != n) {
      stop(crayon::white$bgRed$bold('grwat:'), ' ',
           crayon::white$italic(' the length of parameters list must be equal to 1 or to the number of years in the data'))
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
  
  
  
  message(crayon::white$bold('grwat: '), ' parameters list and types are OK')
    
}

#' Advanced hydrograph separation
#' 
#' Separates the hydrograph into genetic components: groundwater, thaw, flood and seasonal (freshet) flood.
#'
#' @param df `data.frame` or `tibble` with four columns: date, discharge, temperature, precipitation
#' @param params `list` of separation parameters, as returned by [grwat::gr_get_params()] function
#'
#' @return A `data.frame` with 12 columns: 
#' 
#' | __Column__ | __Description__ |
#' | ------ | ----------- |
#' | `Date`   | date |
#' | `Q`      | total discharge |
#' | `Qbase`  | baseflow |
#' | `Quick`  | quick flow |
#' | `Qseas`  | seasonal/freshet flow |
#' | `Qrain`  | rain floods | 
#' | `Qthaw`  | thaw floods |
#' | `Qpb`    | mountain floods | 
#' | `Type`   | a combination of discharge types |
#' | `Year`   | a water-resources year |
#' | `Temp`   | temperature |
#' | `Prec`   | precipitation |
#' 
#' @export
#'
#' @example inst/examples/gr_separate.R
gr_separate <- function(df, params = gr_get_params(), debug = FALSE) {
  
  gr_check_data(df)
  
  gr_check_params(df, params)
  
  df = df %>% 
    dplyr::rename(Date = 1) %>% 
    dplyr::filter(!is.na(Date)) %>% 
    tidyr::complete(Date = seq(min(Date, na.rm = T), max(Date, na.rm = T), by = 'day'))
  
  if (!is.list(params[[1]]))
    params = list(params)
  
  sepraw = separate_cpp(lubridate::year(df[[1]]), 
               lubridate::month(df[[1]]),
               lubridate::day(df[[1]]),
               df[[2]], df[[3]], df[[4]],
               params, debug) 
  
  sep = sepraw %>%
    dplyr::bind_cols(df, .) %>%
    dplyr::rename(Date = 1, Q = 2, Temp = 3, Prec = 4) %>%
    dplyr::relocate(Temp, Prec, .after = dplyr::last_col()) %>%
    dplyr::mutate(dplyr::across(3:10, ~ replace(.x, .x < 0, NA)))
  
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

#' Extract baseflow from hydrological series
#'
#' @param Q daily discharge vector 
#' @param alpha filtering parameter. Defaults to 0.925
#' @param padding number of elements padded at the beginning and ending of discharge vector to reduce boundary effects
#' @param passes number of filtering iterations. The first iteration is forward, second is backward, third is forward and so on. Defaults to 3
#' @param method baseflow filtering method. Available methods are "maxwell", "boughton", "jakeman", "lynehollick", "chapman". Default is "lynehollick", which corresponds to Lyne-Hollick (1979) hydrograph separation method.
#'
#' @return baseflow vector
#' @export
#'
#' @examples
gr_baseflow <- function(Q, a = 0.925, k = 0.975, C = 0.05, aq = -0.5, 
                        passes = 3, padding = 30, method = 'lynehollick') {
  get_baseflow_cpp(Q, a, k, C, aq, passes, padding, method)
}

#' Get default separation parameters for selected region
#'
#' @param reg Name of the region
#'
#' @return
#' @export
#'
#' @examples
gr_get_params <- function(reg = 'Midplain') {
  params_in %>% 
    dplyr::filter(region == reg) %>% 
    dplyr::select(-1) %>%
    as.list()
}

#' Get the information about parameters used to separate the hydrograph
#'
#' @return a table with parameter names
#' @export
#'
#' @examples
gr_help_params <- function() {
  return(params_in_desc)
}


#' Set the value of selected parameter for selected years in parameter list
#'
#' @param params List of lists as returned in `params` attribute by `gr_separate()` when `debug = TRUE`.
#' @param p Name of the parameter.
#' @param value Value to set.
#' @param years Integer vector of years to modify. Defaults to NULL, which means that all years will be modified
#'
#' @return List of lists — a modified version of `params`
#' @export
#'
#' @examples
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