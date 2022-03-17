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
    stop(crayon::white$bgRed$bold('grwat:'), ' the number of columns in data frame (', length(df), ') is not equal to 4')
  
  if (!lubridate::is.Date(df[[1]]) || !is.numeric(df[[2]]) || !is.numeric(df[[3]]) || !is.numeric(df[[4]]))
    stop(crayon::white$bgRed$bold('grwat:'), ' the four columns of input data frame (date, discharge, temperature, precipitation) must be of ', 
         crayon::white$italic('Date, numeric, numeric, numeric'), ' data types')
  
  if (sum(df[[2]] < 0, na.rm = T) > 0)
    stop(crayon::white$bgRed$bold('grwat:'), ' there are negative values in discharge (2nd) column, please fix the data before proceeding')
  
  if (sum(df[[4]] < 0, na.rm = T) > 0)
    stop(crayon::white$bgRed$bold('grwat:'), ' there are negative values in precipitation (4th) column, please fix the data before proceeding')
}

#' Check the correctness of parameters list for separating
#'
#' @param params `list` of separation parameters, as returned by [grwat::gr_get_params()] function
#'
#' @return stops the execution if anything is wrong and prints the exact reason of the error. Otherwise prins the message that everything is OK
#' @export
#'
#' @examples 
gr_check_params <- function(params) {
  
  template = gr_get_params()
  
  d = setdiff(names(params), names(template))
  if (length(d) > 0) {
    stop(crayon::white$bgRed$bold('grwat:'), ' ',
         crayon::white$italic(paste(d, collapse = ',')), 
         ' parameter(s) not known. Please use ', crayon::cyan$italic('gr_get_params()'), ' result as a template.')
  }
  
  d = setdiff(names(template), names(params))
  if (length(d) > 0) {
    stop(crayon::white$bgRed$bold('grwat:'), ' ',
         crayon::white$italic(paste(d, collapse = ',')), 
         ' parameter(s) needed. Please use ', crayon::cyan$italic('gr_get_params()'), ' result as a template.')
  }
  
  d = intersect(names(template), names(params))
  types1 = sapply(params[d], class)
  types2 = sapply(template[d], class)
  
  td = which(types1 != types2)
  if(length(td) > 0) {
    stop(crayon::white$bgRed$bold('grwat:'), ' ',
         crayon::white$italic(paste(d[td], collapse = ',')), 
         ' parameter(s) must be of ',
         crayon::cyan$italic(paste(types2[td], collapse = ',')), 
         ' type(s) ')
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
gr_separate <- function(df, params = gr_get_params()) {
  
  gr_check_data(df)
  
  gr_check_params(params)
  
  df = df %>% 
    rename(Date = 1) %>% 
    dplyr::filter(!is.na(Date)) %>% 
    tidyr::complete(Date = seq(min(Date, na.rm = T), max(Date, na.rm = T), by = 'day'))
  
  sep = separate_cpp(lubridate::year(df[[1]]), 
               lubridate::month(df[[1]]),
               lubridate::day(df[[1]]),
               df[[2]], df[[3]], df[[4]],
               params) %>% 
    dplyr::bind_cols(df, .) %>% 
    dplyr::rename(Date = 1, Q = 2, Temp = 3, Prec = 4)  %>% 
    dplyr::relocate(Temp, Prec, .after = dplyr::last_col())
}

#' Extracts baseflow for discharge
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

#' Change separation parameters through graphical interface
#'
#' @param params Parameters list
#'
#' @return
#' @export
#'
#' @examples
gr_set_params <- function(params) {
  app = shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::h3("Set hydrograph separation parameters"),
      purrr::imap(params, function(value, param) {
        shiny::fluidRow(
          shiny::column(2, shiny::numericInput(param, label = NULL, value)),
          shiny::column(2, param)
        )
      }),
      shiny::actionButton("Set", "Set")
    ),
    server = function(input, output) {
      shiny::observeEvent(input$Set, {
        out = list(mome = input$mome,
                   grad = input$mome,
                   grad1 = input$grad1,
                   kdQgr1 = input$kdQgr1,
                   polmon1 = input$polmon1,
                   polmon2 = input$polmon2,
                   polkol1 = input$polkol1,
                   polkol2 = input$polkol2,
                   polkol3 = input$polkol3,
                   polgrad1 = input$polgrad1,
                   polgrad2 = input$polgrad2,
                   prodspada = input$prodspada,
                   nPav = input$nPav,
                   nZam = input$nZam,
                   nWin = input$nWin,
                   Pcr = input$Pcr,
                   Tcr1 = input$Tcr1,
                   Tzam = input$Tzam,
                   Twin = input$Twin,
                   SignDelta = input$SignDelta,
                   SignDelta1 = input$SignDelta1,
                   FlagGaps = input$FlagGaps,
                   PavRate = input$PavRate,
                   InterpolStep = input$InterpolStep,
                   Tcr2 = input$Tcr2,
                   gradabs = input$gradabs,
                   ModeMountain = input$ModeMountain,
                   pgrad = input$pgrad,
                   polkolMount1 = input$polkolMount1,
                   polkolMount2 = input$polkolMount2,
                   polgradMount = input$polgradMount)
        shiny::stopApp(out)
      }) 
    }
  )
  
  return(shiny::runApp(app))
}