# This is a technical function to update grwat core from a separate project
update_core <- function() {
  file.copy('../grwat-core/grwat_core.cpp', 'src/grwat_core.cpp', overwrite = TRUE)
}

#' Separates river hydrograph
#'
#' @param df data frame with six columns: year, month, date, discharge, temperature, precipitation
#' @param params list of separation parameters
#' @param order order, in which the columns are placed in data frame. Defaults to 'dmyqtp'
#'
#' @return
#' @export
#'
#' @examples
gr_separate <- function(df, params = grw_get_params(), cols = 'dmyqtp', niter = 100) {
  
  if (nchar(cols) != length(df))
    stop(crayon::white$bold('grwat:'), ' the number of columns in data frame (', length(df), ') is not equal to the length of `col` parameter (', nchar(cols), ')')
  
  col_vec = strsplit(cols, '')[[1]]
  
  if (length(intersect(col_vec, c('q', 't', 'p'))) != 3)
    stop(crayon::white$bold('grwat:'), ' the `col` parameter does not contain all of qtp values')
  
  if (length(intersect(col_vec, c('d', 'm', 'y'))) == 3) {
    y = df[[get_idx(cols, 'y')]]
    m = df[[get_idx(cols, 'm')]]
    d = df[[get_idx(cols, 'd')]]
  } else if (grepl('D', cols)) {
    y = lubridate::year(df[[get_idx(cols, 'D')]])
    m = lubridate::month(df[[get_idx(cols, 'D')]])
    d = lubridate::day(df[[get_idx(cols, 'D')]])
  } else {
    stop(crayon::white$bold('grwat:'), ' the `col` parameter must contain either date (D) or separate day, month and year columns (dmy) in any order')
  }
  
  separate_cpp(y, m, d,
               df[[get_idx(cols, 'q')]],
               df[[get_idx(cols, 't')]],
               df[[get_idx(cols, 'p')]],
               params,
               niter) %>% 
    bind_cols(df) %>% 
    dplyr::mutate(Date = lubridate::make_date(Year, Month, Day)) %>% 
    select(Date, Qin = Q, Qbase, Quick, Qseas, Qrain, Qthaw, Qpb, Qtype, Temp, Prec)
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