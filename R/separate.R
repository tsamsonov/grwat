# This is a technical function to update grwat core from a separate project
update_core <- function() {
  file.copy('../grwat-core/grwat_core.cpp', 'src/grwat_core.cpp', overwrite = TRUE)
}

#' Separates river hydrograph
#'
#' @param df data frame with four columns: date, discharge, temperature, precipitation
#' @param params list of separation parameters, as returned by \link{gr_get_params()} function
#' @param alpha smoothing parameter for groundwater separation algorithm, defaults to 0.925 for Lyne-Hollick method
#' @param niter number of iterations
#'
#' @return
#' @export
#'
#' @examples
gr_separate <- function(df, params = gr_get_params(), alpha = 0.925, niter = 100) {
  
  if (length(df) != 4)
    stop(crayon::white$bold('grwat:'), ' the number of columns in data frame (', length(df), 
         ') is not equal to 4')
  
  if (!lubridate::is.Date(df[[1]]) || !is.numeric(df[[2]]) || !is.numeric(df[[3]]) || !is.numeric(df[[4]]))
    stop(crayon::white$bold('grwat:'), ' the columns of input data frame must of Date, numeric, numeric and numeric data types')
  
  separate_cpp(lubridate::year(df[[1]]), 
               lubridate::month(df[[1]]),
               lubridate::day(df[[1]]),
               df[[2]], df[[3]], df[[4]],
               params, niter, alpha) %>% 
    bind_cols(df, .) %>% 
    relocate(Temp, Prec, .after = last_col())
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