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
separate <- function(df, params = grwat::get_separation_params(), niter = 100, cols = 'dmyqtp') {
  separate_cpp(df[[get_idx(cols, 'y')]], 
               df[[get_idx(cols, 'm')]],
               df[[get_idx(cols, 'd')]],
               df[[get_idx(cols, 'q')]],
               df[[get_idx(cols, 't')]],
               df[[get_idx(cols, 'p')]],
               params,
               niter) %>% 
    bind_cols(df) %>% 
    dplyr::mutate(Date = lubridate::make_date(Year, Month, Day)) %>% 
    select(Date, Qin = Q, Qbase, Quick, Qseas, Qrain, Qthaw, Qpb, Tin, Pin)
}

#' Extracts baseflow for discharge
#'
#' @param Q daily discharge vector 
#' @param alpha filtering parameter. Defaults to 0.925
#' @param padding number of elements padded at the beginning and ending of discharge vector to reduce boundary effects
#' @param passes number of filtering iterations. The first iteration is forward, second is backward, third is forward and so on. Defaults to 3
#' @param method baseflow filtering method. Currently only `method = LYNE` is supported which corresponds to Lyne-Hollick (1979)
#'
#' @return baseflow vector
#' @export
#'
#' @examples
get_baseflow <- function(Q, alpha = 0.925, padding = 30, passes = 3, method = 'Lyne-Hollick') {
  get_baseflow_cpp(Q, alpha, padding, passes, method)
}

#' Get default separation parameters for selected region
#'
#' @param region Name of the region
#'
#' @return
#' @export
#'
#' @examples
get_separation_params <- function(type = 'Midplain') {
  params_in %>% 
    dplyr::filter(region == type) %>% 
    dplyr::select(-1) %>%
    as.list()
}

#' Change separation parameters through graphical interface
#'
#' @param params Parameters list
#'
#' @return
#' @export
#'
#' @examples
set_separation_params <- function(params) {
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