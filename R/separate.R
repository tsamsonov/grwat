#' Separates river hydrograph
#'
#' @param df data frame with six columns: year, month, date, discharge, temperature, precipitation
#' @param params list of separation parameters
#'
#' @return
#' @export
#'
#' @examples
separate <- function(df, params) {
  return(df)
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
      h3("Set hydrograph separation parameters"),
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