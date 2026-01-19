#' Run the Shiny Application
#'
#' @import shiny
#' @import bslib
#' @importFrom shiny tagList
#'
#' @return A shiny app object
#' @export
shiny_app <- function() {
  shiny::shinyApp(
    ui = app_ui,
    server = app_server
  )
}

#' Launch the Shiny app and expose distribution functions
#'
#' This wraps the app with necessary initialization (e.g. exporting d*/p*/q*/r* functions)
#'
#' @param ... Passed to shiny::runApp()
#' @export
run_app <- function(...) {
  shiny::runApp(shiny_app(), ...)
}
