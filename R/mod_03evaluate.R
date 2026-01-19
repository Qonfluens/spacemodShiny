#' UI for module evaluate
#' @export
mod_evaluate_ui <- function(id) {
  ns <- NS(id)
  bslib::accordion(
    id="data_accordion",
    multiple=FALSE,
    open="Plot",
    bslib::accordion_panel(
      full_screen = TRUE,
      title = "Plot",
      selectInput(ns('select_distribution'), "Distribution", choices = NULL),
      # plotly::plotlyOutput(ns("plot_output"))
    )
  )
}

#' Server of module evaluate
#' @export
mod_evaluate_server <- function(id, shared_global) {
  moduleServer(id, function(input, output, session) {


  })
}
