#' app_ui
#'
#' @return the user interface of the app
#'
#' @export
app_ui <- function(request) {
  ui <- tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    bslib::page_navbar(
      title = "SSD workflow",
      id = "main_navbar",
      theme = bslib::bs_theme(bootswatch = "journal", , primary="#208a8b"),
      selected = "tab_data",
      bslib::nav_panel(
        title = "Data",
        value = "tab_data",
        mod_data_ui("mod_data")
      ),
      bslib::nav_panel(
        title = "Model",
        value = "tab_model",
        mod_model_ui("mod_model")
      ),
      bslib::nav_panel(
        title = "Evaluate",
        value = "tab_evaluate",
        mod_evaluate_ui("mod_evaluate")
      ),
      bslib::nav_panel(
        title = "Credits",
        value = "tab_credit",
        mod_credit_ui("mod_credit")
      )
    )
  )
  return(ui)
}

