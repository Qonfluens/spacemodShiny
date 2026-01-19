#' @title Server Logic for the Shiny Application
#'
#' @description
#' This function defines the server-side logic of the Shiny application and is passed
#' as the `server` argument to `shinyApp()`. It initializes a shared `reactiveValues()`
#' object to allow communication between modules and calls each module's server function.
#'
#' @param input Shiny input object (automatically provided by Shiny).
#' @param output Shiny output object (automatically provided by Shiny).
#' @param session Shiny session object (automatically provided by Shiny).
#'
#' @export
#'
app_server <- function(input, output, session) {
  shared_global <- reactiveValues()

  # Workflow steps
  steps <- c("tab_data", "tab_model", "tab_evaluate")
  free_tabs <- c("tab_credit")

  # Initial nav_state
  nav_state <- reactiveValues(
    tab_data     = FALSE,
    tab_model    = FALSE,
    tab_evaluate = FALSE
  )

  # Modules
  go_model <- mod_data_server("mod_data", shared_global)
  go_evaluate <- mod_model_server("mod_model", shared_global)
  mod_evaluate_server("mod_evaluate", shared_global)

  # Observe go_next() signals
  observeEvent(go_model(), ignoreInit = TRUE, ignoreNULL = TRUE, {
    nav_state$tab_model <- TRUE
    # Passer automatiquement à Model après validation de Data
    updateNavbarPage(session, "main_navbar", "tab_model")
  })

  observeEvent(go_evaluate(), ignoreInit = TRUE, ignoreNULL = TRUE, {
    nav_state$tab_evaluate <- TRUE
    # Passer automatiquement à Evaluate après validation de Model
    updateNavbarPage(session, "main_navbar", "tab_evaluate")
  })

  # -----------------------------
  # Guard de navigation
  # -----------------------------
  observeEvent(input$main_navbar, ignoreInit = TRUE, {

    cat("Navbar clicked:", input$main_navbar, "\n")
    cat("Nav state:", paste(names(nav_state), unlist(reactiveValuesToList(nav_state))), "\n")

    # Onglets libres
    if (input$main_navbar %in% free_tabs) return()

    # Index dans steps
    idx <- match(input$main_navbar, steps)
    if (is.na(idx)) return()

    # Vérifier si l'étape est validée
    current_state <- reactiveValuesToList(nav_state)[[input$main_navbar]]
    if (!isTRUE(current_state)) {
      # Revenir au dernier onglet validé
      last_valid <- rev(steps[1:(idx-1)])
      last_valid <- last_valid[sapply(last_valid, function(x) isTRUE(nav_state[[x]]))]
      target <- if (length(last_valid) > 0) last_valid[1] else steps[1]
      updateNavbarPage(session, "main_navbar", target)
    }
  })

  # For debug
  observe({
    cat("DEBUG nav_state:", paste(names(nav_state), unlist(reactiveValuesToList(nav_state))), "\n")
  })
}


