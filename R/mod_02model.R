#' UI for the module model
#'
#' @return the user interface of the model part
#'
#' @export
mod_model_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_column_wrap(
    width = 1,
    heights_equal="row",
    bslib::layout_columns(
      col_widths = c(3, 9),
      heights_equal = "row",
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Setting Model"),
        uiOutput(ns("loading_message")),
        shinycssloaders::withSpinner(
          verbatimTextOutput(ns("ocsge_summary")),
          type = 6
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Plot"),
        bslib::card_body(
          title = "Region/Area of Interest",
          mainPanel(
            leaflet::leafletOutput(ns("map_OCSGE"), height = 600)
          )
        )
      )
    ),
    actionButton(ns("submit_btn"), "Go to Evalutation", class = "btn-primary"),
  )
}

#' Server of module model
#' @export
mod_model_server <- function(id, shared_global) {
  shiny::moduleServer(id, function(input, output, session) {

    last_ocsge <- reactiveVal(NULL)

    observeEvent(shared_global$try, {
      req(shared_global$try)
      if(shared_global$try=="metaleurop"){
        last_ocsge(DATA_TRY[["ocsge_metaleurop"]])
      }
    })

    # Reactive to compute sf_ocsge_ROI when sf_ROI changes
    sf_ocsge_ROI_reactive <- reactive({
      req(shared_global$sf_ROI)
      # Spinner will appear automatically if used in withSpinner
      sf_ocsge <- spacemodR::get_ocsge_data(shared_global$sf_ROI)
      # Store result in shared_global for other modules
      shared_global$sf_ocsge_ROI <- sf_ocsge
      sf_ocsge
    })

    # Output the SF ROI summary
    output$sf_summary <- renderPrint({
      req(shared_global$sf_ROI)
      st_geometry_type(shared_global$sf_ROI)
    })

    # Output the computed OCS-GE summary
    output$loading_message <- renderUI({
      if (is.null(sf_ocsge_ROI_reactive())) {
        tags$div(
          style = "color: #555; font-style: italic; margin-bottom: 10px;",
          "Loading OCS-GE resources, may take up to 2 minutes. Please wait..."
        )
      }
    })
    output$ocsge_summary <- renderPrint({
      req(sf_ocsge_ROI_reactive())
      summary(sf_ocsge_ROI_reactive())
    })

    output$map_OCSGE <- leaflet::renderLeaflet({
      req(sf_ocsge_ROI_reactive())

      leaflet::leaflet(sf_ocsge_ROI_reactive()) |>
        leaflet::addTiles() |>
        leaflet::addPolygons(
          color = "#0073B7",
          weight = 2,
          opacity = 0.8,
          fillOpacity = 0.5,
          popup = ~as.character(rownames(sf_ocsge_ROI_reactive()))
        )
    })

    # GO TO EVALUATE
    # --------------------------
    # SIGNAL DE VALIDATION (GO NEXT)
    # --------------------------
    go_next <- reactiveVal(FALSE)
    observe({
      cat("mod_model go_next =", go_next(), "\n")
    })
    observeEvent(input$submit_btn, {
      # UTILISE LE HELPER validate_data()
      if (!validate_model(input, shared_global)) {
        showNotification("Require a fit object. Click Run and/or Wait the run succeed.",
                         type = "error")
        return()
      }
      # signal “ok, on peut passer à l’étape suivante”
      go_next(TRUE)
    })
    return(go_next)

    return(reactive(input$submit_btn))

  })
}
