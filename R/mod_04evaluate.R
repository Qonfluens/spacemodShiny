#' UI for Evaluation/Exposure Module
#' @export
mod_evaluate_ui <- function(id) {
  ns <- NS(id)

  # Sidebar Content
  sidebar_content <- list(
    h4("Exposure Assessment"),

    # --- 1. Execution ---
    tags$div(
      class = "bg-light p-3 rounded border",
      tags$p("Click below to combine Habitats, Trophic Links, and Ground Concentration into a Space Model."),
      actionButton(
        ns("run_model_btn"),
        "Run Exposure Model",
        icon = icon("play"),
        class = "btn-primary w-100"
      )
    ),

    tags$hr(),

    # --- 2. Visualization Control ---
    # Only shown when model is computed
    uiOutput(ns("viz_controls")),

    tags$hr(),

    # --- 3. Statistics ---
    uiOutput(ns("layer_stats"))
  )

  # Main Layout
  bslib::page_fillable(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 400,
        title = "Model Outputs",
        sidebar_content
      ),

      # Visualization Card
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          "Spatial Distribution",
          bslib::tooltip(
            icon("circle-info"),
            "Interactive map showing the computed raster layers. Use the selector in the sidebar to switch layers."
          )
        ),
        bslib::card_body(
          class = "p-0",
          leaflet::leafletOutput(ns("map_evaluate"), height = "100%")
        )
      )
    )
  )
}

#' Server for Evaluation Module
#' @export
mod_evaluate_server <- function(id, shared_global) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # --------------------------
    # 1. RUN MODEL (COMPUTATION)
    # --------------------------

    # Reactive to store the resulting SpatRaster Stack
    result_stack <- reactiveVal(NULL)

    observeEvent(input$run_model_btn, {
      # Data Check
      req(shared_global$spacemodr_habitats) # From Step 2 (Model)
      req(shared_global$trophic_data)       # From Step 3 (Trophic)
      req(shared_global$raster_ground)      # From Step 1 (Data)

      shiny::withProgress(message = "Computing Space Model...", value = 0.2, {

        tryCatch({
          # A. Prepare Ground Data
          # We use the raster uploaded in Step 1
          ground_cd <- shared_global$raster_ground

          # B. Build Habitat Stack
          # We iterate over the spacemodR objects prepared in Step 2
          incProgress(0.2, detail = "Stacking habitats...")

          # Note: We assume 'habitat_raster' is available in your package/env
          stack_habitat_list <- lapply(shared_global$spacemodr_habitats, function(hab) {
            res <- spacemodR::habitat_raster(ground_cd, hab)
            return(res)
          })

          # Convert list of rasters to a single SpatRaster stack if necessary
          # If habitat_raster returns a SpatRaster, we can create a collection
          # For this example, we assume spacemodR::spacemodel handles the list or we stack it here.
          # If spacemodR::spacemodel expects a list, keep as is. If it expects a SpatRaster:
          # stack_habitat <- terra::rast(stack_habitat_list)

          # Based on your prompt, we pass the list:
          stack_habitat <- spacemodR::raster_stack(stack_habitat_list)

          # C. Build Trophic Table
          incProgress(0.2, detail = "Processing trophic links...")
          trophic_df <- spacemodR::trophic(
            shared_global$trophic_data,
            from = "From", to = "To", weight = "Weight"
          )

          # D. Run Space Model
          incProgress(0.3, detail = "Running model...")
          # spcmdl_habitat result is a terra SpatRaster
          spcmdl_habitat <- spacemodR::spacemodel(stack_habitat, trophic_df)

          # Store result
          result_stack(spcmdl_habitat)

          showNotification("Model computed successfully!", type = "message")

        }, error = function(e) {
          showNotification(paste("Model Error:", e$message), type = "error", duration = 10)
        })
      })
    })

    # --------------------------
    # 2. UI UPDATES
    # --------------------------

    # Render the Layer Selector only when data is ready
    output$viz_controls <- renderUI({
      req(result_stack())
      r_stack <- result_stack()

      # Get layer names from the terra object
      layer_names <- names(r_stack)

      tagList(
        tags$strong("Select Layer to Display:"),
        selectInput(
          ns("layer_selector"),
          label = NULL,
          choices = layer_names,
          selected = layer_names[1]
        )
      )
    })

    # --------------------------
    # 3. MAP RENDERING
    # --------------------------

    # Base Map
    output$map_evaluate <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addTiles() |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)
    })

    # Update Map when Layer Changes
    observe({
      req(result_stack(), input$layer_selector)

      r_stack <- result_stack()
      selected_name <- input$layer_selector

      # Check if name exists
      if (!selected_name %in% names(r_stack)) return()

      # 1. Extract specific layer
      r_layer <- r_stack[[selected_name]]

      # 2. Stats for Sidebar
      mm <- terra::minmax(r_layer, compute = TRUE)
      val_min <- round(min(mm), 3)
      val_max <- round(max(mm), 3)

      output$layer_stats <- renderUI({
        tags$div(
          class = "alert alert-info",
          tags$strong(icon("chart-bar"), " Layer Statistics"),
          tags$ul(
            tags$li(paste("Layer:", selected_name)),
            tags$li(paste("Min Value:", val_min)),
            tags$li(paste("Max Value:", val_max))
          )
        )
      })

      # 3. Reproject for Leaflet (WGS84)
      r_visu <- terra::project(r_layer, "EPSG:4326")
      bbox <- as.vector(terra::ext(r_visu))

      # --- AJOUT 1 : Préparation du ROI (Contour) ---
      sf_roi_visu <- NULL
      if (!is.null(shared_global$sf_ROI)) {
        sf_roi_visu <- shared_global$sf_ROI
        # Sécurité projection WGS84
        if (sf::st_crs(sf_roi_visu)$epsg != 4326) {
          sf_roi_visu <- sf::st_transform(sf_roi_visu, 4326)
        }
      }

      # --- AJOUT 2 : Calcul de l'emprise pour le Zoom ---
      # terra::ext renvoie un vecteur c(xmin, xmax, ymin, ymax)
      bbox <- terra::ext(r_visu)

      # 4. Color Palette
      pal <- leaflet::colorNumeric(
        palette = "viridis",
        domain = c(val_min, val_max),
        na.color = "transparent"
      )

      # 5. Update Map via Proxy
      proxy <- leaflet::leafletProxy("map_evaluate") |>
        leaflet::clearImages() |>
        leaflet::clearControls() |>
        leaflet::clearShapes() # Important : on nettoie aussi les polygones précédents (ROI)

      # A. Ajout du Raster
      proxy |> leaflet::addRasterImage(
        r_visu,
        colors = pal,
        opacity = 0.7,
        group = "Model Output",
        maxBytes = 8 * 1024 * 1024
      )

      # B. Ajout du ROI (Cadre uniquement)
      if (!is.null(sf_roi_visu)) {
        proxy |> leaflet::addPolygons(
          data = sf_roi_visu,
          fill = FALSE,       # Pas de remplissage
          color = "white",    # Couleur du contour (blanc ressort bien sur le raster)
          weight = 2,         # Épaisseur du trait
          opacity = 1,
          group = "ROI"
        )
      }

      # C. Légende et Zoom
      proxy |>
        leaflet::addLegend(
          pal = pal,
          values = terra::values(r_visu),
          title = selected_name,
          position = "bottomright"
        ) |>
        # Zoom sur l'étendue du raster (xmin, ymin, xmax, ymax)
        leaflet::fitBounds(lng1 = bbox[1], lat1 = bbox[3], lng2 = bbox[2], lat2 = bbox[4])
    })

    # --------------------------
    # 4. VALIDATION / NEXT STEP
    # --------------------------
    # If there is a "Risk" tab after this, you can unlock it here
    # For now, we just validate the current step

    go_next <- reactiveVal(FALSE)

    observeEvent(result_stack(), {
      # Once model is run, we consider this step "done" or ready for risk assessment
      shared_global$evaluation_results <- result_stack()
      go_next(TRUE)
    })

    return(go_next)
  })
}
