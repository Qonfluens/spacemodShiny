#' @title Data Module UI
#' @export
mod_data_ui <- function(id) {
  ns <- NS(id)

  # Sidebar Content (Configuration)
  sidebar_content <- list(
    h4("Data Source"),

    # 1. Mode Selection: Example vs. Import
    bslib::input_task_button(
      ns("dummy_btn"), # Visual hack or title
      label = "Configuration"
    ),
    radioButtons(
      ns("data_source_mode"),
      label = "Select source:",
      choices = c(
        "Use an example" = "example",
        "Upload my files" = "upload"
      ),
      selected = "example",
      inline = FALSE
    ),

    tags$hr(),

    # 2. Dynamic Input Zone (Managed server-side)
    uiOutput(ns("dynamic_inputs")),

    tags$hr(),

    # 3. Status Indicator (Visual Feedback)
    uiOutput(ns("status_feedback")),

    tags$br(),

    # 4. Validation Button
    actionButton(
      ns("submit_btn"),
      "Validate & Go to Next Step",
      class = "btn-primary w-100",
      icon = icon("arrow-right")
    )
  )

  # Main Layout: Sidebar + Map
  bslib::page_fillable(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 400,
        title = "Parameters",
        sidebar_content
      ),
      # The map takes up all remaining space
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          "Geographic Visualization",
          bslib::tooltip(
            icon("circle-info"),
            "Displays the Raster (Background) and the ROI (Blue outlines) once loaded."
          )
        ),
        bslib::card_body(
          class = "p-0", # Removes internal padding to fit edges
          leaflet::leafletOutput(ns("map"), height = "100%")
        )
      )
    )
  )
}

#' Server for Data Module
#' @export
mod_data_server <- function(id, shared_global) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns # Important for IDs in renderUI

    # ---------------
    # 0. STATE MANAGEMENT
    # ---------------
    raw_raster <- reactiveVal(NULL)
    raw_roi    <- reactiveVal(NULL)

    # ---------------
    # 1. DYNAMIC UI GENERATION
    # ---------------

    # A. Dynamic inputs based on selected mode
    output$dynamic_inputs <- renderUI({
      req(input$data_source_mode)

      if (input$data_source_mode == "example") {
        # --- EXAMPLE MODE ---
        tagList(
          tags$div(
            class = "alert alert-info",
            icon("info-circle"),
            "Load the 'Metaleurop' dataset to test the application."
          ),
          selectInput(
            ns('try'),
            "Choose a dataset:",
            choices = c("Select..." = "", "Metaleurop" = "metaleurop")
          )
        )
      } else {
        # --- UPLOAD MODE ---
        tagList(
          tags$div(
            class = "alert alert-light",
            style = "border: 1px solid #ddd;",
            tags$small("Import your own geographic data.")
          ),
          fileInput(
            ns("file_raster"),
            label = "1. Raster File (Concentration)",
            accept = c(".tif", ".tiff", ".grd"),
            placeholder = "Drag your GeoTIFF here"
          ),
          fileInput(
            ns("file_sf"),
            label = "2. ROI File (Study Area)",
            accept = c(".geojson", ".gpkg", ".shp"), # .shp needs .shx/.dbf, gpkg is better
            placeholder = "GeoJSON or GPKG recommended"
          )
        )
      }
    })

    # B. Status Feedback (Dynamic Value Box)
    output$status_feedback <- renderUI({
      # Check data state
      has_roi <- !is.null(raw_roi())
      has_rst <- !is.null(raw_raster())

      if (has_roi && has_rst) {
        bslib::value_box(
          title = "Status",
          value = "Ready to validate",
          theme = "success",
          showcase = icon("check"),
          p("Data loaded successfully.")
        )
      } else if (has_roi || has_rst) {
        bslib::value_box(
          title = "Status",
          value = "Incomplete",
          theme = "warning",
          showcase = icon("triangle-exclamation"),
          p("Missing Raster or ROI.")
        )
      } else {
        bslib::value_box(
          title = "Status",
          value = "Waiting",
          theme = "light",
          showcase = icon("database"),
          p("Please load data.")
        )
      }
    })

    # ---------------
    # 2. DATA LOAD HANDLERS
    # ---------------

    # --- Case 1: Try Example ---
    observeEvent(input$try, {
      req(input$try)
      if (input$try == "metaleurop") {
        # ROI
        roi_ex <- DATA_TRY[["roi_metaleurop"]]
        if (sf::st_crs(roi_ex)$epsg != 2154) roi_ex <- sf::st_transform(roi_ex, 2154)
        raw_roi(roi_ex)

        # Raster (Lazy Load to prevent pointer errors)
        r_ex <- DATA_TRY$get_ground_cd()
        crs_code <- terra::crs(r_ex, describe = TRUE)$code
        if (!is.na(crs_code) && crs_code != "2154") r_ex <- terra::project(r_ex, "EPSG:2154")
        raw_raster(r_ex)
      }
    })

    # --- Case 2: Upload Raster ---
    observeEvent(input$file_raster, {
      req(input$file_raster)
      tryCatch({
        r <- terra::rast(input$file_raster$datapath)
        crs_r <- terra::crs(r, describe = TRUE)
        # Force reprojection to EPSG:2154 if needed
        if (!is.na(crs_r$code) && crs_r$code != "2154") {
          showNotification("Reprojecting raster to EPSG:2154...", type = "warning")
          r <- terra::project(r, "EPSG:2154")
        }
        raw_raster(r)
      }, error = function(e) showNotification(paste("Raster Error:", e$message), type = "error"))
    })

    # --- Case 3: Upload SF ---
    observeEvent(input$file_sf, {
      req(input$file_sf)
      tryCatch({
        sf_obj <- sf::st_read(input$file_sf$datapath, quiet = TRUE)
        if (sf::st_crs(sf_obj)$epsg != 2154) {
          showNotification("Reprojecting SF to EPSG:2154...", type = "warning")
          sf_obj <- sf::st_transform(sf_obj, 2154)
        }
        raw_roi(sf_obj)
      }, error = function(e) showNotification(paste("SF Error:", e$message), type = "error"))
    })

    # ---------------
    # 3. MAP RENDERING
    # ---------------

    # Preparation for WGS84 (Distinct Reactive)
    map_raster <- reactive({
      req(raw_raster())
      terra::project(raw_raster(), "EPSG:4326")
    })

    map_roi <- reactive({
      req(raw_roi())
      sf::st_transform(raw_roi(), 4326)
    })

    # Base Map
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addTiles() |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)
    })

    # Update Map Observer
    observe({
      r_visu <- tryCatch(map_raster(), error=function(e) NULL)
      sf_visu <- tryCatch(map_roi(), error=function(e) NULL)

      proxy <- leaflet::leafletProxy("map")
      proxy |> leaflet::clearShapes() |> leaflet::clearImages() |> leaflet::clearControls()

      bbox_global <- c()

      # A. Raster Layer
      if (!is.null(r_visu)) {
        mm <- terra::minmax(r_visu, compute = TRUE)
        domain_range <- c(min(mm), max(mm))

        # Sample for legend distribution logic
        vals_sample <- terra::spatSample(r_visu, size = 10000, method="regular", as.points=FALSE, na.rm=TRUE)
        vals_vec <- unlist(vals_sample)
        vals_vec <- vals_vec[is.finite(vals_vec)]

        if (length(vals_vec) > 0) {
          pal <- leaflet::colorNumeric("viridis", domain = domain_range, na.color = "transparent")
          proxy |> leaflet::addRasterImage(
            r_visu, colors = pal, opacity = 0.7, group = "Raster", maxBytes = 8 * 1024 * 1024
          ) |>
            leaflet::addLegend(pal = pal, values = vals_vec, title = "Concentration", position = "bottomright")
        }

        ext <- as.vector(terra::ext(r_visu))
        bbox_global <- rbind(bbox_global, c(ext[1], ext[3], ext[2], ext[4]))
      }

      # B. SF Layer
      if (!is.null(sf_visu)) {
        proxy |> leaflet::addPolygons(
          data = sf_visu, color = "#0073B7", weight = 2, fillOpacity = 0.2, group = "ROI", popup = "Study Area"
        )
        bbox_global <- rbind(bbox_global, as.numeric(sf::st_bbox(sf_visu)))
      }

      # C. Fit Bounds
      if (!is.null(bbox_global)) {
        xmin <- min(bbox_global[,1]); ymin <- min(bbox_global[,2])
        xmax <- max(bbox_global[,3]); ymax <- max(bbox_global[,4])
        if (is.finite(xmin) && is.finite(xmax)) {
          proxy |> leaflet::fitBounds(xmin, ymin, xmax, ymax)
        }
      }
    })

    # ---------------
    # 4. VALIDATION
    # ---------------

    # Observers to store in shared global (without validation yet)
    observeEvent(raw_roi(), { shared_global$sf_ROI <- raw_roi() })
    observeEvent(raw_raster(), { shared_global$raster_ground <- raw_raster() })

    go_next <- reactiveVal(FALSE)

    observeEvent(input$submit_btn, {
      has_roi <- !is.null(shared_global$sf_ROI)
      has_raster <- !is.null(shared_global$raster_ground)

      if (!has_roi) {
        showNotification("Error: Please define a Region of Interest (ROI).", type = "error")
      } else if (!has_raster) {
        showNotification("Warning: No raster loaded.", type = "warning")
      } else {
        # If in example mode, save this info
        if (input$data_source_mode == "example") {
          shared_global$try <- input$try
        } else {
          shared_global$try <- NULL
        }

        # OFFICIAL VALIDATION -> TRIGGERS NEXT STEP
        shared_global$data_validated <- Sys.time()

        showNotification("Data validated! Proceeding to the next step...", type = "message")
        go_next(TRUE)
      }
    })

    return(go_next)
  })
}
