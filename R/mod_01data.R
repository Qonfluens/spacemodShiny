#' @title Data Module UI
#' @export
mod_data_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_column_wrap(
    width = 1,
    heights_equal="row",
    bslib::layout_columns(
      col_widths = c(3, 9),
      bslib::card(
        full_screen = TRUE,
        bslib::accordion(
          id="data_accordion",
          multiple=FALSE,
          bslib::accordion_panel(
            title = "Examples",
            selectInput(ns('try'), "Try with a dataset", choices = c(NA, "metaleurop"))
          ),
          bslib::accordion_panel(
            title = "Upload Ground",
            fileInput(
              ns("file_raster"),
              "Choose a raster file (GeoTIFF, .tif, .grd, etc.)",
              accept = c(".tif", ".tiff", ".grd"))
          ),
          bslib::accordion_panel(
            title = "Upload ROI",
            # NOTE: Pour les shapefiles (.shp), l'utilisateur doit uploader un ZIP ou
            # vous devez gérer le multi-upload. GeoJSON/GPKG sont préférables ici.
            fileInput(
              ns("file_sf"),
              "Choose a Shapefile GeoJSON or GPKG file",
              accept = c("shp", ".geojson", ".gpkg"))
          )
        )
      ),
      bslib::layout_column_wrap(
        width = 1,
        heights_equal = "row",
        bslib::card(
          title = "Data specification",
          full_screen = TRUE,
          bslib::card(
            title = "Map of Data",
            # Utilisation de leafletOutput standard
            leaflet::leafletOutput(ns("map"), height = "600px")
          )
        )
      )
    ),
    actionButton(ns("submit_btn"), "Submit & Go to Model", class = "btn-primary")
  )
}

#' Serveur du module Data
#' @export
mod_data_server <- function(id, shared_global) {
  shiny::moduleServer(id, function(input, output, session) {

    # ---------------
    # 0. STATE MANAGEMENT
    # ---------------
    # On stocke les données brutes (EPSG:2154 ou autre)
    raw_raster <- reactiveVal(NULL)
    raw_roi    <- reactiveVal(NULL)

    # ---------------
    # 1. TRY EXAMPLE
    # ---------------
    observeEvent(input$try, {
      req(input$try)

      if (input$try == "metaleurop") {

        # 1. Gestion du ROI (SF)
        # ----------------------
        roi_ex <- DATA_TRY[["roi_metaleurop"]]

        # Sécurité CRS
        if (sf::st_crs(roi_ex)$epsg != 2154) {
          roi_ex <- sf::st_transform(roi_ex, 2154)
        }
        raw_roi(roi_ex)

        # 2. Gestion du Raster (Terra) - LA CORRECTION EST ICI
        # ----------------------
        # On appelle la fonction qui charge le fichier "à neuf"
        # Plus besoin de deepcopy car on vient de le créer
        r_ex <- DATA_TRY$get_ground_cd()

        # Sécurité CRS
        crs_code <- terra::crs(r_ex, describe = TRUE)$code
        if (!is.na(crs_code) && crs_code != "2154") {
          r_ex <- terra::project(r_ex, "EPSG:2154")
        }

        raw_raster(r_ex)
      }
    })


    # ---------------
    # 2. LOAD DATA HANDLERS
    # ---------------

    # --- Upload Raster ---
    observeEvent(input$file_raster, {
      req(input$file_raster)
      tryCatch({
        r <- terra::rast(input$file_raster$datapath)

        # Gestion CRS
        crs_r <- terra::crs(r, describe = TRUE)
        if (is.na(crs_r$code)) { # Parfois epsg est vide, on vérifie le code/authority
          # Warning ou gestion à faire ici si pas de CRS
        }

        # Projection forcée en 2154 si besoin pour le traitement interne
        # (Optionnel selon votre logique métier, ici je garde votre logique)
        if (!is.na(crs_r$code) && crs_r$code != "2154") {
          showNotification("Reprojection raster vers EPSG:2154...", type = "warning", duration = 3)
          r <- terra::project(r, "EPSG:2154")
        }
        raw_raster(r)
      }, error = function(e) {
        showNotification(paste("Error raster:", e$message), type = "error")
      })
    })

    # --- Upload SF ---
    observeEvent(input$file_sf, {
      req(input$file_sf)
      tryCatch({
        # Attention: st_read sur un .shp sans .shx/.dbf échouera.
        # GeoJSON/GPKG sont OK.
        sf_obj <- sf::st_read(input$file_sf$datapath, quiet = TRUE)

        if (sf::st_crs(sf_obj)$epsg != 2154) {
          showNotification("Reprojection SF vers EPSG:2154...", type = "warning", duration = 3)
          sf_obj <- sf::st_transform(sf_obj, 2154)
        }
        raw_roi(sf_obj)
      }, error = function(e) {
        showNotification(paste("Error SF:", e$message), type = "error")
      })
    })

    # ---------------
    # 3. PREPARE DATA FOR MAP (WGS84)
    # ---------------
    # C'est ici l'amélioration clé : on prépare les données pour la carte
    # DANS des reactives distincts, pas dans le renderLeaflet.

    map_raster <- reactive({
      req(raw_raster())
      r <- raw_raster()
      # Leaflet a besoin de Web Mercator ou WGS84 (EPSG:4326 est le plus simple pour terra::project)
      # On utilise method="near" pour les classes ou "bilinear" pour le continu
      terra::project(r, "EPSG:4326")
    })

    map_roi <- reactive({
      req(raw_roi())
      sf::st_transform(raw_roi(), 4326)
    })

    # ---------------
    # 4. LEAFLET RENDERING
    # ---------------
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addTiles() |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)
    })

    # -----------------------------
    # RENDERING MAP (FINAL)
    # -----------------------------
    observe({
      # Dépendances réactives
      r_visu <- tryCatch(map_raster(), error=function(e) NULL)
      sf_visu <- tryCatch(map_roi(), error=function(e) NULL)

      proxy <- leaflet::leafletProxy("map")

      # Nettoyage
      proxy |>
        leaflet::clearShapes() |>
        leaflet::clearImages() |>
        leaflet::clearControls()

      bbox_global <- c()

      # --- A. Layer Raster ---
      if (!is.null(r_visu)) {

        # 1. Calcul des bornes EXACTES pour la palette (Fix du warning)
        # compute=TRUE force le calcul si les stats ne sont pas encore connues
        mm <- terra::minmax(r_visu, compute = TRUE)
        domain_range <- c(min(mm), max(mm))

        # 2. Échantillon pour la distribution (optionnel) ou la légende
        vals_sample <- terra::spatSample(r_visu, size = 10000, method="regular", as.points=FALSE, na.rm=TRUE)
        vals_vec <- unlist(vals_sample)
        vals_vec <- vals_vec[is.finite(vals_vec)]

        if (length(vals_vec) > 0) {
          # On définit le domaine sur les bornes réelles (mm), pas sur l'échantillon
          pal <- leaflet::colorNumeric("viridis", domain = domain_range, na.color = "transparent")

          proxy |> leaflet::addRasterImage(
            r_visu,
            colors = pal,
            opacity = 0.7,
            group = "Raster",
            maxBytes = 8 * 1024 * 1024
          ) |>
            leaflet::addLegend(
              pal = pal,
              # Pour la légende, on peut passer soit l'échantillon, soit le range.
              # Passer l'échantillon aide Leaflet à placer les ticks intelligemment.
              values = vals_vec,
              title = "Values",
              position = "bottomright"
            )
        }

        # Gestion BBox Raster
        ext <- as.vector(terra::ext(r_visu))
        bbox_r <- c(ext[1], ext[3], ext[2], ext[4])
        bbox_global <- rbind(bbox_global, bbox_r)
      }

      # --- B. Layer SF ---
      if (!is.null(sf_visu)) {
        proxy |> leaflet::addPolygons(
          data = sf_visu,
          color = "#0073B7",
          weight = 2,
          fillOpacity = 0.2,
          group = "ROI",
          popup = "ROI"
        )
        bbox_s <- as.numeric(sf::st_bbox(sf_visu))
        bbox_global <- rbind(bbox_global, bbox_s)
      }

      # --- C. Fit Bounds ---
      if (!is.null(bbox_global)) {
        final_xmin <- min(bbox_global[,1])
        final_ymin <- min(bbox_global[,2])
        final_xmax <- max(bbox_global[,3])
        final_ymax <- max(bbox_global[,4])

        if (is.finite(final_xmin) && is.finite(final_xmax)) {
          proxy |> leaflet::fitBounds(final_xmin, final_ymin, final_xmax, final_ymax)
        }
      }
    })

    # ---------------
    # 5. VALIDATION & NAVIGATION
    # ---------------
    # Mise à jour des objets partagés
    observeEvent(raw_roi(), { shared_global$sf_ROI <- raw_roi() })
    observeEvent(raw_raster(), { shared_global$raster_ground <- raw_raster() })

    go_next <- reactiveVal(FALSE)

    observeEvent(input$submit_btn, {
      # Validation simple intégrée ici pour l'exemple
      has_roi <- !is.null(shared_global$sf_ROI)
      has_raster <- !is.null(shared_global$raster_ground)

      if (!has_roi) {
        showNotification("Please select a ROI (Shapefile/GeoJSON).", type = "error")
      } else if (!has_raster) {
        showNotification("Please select a Raster file.", type = "warning")
        # Warning seulement si le raster n'est pas obligatoire
      } else {
        shared_global$try <- input$try
        go_next(TRUE)
      }
    })

    return(go_next)
  })
}
