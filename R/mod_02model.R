#' UI for Model Module
#' @export
mod_model_ui <- function(id) {
  ns <- NS(id)

  # Sidebar Content (Configuration)
  sidebar_content <- list(
    h4("Model Configuration"),

    # --- Section 1: Input Form (Master) ---
    tags$strong("1. Define a New Habitat"),
    tags$div(
      class = "bg-light p-3 rounded border",
      textInput(
        ns("new_hab_name"),
        label = NULL,
        placeholder = "Habitat Name (e.g., Forest)"
      ),
      selectizeInput(
        ns("new_hab_codes"),
        label = NULL,
        choices = NULL,
        multiple = TRUE,
        options = list(placeholder = "Select CS Codes...")
      ),
      actionButton(
        ns("add_hab_btn"),
        "Add to List",
        icon = icon("plus"),
        class = "btn-success w-100"
      )
    ),

    tags$br(),

    # --- Section 2: List Table (Detail) ---
    div(
      class = "d-flex justify-content-between align-items-center mb-1",
      tags$strong("2. Created Habitats"),
      # Small delete button aligned to the right
      actionButton(
        ns("remove_hab_btn"),
        "Remove",
        class = "btn-outline-danger btn-sm py-0",
        icon = icon("trash")
      )
    ),
    # DT Table (Compact view)
    DT::DTOutput(ns("table_habitats")),

    tags$br(),
    tags$hr(),

    # --- Section 4: Main Action ---
    actionButton(
      ns("submit_btn"),
      "Validate Habitat & Go Trophic Setting",
      class = "btn-primary w-100",
      icon = icon("check-double")
    )
  )

  # Main Layout
  bslib::page_fillable(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 400,
        title = "Settings",
        sidebar_content
      ),

      # Map Card - Takes full remaining space
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          "Habitats Visualization",
          bslib::tooltip(
            icon("circle-info"),
            "Map showing OCS-GE polygons colored by CS code."
          )
        ),
        bslib::card_body(
          class = "p-0", # Full bleed map
          # spinner autour de l'output
          shinycssloaders::withSpinner(
            leaflet::leafletOutput(ns("map_OCSGE"), height = "100%"),
            type = 8,          # Type 8 = Cercles qui tournent (moderne et discret)
            color = "#208a8b", # Votre couleur "Teal" primaire
            size = 0.5         # Taille modérée pour ne pas être agressif
          )
        )
      )
    )
  )
}

#' Server of module model
#' @export
mod_model_server <- function(id, shared_global) {
  shiny::moduleServer(id, function(input, output, session) {

      # --------------------------
      # 1. DATA RETRIEVAL (FULL FGB VERSION)
      # --------------------------
      sf_ocsge_ROI_reactive <- reactive({
        # 1. Vérifications initiales
        req(shared_global$sf_ROI)
        req(shared_global$data_validated)

        sf_ocsge <- NULL

        # Référence pour les codes couleurs/noms (Essentiel !)
        ref_ocsge <- spacemodR::ref_ocsge

        # 2. Récupération des données
        # Cas "Exemple" (Metaleurop)
        if (!is.null(shared_global$try) && shared_global$try == "metaleurop") {
          sf_ocsge <- DATA_TRY[["ocsge_metaleurop"]]
        } else {
          # Cas Normal : Lecture optimisée du FlatGeobuf sur S3
          shiny::withProgress(
            message = 'Loading OCS-GE Data...',
            detail = 'Connect to Qonfluens API (may take > 1 minute) ...',
            value = 0.3,
            {
              tryCatch({
                # Configuration GDAL pour la robustesse réseau
                Sys.setenv(
                  GDAL_HTTP_TIMEOUT = "30",
                  GDAL_HTTP_MAX_RETRY = "3",
                  GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR"
                )

                url_fgb <- "https://spacemod-ocsge.s3.fr-par.scw.cloud/ocsge_france.fgb"
                vsicurl_url <- paste0("/vsicurl/", url_fgb)

                # Filtre spatial (Projection Lambert 93 / 2154 pour l'interrogation)
                roi_filter <- shared_global$sf_ROI
                if (sf::st_crs(roi_filter)$epsg != 2154) {
                  roi_filter <- sf::st_transform(roi_filter, 2154)
                }
                wkt_geom <- sf::st_as_text(sf::st_geometry(roi_filter))

                # Lecture partielle via GDAL
                sf_subset <- sf::read_sf(vsicurl_url, wkt_filter = wkt_geom)

                if (nrow(sf_subset) > 0) {
                  # Découpage propre selon le ROI exact
                  sf_ocsge <- sf::st_intersection(sf_subset, roi_filter)
                }

                sf_ocsge

              }, error = function(e) {
                showNotification(paste("Erreur API/FGB:", e$message), type = "error", duration = 10)
                return(NULL)
              })
            }
          )
        }

        # 3. Nettoyage et Jointure (Si des données ont été trouvées)
        if (!is.null(sf_ocsge) && nrow(sf_ocsge) > 0) {

          # Normalisation des noms de colonnes (parfois en majuscules dans le FGB)
          col_names <- names(sf_ocsge)
          if ("CODE_CS" %in% col_names && !"code_cs" %in% col_names) {
            names(sf_ocsge)[names(sf_ocsge) == "CODE_CS"] <- "code_cs"
          }

          # Jointure avec la table de référence (pour avoir 'couleur' et 'nomenclature')
          # On ne le fait que si 'code_cs' existe
          if ("code_cs" %in% names(sf_ocsge)) {
            sf_ocsge <- merge(sf_ocsge, ref_ocsge, by = "code_cs", all.x = TRUE)
          }

          # --- LE BOUT DE CODE QUE VOUS VOULIEZ RAJOUTER ---
          # Gestion des codes inconnus / manquants
          if ("couleur" %in% names(sf_ocsge)) {
            sf_ocsge$couleur[is.na(sf_ocsge$couleur)] <- "#808080" # Gris par défaut
          }
          if ("nomenclature" %in% names(sf_ocsge)) {
            sf_ocsge$nomenclature[is.na(sf_ocsge$nomenclature)] <- "Unknown"
          }
        }

        return(sf_ocsge)
      })

      # Mise à jour de l'objet global une fois les données reçues
      observeEvent(sf_ocsge_ROI_reactive(), {
        # On stocke le résultat propre dans shared_global
        shared_global$sf_ocsge_ROI <- sf_ocsge_ROI_reactive()
      })

      # --------------------------
      # 2. OUTPUTS (Summary & Map)
      # --------------------------

      # Résumé textuel
      output$ocsge_summary <- renderPrint({
        req(sf_ocsge_ROI_reactive())
        summary(sf_ocsge_ROI_reactive())
      })

      # Carte Leaflet (Version classique sans JS externe)
      output$map_OCSGE <- leaflet::renderLeaflet({
        req(sf_ocsge_ROI_reactive())

        # Récupération des données nettoyées
        data_map <- sf_ocsge_ROI_reactive()

        # Sécurité : Si vide, on ne plante pas, on renvoie une carte vide
        if (nrow(data_map) == 0) return(leaflet::leaflet() |> leaflet::addTiles())

        # Transformation WGS84 pour l'affichage Leaflet
        if (sf::st_crs(data_map)$epsg != 4326) {
          data_map <- sf::st_transform(data_map, 4326)
        }

        # Préparation de la légende
        # (On utilise data_map qui contient maintenant les couleurs grâce au merge ci-dessus)
        df_legend <- sf::st_drop_geometry(data_map)

        # Sécurité si merge raté
        if (!"couleur" %in% names(df_legend)) df_legend$couleur <- "#0073B7"
        if (!"nomenclature" %in% names(df_legend)) df_legend$nomenclature <- "N/A"

        df_legend <- unique(df_legend[, c("nomenclature", "couleur")])
        df_legend <- df_legend[!is.na(df_legend$nomenclature), ]
        df_legend <- df_legend[order(df_legend$nomenclature), ]

        # Construction de la carte
        leaflet::leaflet(data_map) |>
          leaflet::addTiles() |>
          leaflet::addPolygons(
            fillColor = ~couleur,
            color = "white",
            weight = 0.5,
            opacity = 1,
            fillOpacity = 0.7,
            popup = ~paste("<b>Code:</b>", as.character(code_cs),
                           "<br><b>Libelle:</b>", as.character(nomenclature))
          ) |>
          leaflet::addLegend(
            position = "bottomright",
            colors = df_legend$couleur,
            labels = df_legend$nomenclature,
            title = "OCS-GE",
            opacity = 1
          )
      })

    # 3. HABITATS MANAGEMENT (ADD/REMOVE TABLE)
    # --------------------------
    # A. Mise à jour des choix de codes CS avec Nomenclature
    observeEvent(sf_ocsge_ROI_reactive(), {
      data <- sf_ocsge_ROI_reactive()

      # Si data est NULL ou vide (0 lignes), on ne fait RIEN.
      req(data)
      if (nrow(data) == 0) return()
      # ----------------------------------

      # Vérification des colonnes nécessaires
      if ("code_cs" %in% names(data) && "nomenclature" %in% names(data)) {

        # ... Reste de votre code inchangé ...
        df_unique <- unique(sf::st_drop_geometry(data)[, c("code_cs", "nomenclature")])
        df_unique <- df_unique[order(df_unique$code_cs), ]

        # C'est ici que ça plantait avant :
        labels <- paste(df_unique$code_cs, "-", df_unique$nomenclature)
        choices_vec <- setNames(df_unique$code_cs, labels)

        updateSelectizeInput(
          session,
          "new_hab_codes",
          choices = choices_vec, server = TRUE)
      }  else if ("code_cs" %in% names(data)) {
        # Fallback si pas de nomenclature : on affiche juste les codes
        codes <- sort(unique(data$code_cs))
        updateSelectizeInput(session, "new_hab_codes", choices = codes, server = TRUE)
      }
    })

    # B. Stockage réactif des habitats
    habitats_store <- reactiveValues(table = data.frame(
      Name = character(),
      Codes = character(),
      stringsAsFactors = FALSE
    ))

    # C. Ajout (Bouton +)
    observeEvent(input$add_hab_btn, {
      req(input$new_hab_name, input$new_hab_codes)

      # Vérification doublon nom
      if (input$new_hab_name %in% habitats_store$table$Name) {
        showNotification("Habitat name already exists.", type = "warning")
        return()
      }

      # Création nouvelle ligne
      codes_str <- paste(input$new_hab_codes, collapse = ", ")

      new_row <- data.frame(
        Name = input$new_hab_name,
        Codes = codes_str,
        stringsAsFactors = FALSE
      )

      habitats_store$table <- rbind(habitats_store$table, new_row)

      # Reset des champs
      updateTextInput(session, "new_hab_name", value = "")
      updateSelectizeInput(session, "new_hab_codes", selected = character(0))
    })

    # D. Suppression (Bouton Remove)
    observeEvent(input$remove_hab_btn, {
      selected_idx <- input$table_habitats_rows_selected
      if (is.null(selected_idx)) {
        showNotification("Select a row to remove.", type = "warning")
        return()
      }
      habitats_store$table <- habitats_store$table[-selected_idx, , drop = FALSE]
    })

    # E. Rendu du tableau
    output$table_habitats <- DT::renderDT({
      DT::datatable(
        habitats_store$table,
        selection = 'single',
        options = list(
          dom = 't',
          paging = FALSE,
          scrollY = "200px",
          scrollCollapse = TRUE
        ),
        rownames = FALSE
      )
    })

    # --------------------------
    # 3a. SPATIAL DATA PREPARATION (For SpacemodR & Map)
    # --------------------------

    # Ce reactive construit la liste des objets SF pour chaque habitat défini
    habitats_spatial_list <- reactive({
      req(sf_ocsge_ROI_reactive())
      # On dépend de la table des habitats
      req(nrow(habitats_store$table) > 0)

      all_data <- sf_ocsge_ROI_reactive()
      habitats_def <- habitats_store$table

      # Initialisation de la liste
      spatial_list <- list()

      # Si la colonne code_cs n'existe pas, on arrête
      if (!"code_cs" %in% names(all_data)) return(list())

      # Boucle sur chaque habitat défini par l'utilisateur
      for (i in seq_len(nrow(habitats_def))) {
        hab_name <- habitats_def$Name[i]
        hab_codes_str <- habitats_def$Codes[i]

        # Parsing des codes
        target_codes <- trimws(unlist(strsplit(hab_codes_str, ",")))

        # Création du subset (Conservant le CRS d'origine, ex: 2154)
        subset_data <- all_data[all_data$code_cs %in% target_codes, ]

        # On stocke dans la liste nommée
        if (nrow(subset_data) > 0) {
          spatial_list[[hab_name]] <- subset_data
        }
      }

      return(spatial_list)
    })

    # --------------------------
    # 3b. INTERACTIVE MAP HIGHLIGHT
    # --------------------------
    observeEvent(input$table_habitats_rows_selected, {
      proxy <- leaflet::leafletProxy("map_OCSGE")
      proxy |> leaflet::clearGroup("highlight_layer")

      selected_idx <- input$table_habitats_rows_selected
      if (is.null(selected_idx)) return()

      # 1. Récupération du Nom de l'habitat sélectionné
      selected_row <- habitats_store$table[selected_idx, ]
      hab_name <- selected_row$Name

      # 2. Récupération des données spatiales pré-calculées
      spatial_list <- habitats_spatial_list()

      # Vérifie si cet habitat a bien des données géographiques associées
      if (!is.null(spatial_list[[hab_name]])) {

        subset_visu <- spatial_list[[hab_name]]

        # 3. Reprojection à la volée UNIQUEMENT pour l'affichage (WGS84)
        if (sf::st_crs(subset_visu)$epsg != 4326) {
          subset_visu <- sf::st_transform(subset_visu, 4326)
        }

        # 4. Affichage
        proxy |> leaflet::addPolygons(
          data = subset_visu,
          group = "highlight_layer",
          color = "#FF0000",
          weight = 3,
          opacity = 1,
          fillColor = "#FF0000",
          fillOpacity = 0.6,
          popup = ~paste("<b>Habitat:</b>", hab_name, "<br><b>Code:</b>", code_cs)
        )
      } else {
        showNotification(paste("No geometry found for habitat:", hab_name), type="warning")
      }

    }, ignoreNULL = FALSE)

    # --------------------------
    # 4. VALIDATION & NAVIGATION
    # --------------------------
    go_next <- reactiveVal(FALSE)

    observeEvent(input$submit_btn, {
      if (nrow(habitats_store$table) == 0) {
        showNotification("Please create at least one habitat.", type = "error")
        return()
      }

      # Récupération de la liste spatiale (CRS origine, ex: 2154)
      spatial_list <- habitats_spatial_list()

      # On récupère aussi le Raster et le ROI pour spacemodR
      # (Supposons que vous les ayez dans shared_global)
      # raster_obj <- shared_global$raster_ground
      # roi_obj <- shared_global$sf_ROI

      # Liste finale pour spacemodR
      spacemodr_habitats <- list()

      # --- CONSTRUCTION SPACEMODR ---
      tryCatch({
        for (hab_name in names(spatial_list)) {

          layer_hab <- spatial_list[[hab_name]]

           # Votre logique spacemodR :
          # On crée un objet habitat vide puis on ajoute les couches
          # Note: Assurez-vous que spacemodR::habitat() est bien la fonction constructeur
          # h_obj <- spacemodR::habitat() |>
          #   spacemodR::add_habitat(layer_hab) |>
          #   spacemodR::add_nohabitat(layer_soil_artificial) # Si besoin

          # Pour l'instant, on stocke juste le layer positif dans la config
          spacemodr_habitats[[hab_name]] <- spacemodR::habitat() |>
                 spacemodR::add_habitat(layer_hab)
        }

        # Sauvegarde dans shared_global pour l'étape suivante
        shared_global$spacemodr_habitats <- spacemodr_habitats

        # Sauvegarde de la config textuelle (pour le module Trophic)
        habitats_config_names <- list()
        for(i in seq_len(nrow(habitats_store$table))) {
          habitats_config_names[[habitats_store$table$Name[i]]] <- habitats_store$table$Codes[i]
        }
        shared_global$habitats_config <- habitats_config_names

        message("SpacemodR objects prepared: ", length(spacemodr_habitats))
        go_next(TRUE)

      }, error = function(e) {
        showNotification(paste("Error building spacemodR objects:", e$message), type="error")
      })
    })

    return(go_next)
  })
}
