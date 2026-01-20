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
      "Validate Model & Evaluate",
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
        # Passing content directly (Correct syntax)
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
          shinycssloaders::withSpinner(
            leaflet::leafletOutput(ns("map_OCSGE"), height = "100%"),
            type = 6,
            color = "#208a8b"
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
    # 1. DATA RETRIEVAL
    # --------------------------
    sf_ocsge_ROI_reactive <- reactive({
      # 1. check the data exist and are validated
      req(shared_global$sf_ROI)
      req(shared_global$data_validated)

      # 2. init output sf
      sf_ocsge <- NULL
      ref_ocsge <- spacemodR::ref_ocsge

      # Gestion du cas "Exemple" (Metaleurop) - Pas d'appel API
      if (!is.null(shared_global$try) && shared_global$try == "metaleurop") {
        sf_ocsge <- DATA_TRY[["ocsge_metaleurop"]]
      } else{
        # Cas Normal : Appel API Géoportail avec barre de progression
        shiny::withProgress(
          message = 'Contacting Geoportail API...',
          detail = 'Downloading OCS-GE data (this may take a minute)...',
          value = 0.5,
          {
            tryCatch({
              sf_ocsge <- spacemodR::get_ocsge_data(shared_global$sf_ROI)
              sf_ocsge
            }, error = function(e) {
              showNotification(paste("API Error:", e$message), type = "error", duration = 10)
              return(NULL)
            })
          }
        )
      }
      # 4. Jointure avec la référence (Si on a récupéré des données)
      if (!is.null(sf_ocsge)) {
        sf_ocsge <- merge(sf_ocsge, ref_ocsge, by = "code_cs", all.x = TRUE)
        # Gestion des codes inconnus (pour éviter que Leaflet ne plante sur des NA)
        sf_ocsge$couleur[is.na(sf_ocsge$couleur)] <- "#808080" # Gris par défaut
        sf_ocsge$nomenclature[is.na(sf_ocsge$nomenclature)] <- "Unknown"
      }
      return(sf_ocsge)
    })

    # Mise à jour de l'objet global une fois les données reçues
    observeEvent(sf_ocsge_ROI_reactive(), {
      shared_global$sf_ocsge_ROI <- sf_ocsge_ROI_reactive()
    })

    # --------------------------
    # 2. OUTPUTS (Summary & Map)
    # --------------------------
    output$ocsge_summary <- renderPrint({
      req(sf_ocsge_ROI_reactive())
      summary(sf_ocsge_ROI_reactive())
    })

    output$map_OCSGE <- leaflet::renderLeaflet({
      req(sf_ocsge_ROI_reactive())
      data_map <- sf_ocsge_ROI_reactive()

      # Si malgré tout les colonnes manquent (cas rare), on évite le crash
      if (!"couleur" %in% names(data_map)) data_map$couleur <- "#0073B7"
      if (!"nomenclature" %in% names(data_map)) data_map$nomenclature <- "N/A"

      # Sécurité WGS84
      if (sf::st_crs(data_map)$epsg != 4326) {
        data_map <- sf::st_transform(data_map, 4326)
      }
      # --- PRÉPARATION DE LA LÉGENDE ---
      df_legend <- sf::st_drop_geometry(data_map)
      df_legend <- unique(df_legend[, c("nomenclature", "couleur")])
      df_legend <- df_legend[!is.na(df_legend$nomenclature), ]
      df_legend <- df_legend[order(df_legend$nomenclature), ]

      # --- CARTE ---
      leaflet::leaflet(data_map) |>
        leaflet::addTiles() |>
        leaflet::addPolygons(
          # COULEUR DYNAMIQUE ICI :
          fillColor = ~couleur,        # Utilise la colonne HEX pour le fond
          color = "white",             # Bordure blanche pour séparer les zones
          weight = 1,
          opacity = 1,
          fillOpacity = 0.7,
          # Popup amélioré avec du HTML simple (gras)
          popup = ~paste("<b>Code:</b>", as.character(code_cs),
                         "<br><b>Libelle:</b>", as.character(nomenclature))
        ) |>
        # AJOUT DE LA LÉGENDE ICI :
        leaflet::addLegend(
          position = "bottomright",
          colors = df_legend$couleur,      # La liste des couleurs
          labels = df_legend$nomenclature, # La liste des noms correspondants
          title = "OCS-GE",
          opacity = 1
        )
    })


    # --------------------------
    # 3. HABITATS MANAGEMENT (ADD/REMOVE TABLE)
    # --------------------------

    # A. Mise à jour des choix de codes CS
    observeEvent(sf_ocsge_ROI_reactive(), {
      data <- sf_ocsge_ROI_reactive()
      req(data)
      if ("code_cs" %in% names(data)) {
        codes <- sort(unique(data$code_cs))
        # On peut ajouter la nomenclature pour aider : "CS 1.1 (Bati)"
        # Ici on reste simple
        updateSelectizeInput(session, "new_hab_codes", choices = codes, server = TRUE)
      }
    })

    # B. Stockage réactif des habitats
    # Structure: data.frame(Name, Codes_List)
    # Note: On stocke les codes sous forme de string concaténé pour l'affichage,
    # mais on gardera la liste réelle pour le traitement si besoin.
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
      # On concatène les codes avec une virgule pour l'affichage tableau
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
      # Suppression (attention aux indices si trié, mais ici rownames=FALSE et pas de tri par défaut)
      habitats_store$table <- habitats_store$table[-selected_idx, , drop = FALSE]
    })

    # E. Rendu du tableau
    output$table_habitats <- DT::renderDT({
      DT::datatable(
        habitats_store$table,
        selection = 'single', # Une seule ligne sélectionnable à la fois
        options = list(
          dom = 't', # Juste la table
          paging = FALSE,
          scrollY = "200px",
          scrollCollapse = TRUE
        ),
        rownames = FALSE
      )
    })

    # --------------------------
    # 4. VALIDATION & NAVIGATION
    # --------------------------
    go_next <- reactiveVal(FALSE)

    observeEvent(input$submit_btn, {
      # Validation : Il faut au moins un habitat
      if (nrow(habitats_store$table) == 0) {
        showNotification("Please create at least one habitat.", type = "error")
        return()
      }

      # Construction de l'objet final de configuration
      # On doit "dé-concaténer" les codes pour refaire une liste propre
      habitats_config <- list()
      for(i in seq_len(nrow(habitats_store$table))) {
        h_name <- habitats_store$table$Name[i]
        h_codes_str <- habitats_store$table$Codes[i]
        # strsplit renvoie une liste, on prend le 1er element
        h_codes <- trimws(unlist(strsplit(h_codes_str, ",")))

        habitats_config[[h_name]] <- h_codes
      }

      # Sauvegarde
      shared_global$habitats_config <- habitats_config
      message("Habitats Saved: ", length(habitats_config))

      # Validation Modèle externe (si existe)
      if (exists("validate_model") && !validate_model(input, shared_global)) {
        # showNotification...
        return()
      }
      go_next(TRUE)
    })

    return(go_next)
  })
}
################################################################################
