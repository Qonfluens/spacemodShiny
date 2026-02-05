#' app_ui
#'
#' @return the user interface of the app
#'
#' @export
app_ui <- function(request) {

  # --- Gestion des ressources statiques (CSS/Images) ---
  path_www <- system.file("app/www", package = "spacemodShiny")
  if (path_www == "") {
    path_www <- file.path(getwd(), "inst", "app", "www")
  }
  if (dir.exists(path_www)) {
    shiny::addResourcePath(prefix = "assets", directoryPath = path_www)
  }
  # -------------------------------------------------------------------
  ui <- tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "assets/custom.css")
    ),
    bslib::page_navbar(
      title = span(
        # On peut ajouter un logo ici
        # tags$img(src = "www/logo.png", height = "30px", style = "margin-right: 10px;"),
        "SPACEMOD"
      ),
      id = "main_navbar",
      # --- 1. THÈME ---
      # "Zephyr" ou "Yeti" sont souvent plus "clean" pour la science que "Journal"
      # Mais on garde votre couleur primaire Teal (#208a8b)
      theme = bslib::bs_theme(
        bootswatch = "zephyr",
        primary="#208a8b",
        "navbar-bg" = "#208a8b", # Barre de nav colorée (plus moderne)
        "navbar-light-color" = "#ffffff", # Texte blanc sur la navbar
        font_scale = 0.9 # reduce all font size by 10%
      ),
      # --- 2. REMPLISSAGE (CRUCIAL) ---
      # Permet aux cartes (Map) de prendre toute la hauteur disponible
      fillable = TRUE,
      # Onglet sélectionné par défaut
      selected = "tab_data",
      # --- 3. ONGLETS DU WORKFLOW ---
      bslib::nav_panel(
        title = "Data",
        value = "tab_data",
        icon = icon("database"), # Icône explicite
        mod_data_ui("mod_data")
      ),
      bslib::nav_panel(
        title = "Habitat",
        value = "tab_model",
        icon = icon("layer-group"), # Icône de couches/habitats
        mod_model_ui("mod_model")
      ),
      bslib::nav_panel(
        title = "Trophic",
        value = "tab_trophic",
        icon = icon("diagram-project"), # Trophic
        mod_trophic_ui("mod_trophic")
      ),
      bslib::nav_panel(
        title = "Exposure",
        value = "tab_evaluate",
        icon = icon("magnifying-glass-chart"), # Exposure
        mod_evaluate_ui("mod_evaluate")
      ),
      bslib::nav_panel(
        title = "Risk",
        value = "tab_risk",
        icon = icon("triangle-exclamation"), # Alerte/Risque
        mod_evaluate_ui("mod_risk")
      ),
      # --- 4. SÉPARATEUR ---
      # Pousse les éléments suivants tout à droite
      bslib::nav_spacer(),
      # --- 5. INFO / CREDITS ---
      bslib::nav_item(
        # Utiliser nav_item pour un lien ou nav_panel pour une page
        # Ici je garde nav_panel comme vous aviez
        tags$a(
          icon("github"),
          "Source Code",
          href = "https://github.com/Qonfluens",
          target = "_blank",
          class = "nav-link"
        )
      ),
      bslib::nav_panel(
        title = "Credits",
        value = "tab_credit",
        icon = icon("circle-info"),
        mod_credit_ui("mod_credit")
      )
    )
  )
  return(ui)
}

