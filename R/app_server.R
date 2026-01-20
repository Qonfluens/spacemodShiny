#' @title Server Logic for the Shiny Application
#' @export
app_server <- function(input, output, session) {

  # Objet partagé entre les modules
  shared_global <- reactiveValues()

  # Définition de l'ordre des étapes du workflow
  steps <- c("tab_data", "tab_model", "tab_trophic", "tab_evaluate", "tab_risk")
  # Onglets toujours accessibles
  free_tabs <- c("tab_credit")

  # 1. État de navigation (Verrous)
  # TRUE = Onglet accessible / débloqué
  # FALSE = Onglet verrouillé
  nav_state <- reactiveValues(
    tab_data     = TRUE,  # La 1ère étape est toujours ouverte
    tab_model    = FALSE,
    tab_trophic  = FALSE, # Note: correspond à votre nouvelle page Trophic
    tab_evaluate = FALSE, # Exposure
    tab_risk     = FALSE  # Risk
  )

  # -----------------------------
  # 2. Appel des Modules
  # -----------------------------
  # On récupère le signal de validation (go_next) dans une variable 'outcome_*'

  # A. Data -> Valide et ouvre Model
  outcome_data <- mod_data_server("mod_data", shared_global)

  # B. Model (Habitat) -> Valide et ouvre Trophic
  outcome_model <- mod_model_server("mod_model", shared_global)

  # C. Trophic -> Valide et ouvre Evaluate
  outcome_trophic <- mod_trophic_server("mod_trophic", shared_global)

  # D. Evaluate / Risk -> (Pas de suite pour l'instant)
  mod_evaluate_server("mod_evaluate", shared_global)
  mod_evaluate_server("mod_risk", shared_global) # Si mod_risk utilise le même server

  # -----------------------------
  # 3. Logique de Workflow (Chaînage)
  # -----------------------------

  # Quand DATA est validé -> Débloquer et aller à MODEL
  observeEvent(outcome_data(), ignoreInit = TRUE, ignoreNULL = TRUE, {
    if(outcome_data() == TRUE) {
      nav_state$tab_model <- TRUE
      updateNavbarPage(session, "main_navbar", "tab_model")
    }
  })

  # Quand MODEL est validé -> Débloquer et aller à TROPHIC
  observeEvent(outcome_model(), ignoreInit = TRUE, ignoreNULL = TRUE, {
    if(outcome_model() == TRUE) {
      nav_state$tab_trophic <- TRUE
      updateNavbarPage(session, "main_navbar", "tab_trophic")
    }
  })

  # Quand TROPHIC est validé -> Débloquer et aller à EVALUATE (Exposure)
  observeEvent(outcome_trophic(), ignoreInit = TRUE, ignoreNULL = TRUE, {
    if(outcome_trophic() == TRUE) {
      nav_state$tab_evaluate <- TRUE
      # On débloque aussi Risk si c'est lié, ou on attend une autre validation
      nav_state$tab_risk <- TRUE
      updateNavbarPage(session, "main_navbar", "tab_evaluate")
    }
  })

  # -----------------------------
  # 4. Guard (Protection de navigation)
  # -----------------------------
  observeEvent(input$main_navbar, ignoreInit = TRUE, {
    req(input$main_navbar)

    # Si c'est un onglet libre (Crédits), on laisse passer
    if (input$main_navbar %in% free_tabs) return()

    # Si l'onglet n'est pas dans notre liste 'steps', on ignore
    if (!input$main_navbar %in% steps) return()

    # Vérification : L'onglet est-il débloqué dans nav_state ?
    is_unlocked <- nav_state[[input$main_navbar]]

    if (!isTRUE(is_unlocked)) {
      # CAS BLOQUANT : L'utilisateur essaie de tricher ou clique trop vite

      # 1. Identifier le dernier onglet valide pour le renvoyer là-bas
      # On parcourt les étapes dans l'ordre pour trouver la dernière TRUE
      valid_steps <- steps[sapply(steps, function(x) isTRUE(nav_state[[x]]))]
      target_tab <- tail(valid_steps, 1) # Le dernier valide

      # 2. Message d'erreur explicite
      showNotification(
        ui = span(icon("lock"), " Veuillez valider l'étape précédente avant d'accéder à celle-ci."),
        type = "error",
        duration = 4
      )

      # 3. Renvoi forcé vers l'onglet autorisé
      updateNavbarPage(session, "main_navbar", target_tab)
    }
  })
}
