#' @title UI for Credits Module
#'
#' @description
#' A module to display project credits, context, and references for SPACEMOD.
#'
#' @param id Module namespace ID
#'
#' @export
mod_credit_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_column_wrap(
    width = 1,
    heights_equal = "row",
    bslib::card(
      title = "About the Project",
      full_screen = TRUE,

      # --- Section 1: The Scientific Project ---
      h3("The SPACEMOD Project"),
      p("The SPACEMOD project (SPAtially explicit MODels for exposure and impact assessment of contaminants in terrestrial ecosystems)
        aims to improve ecotoxicological risk assessment by explicitly integrating the spatial dimension
        and landscape connectivity."),
      p("In traditional risk assessments, the environment is often treated in a simplified or homogeneous manner.
        SPACEMOD proposes a spatialized modeling approach that accounts for:"),
      tags$ul(
        tags$li("Land use and cover (specifically via OCS-GE data) to identify pressure sources."),
        tags$li("The structure of the trophic network and landscape connectivity."),
        tags$li("The transfer of contaminants from source ares to terrestrial organisms.")
      ),
      p("The goal is to provide tools to predict organism exposure and understand how
        landscape structure influences ecosystem vulnerability."),

      # --- Section 2: Technical Tools ---
      h3("The R Package: spacemodR"),
      p("This application serves as the graphical interface for the ", tags$code("spacemodR"),
        " R package, developed to facilitate the manipulation of complex geospatial data within an ecotoxicological framework."),
      p("Key features of the package include:"),
      tags$ul(
        tags$li("Automated retrieval of geographic data (FlatGeobuf API)."),
        tags$li("Processing of Land Use/Land Cover data (OCS-GE) and habitat definition."),
        tags$li("Construction of spatial graphs to model trophic networks and organism dispersion."),
        tags$li("Integration with exposure models for landscape-scale risk calculation.")
      ),
      p("The source code is available as open-source software on GitHub: ",
        tags$a(href = "https://github.com/Qonfluens/spacemodR", target = "_blank", "Qonfluens/spacemodR")
      ),

      # --- Section 3: Funding and Partners ---
      h3("Partners and Funding"),
      p("This project is the result of a collaboration between academic research and private expertise,
        supported by the French Agency for Ecological Transition (ADEME) under the IMPACT program."),

      h5("Consortium:"),
      tags$ul(
        tags$li(strong("Laboratoire Chrono-Environnement"), " (UMR 6249 UFC / CNRS): Expertise in landscape ecotoxicology and environmental modeling."),
        tags$li(strong("Qonfluens"), ": Software development, data architecture, and digital solutions for ecology.")
      ),

      # --- Section 4: Links and References ---
      h3("Learn More"),
      p("For more details on the scientific foundations and project news:"),
      p(
        tags$a(href = "https://chrono-environnement.univ-fcomte.fr/project/spacemod/", target = "_blank",
               icon("link"), " Project Page - Chrono-Environnement (University of Franche-Comté)")
      ),
      p(
        tags$a(href = "https://recherche.ademe.fr/spacemod", target = "_blank",
               icon("leaf"), " Project Sheet - ADEME Research")
      ),

      hr(),
      p(em("Application generated with R Shiny and spacemodR."), style = "text-align: right; font-size: 0.8rem; color: #777;")
    )
  )
}

#' Server for the credits module
#' @export
mod_credit_server <- function(id, shared_global) {
  # No server logic required for static text display
}
