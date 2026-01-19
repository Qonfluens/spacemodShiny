#' @title title
#'
#' @description
#' A short description...
#'
#' @param id description
#'
#' @export
mod_credit_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_column_wrap(
    width = 1,
    heights_equal="row",
    bslib::card(
      title = "Data specification",
      full_screen = TRUE,
      h3("What is a Species Sensitivity Distribution (SSD)?"),
      p("A Species Sensitivity Distribution (SSD) is a statistical approach used
      in ecotoxicology to estimate the relative sensitivity of different species
      to a chemical substance. The method compiles toxicity values such as LC50,
      EC50, EC10, or NOEC obtained from standardised ecotoxicological tests
      conducted on a range of taxonomically diverse species. These values are
      fitted to a statistical distribution, most commonly log-normal, log-logistic,
      or Burr type functions. From this fitted curve, a hazardous concentration
      for a given percentage of species can be derived. The HC5, which
      represents the concentration expected to affect 5% of the species,
      is widely used as a scientifically based threshold for ecological protection."),
      p("Regulatory authorities use SSDs to derive Predicted No Effect
        Concentrations (PNECs) and to establish environmental quality criteria
        for water, soil, and sediment. The approach is valued because it integrates
        interspecies variability in sensitivity and provides a transparent,
        reproducible method for risk assessment."),
      h3("OECD Guidance and International Frameworks Referring to SSDs"),
      p("Within the OECD framework, SSDs are not test guidelines (TG) in themselves
        but rather an analytical method referenced in several guidance documents.
        OECD Guidance Document 54 (2006), “Current Approaches in the Statistical Analysis of Ecotoxicity Data,”
        provides detailed recommendations on constructing SSDs and deriving HC5 values.
        OECD Guidance Document 27 (2001), “Guidance Document on the Use of the Harmonised
        System for the Classification of Chemicals which are Hazardous for the Aquatic Environment,”
        refers to SSDs as a tool to support classification decisions and to derive threshold values.
        More recently, OECD Guidance Document 23 (2019, revised), “Guidance Document on Aquatic Toxicity
        Testing of Difficult Substances and Mixtures,” mentions the application of SSDs for deriving
        PNECs when multiple species datasets are available. In addition, OECD Guidance Document 91 (2008),
        reporting on the workshop on multimedia models, highlights SSDs in the context of environmental risk assessment,
        in combination with persistence and exposure models."),
      p("Beyond the OECD, several international frameworks explicitly require or recommend the use of SSDs.
        Under the European Chemicals Agency (ECHA) REACH Regulation, the guidance document R.10 specifies
        SSDs as the preferred method for deriving a PNEC when at least ten independent species data points
        are available, representing diverse taxa and trophic levels. The European Food Safety Authority (EFSA)
        guidance documents on aquatic organisms (2013, 2018) recommend SSDs to establish thresholds protective
        of aquatic communities, and more recently SSDs have been applied to pollinators in the EFSA Bee Guidance
        (2013, with ongoing revision). In the United States, the Environmental Protection Agency (US EPA)
        has adopted the SSD approach since the 1985 Guidelines for Water Quality Criteria, often referred
        to as the Final Acute Value (FAV) method, to establish national water quality standards.
        Furthermore, SSDs are endorsed by international scientific bodies such as UNEP and WHO in
        the context of chemical safety and environmental protection."),
      h3("OECD Test Guidelines Supplying Data for SSD Construction"),
      p("Although no OECD Test Guideline is dedicated to producing an SSD directly,
        many provide the toxicity data required to construct them.
        The following types of TGs are most frequently used:"),
      h5("Aquatic tests:"),
      p("TG 201 (algal growth inhibition), TG 202 (acute daphnia immobilisation),
        TG 203 (acute fish toxicity), TG 210 (fish early life stage),
        TG 211 (daphnia reproduction), TG 225, 229, 230 (aquatic plants such as Lemna, Myriophyllum)."),
      h5("Terrestrial tests:"),
      p("TG 208 (seedling emergence and plant growth), TG 222 (earthworm reproduction),
        TG 226 (honeybee acute toxicity), TG 213/214 (avian dietary and gavage toxicity)."),
      h5("Soil and sediment organisms"),
      p("TG 216/217 (soil microorganisms: nitrification and transformation processes),
        TG 218/219 (sediment organisms such as Chironomus and Lumbriculus)."),
      h3("References"),
      p("OECD (2001). ",
        em("Guidance Document on the Use of the Harmonised System for the Classification of Chemicals which are Hazardous for the Aquatic Environment."),
        " OECD Series on Testing and Assessment, No. 27. Organisation for Economic Co-operation and Development, Paris."
      ),
      p("OECD (2006). ",
        em("Current Approaches in the Statistical Analysis of Ecotoxicity Data: A Guidance to Application."),
        " OECD Series on Testing and Assessment, No. 54. Organisation for Economic Co-operation and Development, Paris."
      ),
      p("OECD (2008). ",
        em("Report of the OECD Workshop on the Use of Multimedia Models for Estimating Overall Environmental Persistence and Long-range Transport."),
        " OECD Series on Testing and Assessment, No. 91. Organisation for Economic Co-operation and Development, Paris."
      ),
      p("OECD (2019). ",
        em("Guidance Document on Aquatic Toxicity Testing of Difficult Substances and Mixtures (Second Edition)."),
        " OECD Series on Testing and Assessment, No. 23. Organisation for Economic Co-operation and Development, Paris."
      ),
      p("ECHA (2012). ",
        em("Guidance on Information Requirements and Chemical Safety Assessment, Chapter R.10: Characterisation of Dose [Concentration]-Response for Environment."),
        " European Chemicals Agency, Helsinki."
      ),
      p("EFSA (2013). ",
        em("Guidance Document on Aquatic Ecotoxicology in the Context of the EU Plant Protection Product Regulation."),
        " EFSA Journal 11(7):3290. European Food Safety Authority, Parma."
      ),
      p("EFSA (2013, revised 2023). ",
        em("Guidance Document on the Risk Assessment of Plant Protection Products on Bees."),
        " EFSA Journal 11(7):3295. European Food Safety Authority, Parma."
      ),
      p("EFSA (2018). ",
        em("Aquatic Guidance Document for Pesticide Risk Assessment for Aquatic Organisms."),
        " European Food Safety Authority, Parma."
      ),
      p("US EPA (1985). ",
        em("Guidelines for Deriving Numerical National Water Quality Criteria for the Protection of Aquatic Organisms and Their Uses."),
        " Office of Research and Development, US Environmental Protection Agency, Washington, D.C."
      ),
      p("UNEP/WHO/IPCS (2004). ",
        em("Chemical-Specific Adjustment Factors for Interspecies Differences and Human Variability: Guidance Document for Use of Data in Dose/Concentration–Response Assessment."),
        " International Programme on Chemical Safety, Geneva."
      )
    )
  )
}



#' Serveur du module credits
#' @export
mod_credit_server <- function(id, shared_global) {

}
