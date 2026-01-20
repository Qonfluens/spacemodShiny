#' UI for Trophic Module
#' @export
mod_trophic_ui <- function(id) {
  ns <- NS(id)

  # Sidebar Content
  sidebar_content <- list(
    h4("Trophic Network"),

    # --- 1. Add Interaction ---
    tags$div(
      class = "bg-light p-3 rounded border",
      tags$strong("Define Interaction"),

      # From / To Selectors (Empty initially, updated via server)
      selectInput(ns("source_node"), "From (Source):", choices = NULL),
      selectInput(ns("target_node"), "To (Target):", choices = NULL),

      numericInput(ns("link_weight"), "Weight:", value = 1, min = 0, step = 0.1),

      actionButton(
        ns("add_link_btn"),
        "Add Link",
        icon = icon("plus"),
        class = "btn-success w-100"
      )
    ),

    tags$hr(),

    # --- 2. List of Interactions ---
    div(
      class = "d-flex justify-content-between align-items-center mb-1",
      tags$strong("Interaction List"),
      actionButton(
        ns("remove_link_btn"),
        "Remove Selected",
        class = "btn-outline-danger btn-sm py-0",
        icon = icon("trash")
      )
    ),
    DT::DTOutput(ns("table_links")),

    tags$br(),
    tags$hr(),

    # --- 3. Validation ---
    actionButton(
      ns("submit_btn"),
      "Validate Network",
      class = "btn-primary w-100",
      icon = icon("check")
    )
  )

  # Main Layout
  bslib::page_fillable(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 400,
        title = "Parameters",
        sidebar_content
      ),

      # Visualization Card
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          "Adjacency Matrix (Heatmap)",
          bslib::tooltip(
            icon("circle-info"),
            "Visualizes the flow of energy. Rows are Sources, Columns are Targets."
          )
        ),
        bslib::card_body(
          # Plotly Output
          plotly::plotlyOutput(ns("heatmap_trophic"), height = "100%")
        )
      )
    )
  )
}

#' Server for Trophic Module
#' @export
mod_trophic_server <- function(id, shared_global) {
  shiny::moduleServer(id, function(input, output, session) {

    # --------------------------
    # 1. INITIALIZATION & UPDATES
    # --------------------------

    # Reactive to get habitat names from the previous step
    habitat_names <- reactive({
      # Check if previous step is validated and config exists
      req(shared_global$habitats_config)
      names(shared_global$habitats_config)
    })

    # Update SelectInputs when habitats change
    observeEvent(habitat_names(), {
      habs <- habitat_names()
      updateSelectInput(session, "source_node", choices = habs)
      updateSelectInput(session, "target_node", choices = habs)
    })

    # --------------------------
    # 2. MANAGE LINKS (DATA)
    # --------------------------

    # Store links in a reactive dataframe
    # Columns: From, To, Weight
    links_store <- reactiveValues(data = data.frame(
      From = character(),
      To = character(),
      Weight = numeric(),
      stringsAsFactors = FALSE
    ))

    # A. Add Link
    observeEvent(input$add_link_btn, {
      req(input$source_node, input$target_node, input$link_weight)

      # Prevent duplicates (same Source -> Target)
      # You can decide if you want to overwrite or block. Here we block.
      current_data <- links_store$data
      is_duplicate <- any(current_data$From == input$source_node & current_data$To == input$target_node)

      if(is_duplicate) {
        showNotification("This link already exists. Remove it first to update.", type = "warning")
        return()
      }

      new_row <- data.frame(
        From = input$source_node,
        To = input$target_node,
        Weight = input$link_weight,
        stringsAsFactors = FALSE
      )

      links_store$data <- rbind(links_store$data, new_row)
    })

    # B. Remove Link
    observeEvent(input$remove_link_btn, {
      selected_idx <- input$table_links_rows_selected
      if (is.null(selected_idx)) {
        showNotification("Select a row to remove.", type = "warning")
        return()
      }
      links_store$data <- links_store$data[-selected_idx, , drop = FALSE]
    })

    # C. Render Table
    output$table_links <- DT::renderDT({
      DT::datatable(
        links_store$data,
        selection = 'single',
        options = list(dom = 't', paging = FALSE, scrollY = "200px", scrollCollapse = TRUE),
        rownames = FALSE
      )
    })

    # --------------------------
    # 3. VISUALIZATION (PLOTLY HEATMAP)
    # --------------------------

    output$heatmap_trophic <- plotly::renderPlotly({
      # Requirement: Habitats must allow matrix construction
      habs <- habitat_names()
      req(length(habs) > 0)

      data_links <- links_store$data

      # Construct Matrix N x N
      # Rows = FROM, Cols = TO
      n <- length(habs)
      mat <- matrix(0, nrow = n, ncol = n, dimnames = list(habs, habs))

      if (nrow(data_links) > 0) {
        for (i in 1:nrow(data_links)) {
          r <- data_links$From[i]
          c <- data_links$To[i]
          w <- data_links$Weight[i]

          # Fill matrix
          if (r %in% habs && c %in% habs) {
            mat[r, c] <- w
          }
        }
      }

      # Create Heatmap
      # Note: Plotly Heatmap x=Columns(To), y=Rows(From)
      plotly::plot_ly(
        x = colnames(mat),
        y = rownames(mat),
        z = mat,
        type = "heatmap",
        colors = colorRamp(c("#f7fbff", "#08306b")), # Light to Dark Blue
        text = text,
        hoverinfo = "x+y+z"
      ) |>
        plotly::layout(
          title = "Trophic Interaction Matrix",
          xaxis = list(title = "To (Target)"),
          yaxis = list(title = "From (Source)", autorange = "reversed") # Reversed to match matrix convention top-down
        )
    })

    # --------------------------
    # 4. VALIDATION
    # --------------------------
    go_next <- reactiveVal(FALSE)

    observeEvent(input$submit_btn, {
      # Optional: Check if at least one link exists?
      # if (nrow(links_store$data) == 0) { ... }

      shared_global$trophic_data <- links_store$data


      showNotification("Trophic network validated.", type = "message")
      go_next(TRUE)
    })

    return(go_next)
  })
}
