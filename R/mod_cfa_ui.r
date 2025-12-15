#' Confirmatory Factor Analysis (CFA) UI Module
#'
#' @param id Module namespace ID.
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @importFrom shinycssloaders withSpinner
#' @export
cfa_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_columns(
      col_widths = c(4, 8),

      # --- LEFT COLUMN: Setup & Model Building ---

      # Card 1: Model Builder (Geliştirilmiş)
      card(
        card_header(
          class = "bg-info text-white",
          "Model Builder",
          bs_icon("magic")
        ),
        card_body(
          # 1. Faktör Tanımlama
          h6("Define Factor (=~)", class = "text-primary"),
          textInput(ns("builder_factor_name"), "Factor Name (e.g., F1):", placeholder = "F1"),
          selectizeInput(
            ns("builder_items"),
            "Select Indicators:",
            choices = NULL,
            multiple = TRUE,
            options = list(placeholder = "Select variables...")
          ),
          actionButton(
            ns("btn_add_to_model"),
            "Add Factor",
            icon = icon("plus"),
            class = "btn-secondary btn-sm w-100 mb-3"
          ),

          # 2. Kovaryans Ekleme (YENİ)
          h6("Add Covariance (~~)", class = "text-primary"),
          selectizeInput(
            ns("builder_cov_items"),
            "Select 2 Variables:",
            choices = NULL,
            multiple = TRUE,
            options = list(maxItems = 2, placeholder = "Select 2 vars...")
          ),
          actionButton(
            ns("btn_add_cov"),
            "Add Covariance (~~)",
            icon = icon("link"),
            class = "btn-secondary btn-sm w-100"
          )
        )
      ),

      # Card 2: Manual Syntax & Estimation Settings
      card(
        card_header(
          class = "bg-primary text-white",
          "CFA Analysis Setup",
          bs_icon("sliders")
        ),
        card_body(
          textAreaInput(
            ns("cfa_model_syntax_input"),
            label = "Model Syntax (lavaan):",
            placeholder = "e.g.,\nF1 =~ item1 + item2\nV1 ~~ V2",
            rows = 8,
            resize = "vertical"
          ),
          radioButtons(
            ns("cfa_correlation_type_radio"),
            label = "Data Type:",
            choices = c("Continuous (Pearson)" = "pea", "Ordinal (Polychoric)" = "poly"),
            inline = TRUE,
            selected = "pea"
          ),
          selectInput(
            ns("cfa_estimator_select"),
            label = "Estimator:",
            choices = c("ML", "MLR", "GLS"),
            selected = "ML"
          ),
          actionButton(
            ns("run_cfa_button"),
            "Run CFA Analysis",
            icon = icon("play"),
            class = "btn-success w-100"
          )
        )
      )
    ),

    # --- RIGHT COLUMN: Results & Plot ---

    navset_card_tab(
      full_screen = TRUE,

      # Tab 1: Path Diagram
      nav_panel(
        title = "Path Diagram",
        icon = bs_icon("diagram-3"),
        layout_sidebar(
          sidebar = sidebar(
            title = "Plot Settings",
            width = 250,
            selectInput(ns("plot_layout"), "Layout Style:",
                        choices = c("Tree (Hierarchical)" = "tree",
                                    "Tree (Left-Right)" = "tree2",
                                    "Spring (Force-directed)" = "spring",
                                    "Circle" = "circle")),
            sliderInput(ns("plot_rotation"), "Rotation:", min = 1, max = 4, value = 2, step = 1),
            sliderInput(ns("plot_man_size"), "Box Width:", min = 4, max = 20, value = 10),
            sliderInput(ns("plot_edge_label_cex"), "Label Size:", min = 0.5, max = 1.5, value = 0.8, step = 0.1),
            checkboxInput(ns("plot_show_labels"), "Show Estimates", value = TRUE)
          ),
          plotOutput(ns("cfa_path_diagram_output"), height = "600px") %>% withSpinner(type = 8, color = "#2C3E50"),
          downloadButton(ns("download_path_diagram_button"), "Download Diagram (.svg)", class = "btn-sm")
        )
      ),

      # Tab 2: Fit Measures
      nav_panel(
        title = "Fit Measures",
        icon = bs_icon("table"),
        tableOutput(ns("cfa_fit_measures_table")),
        downloadButton(ns("download_fit_measures_button"), "Download CSV", class = "btn-sm")
      ),

      # Tab 3: Factor Loadings
      nav_panel(
        title = "Factor Loadings",
        icon = bs_icon("list-ol"),
        tableOutput(ns("cfa_factor_loadings_table")),
        downloadButton(ns("download_factor_loadings_button"), "Download CSV", class = "btn-sm")
      ),

      # Tab 4: Modification Indices
      nav_panel(
        title = "Modification Indices",
        icon = bs_icon("tools"),
        tableOutput(ns("cfa_modification_indices_table"))
      )
    )
  )
}
