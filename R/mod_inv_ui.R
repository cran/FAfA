#' Measurement Invariance UI
#' @noRd
inv_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 8),
      tagList(
        card(
          card_header("Model Builder", class = "bg-info text-white", bs_icon("magic")),
          card_body(
            h6("Define Factor (=~)", class = "text-primary"),
            textInput(ns("builder_factor_name"), "Factor Name:", placeholder = "e.g. F1, F2, Factor1 ..."),
            selectizeInput(
              ns("builder_items"),
              "Select Indicators:",
              choices = NULL,
              multiple = TRUE,
              options = list(placeholder = "Select variables...")
            ),
            actionButton(
              ns("btn_add_to_model"),
              "Add to Syntax",
              icon = icon("code"),
              class = "btn-secondary btn-sm w-100 mb-3"
            ),

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
              "Add Covariance",
              icon = icon("link"),
              class = "btn-secondary btn-sm w-100"
            )
          )
        ),

        card(
          card_header("Invariance Setup", class = "bg-primary text-white"),
          card_body(
            textAreaInput(
              ns("inv_model_syntax"),
              "Model Syntax (lavaan):",
              rows = 6,
              placeholder = "Use the builder above or type syntax here..."
            ),
            selectInput(ns("grouping_variable_select"), "Grouping Variable:", choices = ""),
            radioButtons(
              ns("correlation_matrix_type"),
              "Correlation Matrix:",
              choices = c(
                "Pearson (continuous)" = "pea",
                "Polychoric (ordinal)" = "poly"
              ),
              selected = "pea",
              inline = TRUE
            ),
            selectInput(
              ns("invariance_estimator_select"),
              "Estimator:",
              choices = c("Default (MLR)" = "default", "MLR", "ML", "GLS"),
              selected = "default"
            ),
            checkboxGroupInput(
              ns("invariance_levels_checkbox"),
              "Levels:",
              choices = c("configural", "metric", "scalar", "strict"),
              selected = c("configural", "metric")
            ),
            helpText(
              class = "small text-muted",
              "For ordinal/polychoric models, scalar invariance uses threshold constraints."
            ),
            actionButton(ns("run_invariance_button"), "Run Analysis", class = "btn-success w-100")
          )
        )
      ),


      card(
        card_header("Results"),
        navset_card_tab(
          nav_panel(
            "Fit Measures",
            tableOutput(ns("invariance_fit_measures_table")),
            downloadButton(ns("download_fit_measures_button"), "Download CSV", class = "btn-sm")
          ),
          nav_panel(
            "Model Comparison",
            tableOutput(ns("model_comparison_table")),
            downloadButton(ns("download_model_comparison_button"), "Download CSV", class = "btn-sm")
          )
        )
      )
    )
  )
}
