#' Assumptions UI Module
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @noRd
assumptions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Descriptive Statistics
    card(
      card_header("Descriptive Statistics", class = "bg-info text-white", bs_icon("table")),
      card_body(
        actionButton(ns("run_descriptives_button"), "Calculate Descriptives", icon = icon("calculator"), class = "btn-info mb-3"),
        tableOutput(ns("descriptives_table_output")) %>% withSpinner(type = 8, color = "#2C3E50"),
        downloadButton(ns("download_descriptives_button"), "Download CSV")
      )
    ),
    
    # Collinearity & Normality Side-by-Side
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Collinearity", bs_icon("link-45deg")),
        card_body(
          p("Check for multicollinearity (VIF, Tolerance)."),
          actionButton(ns("run_collinearity_button"), "Run Collinearity Check", class = "btn-secondary w-100 mb-2"),
          tableOutput(ns("collinearity_table_output"))
        )
      ),
      card(
        card_header("Multivariate Normality", bs_icon("graph-up-arrow")),
        card_body(
          p("Mardia's Skewness & Kurtosis, Energy Test."),
          actionButton(ns("run_normality_tests_button"), "Run Normality Tests", class = "btn-secondary w-100 mb-2"),
          tableOutput(ns("multivariate_normality_table_output"))
        )
      )
    )
  )
}