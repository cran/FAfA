#' ItemRest Analysis UI Module
#'
#' @param id Module namespace ID.
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @export
mod_itemrest_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 8),
      
      # Analysis Settings Card
      card(
        card_header(
          class = "bg-info text-white",
          "Analysis Settings", 
          bs_icon("sliders")
        ),
        card_body(
          p("Automated item removal strategies for EFA."),
          
          # Inputs based on itemrest function arguments [cite: 78-84]
          numericInput(
            ns("n_factors"),
            "Number of Factors (Optional):",
            value = NA, 
            min = 1,
            step = 1
          ),
          helpText("Leave empty to determine automatically via Parallel Analysis."),
          
          selectInput(
            ns("cor_method"),
            "Correlation Method:",
            choices = c("Pearson" = "pearson", "Polychoric" = "polychoric"),
            selected = "pearson"
          ),
          
          selectInput(
            ns("extraction_method"),
            "Extraction Method:",
            choices = c(
              "Unweighted Least Squares" = "uls",
              "Minimum Residual" = "minres",
              "Maximum Likelihood" = "ml",
              "Principal Axis" = "pa"
            ),
            selected = "uls"
          ),
          
          selectInput(
            ns("rotation_method"),
            "Rotation Method:",
            choices = c(
              "Oblimin" = "oblimin",
              "Varimax" = "varimax",
              "Promax" = "promax",
              "GeominQ" = "geominQ"
            ),
            selected = "oblimin"
          ),
          
          actionButton(
            ns("run_itemrest"),
            "Run ItemRest Analysis",
            class = "btn-info w-100",
            icon = icon("play")
          )
        )
      ),
      
      # Optimal Strategy Results
      card(
        card_header("Optimal Strategy Result"),
        card_body(
          verbatimTextOutput(ns("optimal_strategy_text")),
          p("The strategy above represents the cleanest factor structure found.")
        )
      )
    ),
    
    # Detailed Summary
    card(
      card_header("Comparative Removal Strategies"),
      card_body(
        tableOutput(ns("removal_summary_table")),
        p(em("This table compares model fit and structure across different item removal thresholds."))
      )
    )
  )
}