#' Reliability UI
#' @noRd
reliability_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(5, 7),
      card(
        card_header("Reliability Setup", class = "bg-info text-white"),
        card_body(
          selectizeInput(
            ns("reliability_item_select"),
            "Select Items (leave empty for all):",
            choices  = NULL,
            multiple = TRUE,
            options  = list(placeholder = "Select specific variables...")
          ),
          hr(),
          radioButtons(ns("reliability_coefficient_select"), "Coefficient:",
                       choices = c(
                         "Cronbach's Alpha"                  = "alpha",
                         "McDonald's Omega Total"            = "omega",
                         "McDonald's Omega Hierarchical"     = "omega_h",
                         "Armor's Theta"                     = "theta",
                         "Stratified Alpha"                  = "s_alpha",
                         "Composite Reliability & AVE (CFA)" = "cr"
                       )
          ),

          # Theta: correlation type
          conditionalPanel("input.reliability_coefficient_select == 'theta'", ns = ns,
                           radioButtons(ns("correlation_type_radio"), "Correlation:",
                                        choices = c("Pearson" = "cor", "Polychoric" = "poly"), inline = TRUE)
          ),

          # Stratified Alpha: strata definition
          conditionalPanel("input.reliability_coefficient_select == 's_alpha'", ns = ns,
                           textInput(ns("strata_definition_input"), "Strata (e.g., 1,1,2,2,3):",
                                     placeholder = "Comma-separated item strata numbers...")
          ),

          # CR & AVE: requires CFA model syntax
          conditionalPanel("input.reliability_coefficient_select == 'cr'", ns = ns,
                           textAreaInput(ns("cfa_model_for_reliability_input"),
                                         "CFA Model Syntax (lavaan):",
                                         rows = 5,
                                         placeholder = "e.g.\nF1 =~ item1 + item2 + item3\nF2 =~ item4 + item5"),
                           radioButtons(ns("cr_correlation_type_radio"), "Data Type:",
                                        choices = c("Continuous (Pearson)" = "cor", "Ordinal (Polychoric)" = "poly"),
                                        inline = TRUE)
          ),

          actionButton(ns("run_reliability_button"), "Calculate",
                       icon = icon("calculator"), class = "btn-info w-100")
        )
      ),
      card(
        card_header("Result"),
        card_body(
          verbatimTextOutput(ns("reliability_result_output")) %>%
            withSpinner(type = 8, color = "#2C3E50")
        )
      )
    )
  )
}
