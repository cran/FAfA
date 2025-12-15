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
          # --- YENİ EKLENEN KISIM: Madde Seçimi ---
          selectizeInput(
            ns("reliability_item_select"),
            "Select Items (Leave empty for all):",
            choices = NULL,
            multiple = TRUE,
            options = list(placeholder = "Select specific variables...")
          ),
          hr(),
          # ----------------------------------------

          # 1. Katsayı Seçimi
          selectInput(ns("reliability_coefficient_select"), "Coefficient:",
                      choices = c("Cronbach's Alpha"="alpha", "McDonald's Omega"="omega", "Armor's Theta"="theta", "Stratified Alpha" = "s_alpha")),

          conditionalPanel("input.reliability_coefficient_select == 'theta'", ns=ns,
                           radioButtons(ns("correlation_type_radio"), "Correlation:", choices = c("Pearson"="cor", "Polychoric"="poly"), inline=TRUE)),

          # 2. Stratified Alpha Alanı
          conditionalPanel("input.reliability_coefficient_select == 's_alpha'", ns=ns,
                           textInput(ns("strata_definition_input"), "Define Strata (e.g., 1,1,2,2,3):", placeholder = "Comma separated item strata...")),

          actionButton(ns("run_reliability_button"), "Calculate", icon = icon("calculator"), class = "btn-info w-100")
        )
      ),
      card(
        card_header("Result"),
        card_body(
          h4("Calculated Coefficient:"),
          verbatimTextOutput(ns("reliability_result_output")) %>% withSpinner(type = 8, color = "#2C3E50"),
          p(class = "text-muted", "Reference: > 0.70 Acceptable, > 0.80 Good")
        )
      )
    )
  )
}
