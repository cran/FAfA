# Reliability Analysis UI Module
 
reliability_ui <- function(id) {
  ns <- NS(id) # Namespace function

  tagList(
    fluidRow(
      column(
        width = 11, # Or 12 for full width
        box(
          title = "Introduction to Reliability Analysis",
          collapsible = TRUE,
          status = "info", 
          solidHeader = TRUE,
          width = NULL,
          strong(
            "Reliability analysis assesses the consistency and stability of a measurement scale or instrument.",br(),
            "This module allows you to calculate various reliability coefficients, such as Cronbach's Alpha, McDonald's Omega, and Armor's Theta, among others."
          ),
          br(),
          em(
            "To proceed:",br(),
            "1. Select the desired reliability coefficient from the dropdown menu.",br(),
            "2. Provide any additional information required for the selected coefficient (e.g., factor structure for structural reliability, strata definition for stratified alpha).",br(),
            "3. Specify the correlation type if relevant (primarily for Armor's Theta in this implementation).",br(),
            "4. Click 'Calculate Reliability' to view the result."
          )
        )
      )
    ),
    fluidRow(
      # Column for options
      column(
        width = 6, 
        box(
          title = "Step 1: Select Reliability Coefficient & Options",
          collapsible = TRUE,
          status = "primary", 
          solidHeader = TRUE,
          width = NULL,
          selectInput(
            ns("reliability_coefficient_select"), 
            label = "Choose Reliability Coefficient:",
            choices = c(
              "Cronbach's Alpha" = "alpha",
              "McDonald's Omega" = "omega", 
              "Armor's Theta (Principal Components based)" = "theta",
              "Structural Reliability (Model-based, requires CFA structure)" = "structure",
              "Stratified Alpha (For scales with subscales/strata)" = "s_alpha"
            ),
            selected = "alpha" 
          ),
          # Conditional panel for "Structural Reliability"
          conditionalPanel(
            condition = "input.reliability_coefficient_select == 'structure'",
            ns = ns, 
            textAreaInput(
              ns("cfa_model_for_reliability_input"), 
              label = "Define CFA Structure (lavaan syntax):",
              placeholder = "e.g., F1 =~ item1 + item2 + item3\nF2 =~ item4 + item5 + item6",
              rows = 4
            )
          ),
          # Conditional panel for "Stratified Alpha"
          conditionalPanel(
            condition = "input.reliability_coefficient_select == 's_alpha'",
            ns = ns,
            textInput(
              ns("strata_definition_input"), 
              label = "Define Strata Membership for Each Item (comma-separated integers):",
              placeholder = "e.g., 1,1,1,2,2,2 (for 6 items, 2 strata)"
            ),
            em("Enter one integer per item, indicating its stratum. Must match the number of items in the dataset.")
          ),
          # Conditional panel for correlation type (shown for Theta, or always if other methods might use it)
          # Based on your original code, it's mainly for Theta.
          conditionalPanel(
            condition = "input.reliability_coefficient_select == 'theta'",
            ns = ns,
            radioButtons(
              ns("correlation_type_radio"), 
              label = "Correlation Type (for Armor's Theta):",
              choices = c("Pearson (for continuous data)" = "cor", "Polychoric (for ordinal data)" = "poly"),
              inline = TRUE,
              selected = "cor" 
            )
          ),
          actionButton(ns("run_reliability_button"), "Calculate Reliability", icon = icon("calculator")) 
        )
      ),
      # Column for results
      column(
        width = 5, 
        box(
          title = "Step 2: Reliability Result",
          collapsible = TRUE,
          status = "success", 
          solidHeader = TRUE,
          width = NULL,
          h4("Calculated Reliability Coefficient:"),
          verbatimTextOutput(ns("reliability_result_output")) %>% withSpinner(type = 8, color = "#728FCE"), 
          br(),
          em("Interpretation guidance: Generally, reliability coefficients > 0.70 are considered acceptable, > 0.80 good, and > 0.90 excellent for many research purposes, but context matters.")
        )
      )
    )
  )
}
