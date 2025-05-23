# Measurement Invariance UI Module
 
inv_ui <- function(id) {
  ns <- NS(id) # Namespace function

  tagList(
    fluidRow(
      column(
        width = 11, # Or 12 for full width
        box(
          title = "Introduction to Measurement Invariance Analysis",
          collapsible = TRUE,
          status = "info", # Using "info" for introductory boxes
          solidHeader = TRUE,
          width = NULL,
          strong(
            "Measurement Invariance (MI) analysis, also known as measurement equivalence, tests whether a measurement instrument (e.g., a scale or questionnaire) measures the same construct in the same way across different groups of respondents or across different time points.", br(),
            "This is crucial for making meaningful comparisons between groups. This module will guide you through setting up and interpreting MI tests."
          ),
          br(),
          em(
            "You will need to: ", br(),
            "1. Define the factor structure of your measurement model using lavaan syntax.", br(),
            "2. Select a categorical grouping variable from your dataset.", br(),
            "3. Choose the appropriate correlation matrix type and estimator for your data.", br(),
            "4. Select the levels of invariance you wish to test (e.g., configural, metric, scalar, strict)."
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 5, # Column for model definition and options
        box(
          title = "Step 1: Define Model and Select Options",
          collapsible = TRUE,
          status = "primary", # "primary" for setup/action boxes
          solidHeader = TRUE,
          width = NULL,
          textAreaInput(
            ns("inv_model_syntax"), # Changed ID for clarity
            label = "Define Factor Structure (lavaan syntax):",
            placeholder = "e.g.,\nF1 =~ item1 + item2 + item3\nF2 =~ item4 + item5 + item6",
            rows = 6 # Increased rows for better visibility of model syntax
          ),
          selectInput(
            ns("grouping_variable_select"), # Changed ID
            label = "Select Grouping Variable:",
            choices = c("Select a variable" = "") # Dynamically updated by server
          ),
          radioButtons(
            ns("correlation_matrix_type"), # Changed ID
            label = "Select Data Type / Correlation Matrix:",
            choices = c("Continuous (Pearson)" = "pea", "Ordinal (Polychoric)" = "poly"),
            inline = TRUE,
            selected = "pea" # Default to Pearson
          ),
          selectInput(
            ns("estimator_method_select"), # Changed ID
            label = "Select Estimator:",
            # Choices will be dynamically updated based on correlation_matrix_type
            choices = c("ML" = "ML", "MLR" = "MLR", "GLS" = "GLS"), # Initial choices for Pearson
            selected = "ML"
          ),
          checkboxGroupInput(
            ns("invariance_levels_checkbox"), # Changed ID
            label = "Select Invariance Levels to Test:",
            choices = c(
              "Configural (same factor structure)" = "configural",
              "Metric (equal factor loadings)" = "metric",
              "Scalar (equal intercepts/thresholds)" = "scalar",
              "Strict (equal residuals/error variances)" = "strict"
            ),
            selected = c("configural", "metric") # Default selected levels
          ),
          actionButton(ns("run_invariance_button"), "Run Measurement Invariance Analysis", icon = icon("cogs")) # Changed ID
        )
      ),
      column(
        width = 6, # Column for results
        box(
          title = "Step 2: Review Data Cleaning Information",
          collapsible = TRUE,
          collapsed = TRUE, # Start collapsed as it's informational
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          textOutput(ns("data_cleaning_summary_text")) # Changed ID
        ),
        box(
          title = "Step 3: Examine Fit Measures for Invariance Models",
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          em("This table displays various fit indices for each level of invariance tested. Lower Chi-square/df, higher CFI/TLI (>0.90 or >0.95), and lower RMSEA/SRMR (<0.08 or <0.06) generally indicate better model fit."),
          br(),br(),
          tableOutput(ns("invariance_fit_measures_table")) %>% withSpinner(type = 8, color = "#728FCE"), # Changed ID
          downloadButton(ns("download_fit_measures_button"), "Download Fit Measures (.csv)", icon = icon("download")) # Changed ID
        ),
        box(
          title = "Step 4: Compare Nested Models",
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          em("This table compares nested models (e.g., metric vs. configural). A non-significant Chi-square difference (p > 0.05) suggests that the more constrained model (e.g., metric) does not fit significantly worse than the less constrained model (e.g., configural), supporting the higher level of invariance. Changes in CFI (Delta(CFI) < -0.01) are also commonly used."),
          br(),br(),
          tableOutput(ns("model_comparison_table")) %>% withSpinner(type = 8, color = "#728FCE"), # Changed ID
          downloadButton(ns("download_model_comparison_button"), "Download Model Comparison (.csv)", icon = icon("download")) # Changed ID
        )
      )
    )
  )
}
