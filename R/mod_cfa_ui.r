# Confirmatory Factor Analysis (CFA) UI Module
 
cfa_ui <- function(id) {
  ns <- NS(id) # Namespace function

  tagList(
    fluidRow(
      column(
        width = 11, # Or 12 for full width
        box(
          title = "Introduction to Confirmatory Factor Analysis (CFA)",
          collapsible = TRUE,
          status = "info", # Using "info" for introductory boxes
          solidHeader = TRUE,
          width = NULL,
          strong(
            "Confirmatory Factor Analysis (CFA) is a statistical technique used to verify a pre-defined factor structure (a measurement model).",br(),
            "Unlike Exploratory Factor Analysis (EFA), CFA requires you to specify which variables load onto which factors based on existing theory or prior research."
          ),
          br(),
          em(
            "In this module, you will:",br(),
            "1. Define your hypothesized factor structure using lavaan syntax.",br(),
            "2. Select the appropriate data type (correlation matrix) and estimator.",br(),
            "3. Run the CFA to assess how well your model fits the data.",br(),
            "4. Examine fit indices, factor loadings, modification indices, and a path diagram."
          )
        )
      )
    ),
    fluidRow(
      # Column for CFA Setup
      column(
        width = 4,
        box(
          title = "Step 1: CFA Setup",
          collapsible = TRUE,
          status = "primary", # "primary" for setup/action boxes
          solidHeader = TRUE,
          width = NULL,
          textAreaInput(
            ns("cfa_model_syntax_input"), # Changed ID
            label = "Define Factor Structure (lavaan syntax):",
            placeholder = "e.g.,\nF1 =~ item1 + item2 + item3\nF2 =~ item4 + item5 + item6\n\n# Optional: Specify covariances\n# F1 ~~ F2",
            rows = 7 # Increased rows
          ),
          radioButtons(
            ns("cfa_correlation_type_radio"), # Changed ID
            label = "Select Data Type / Correlation Matrix:",
            choices = c("Continuous (Pearson)" = "pea", "Ordinal (Polychoric)" = "poly"),
            inline = TRUE,
            selected = "pea" # Default to Pearson
          ),
          selectInput(
            ns("cfa_estimator_select"), # Changed ID
            label = "Select Estimator:",
            # Choices will be dynamically updated by the server
            choices = c("ML", "MLR", "GLS"), # Initial choices for Pearson
            selected = "ML"
          ),
          actionButton(ns("run_cfa_button"), "Run CFA Analysis", icon = icon("cogs")) # Changed ID
        )
      ),
      # Column for Path Diagram
      column(
        width = 7,
        box(
          title = "Step 2: Path Diagram",
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          em("The path diagram visually represents your specified model with estimated parameters (standardized loadings by default)."),
          br(),br(),
          plotOutput(ns("cfa_path_diagram_output"), height = "500px") %>% withSpinner(type = 8, color = "#728FCE"), # Changed ID
          br(),
          downloadButton(ns("download_path_diagram_button"), "Download Path Diagram (.svg)", icon = icon("download")) # Changed ID
        )
      )
    ),
    fluidRow(
      # Column for Fit Measures
      column(
        width = 11, # Or 12
        box(
          title = "Step 3: CFA Model Fit Measures",
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          em("Assess how well your specified model fits the observed data using various fit indices. Common cutoffs include: CFI/TLI > 0.90 (good > 0.95), RMSEA < 0.08 (good < 0.06), SRMR < 0.08."),
          br(),br(),
          tableOutput(ns("cfa_fit_measures_table")) %>% withSpinner(type = 8, color = "#728FCE"), # Changed ID
          br(),
          downloadButton(ns("download_fit_measures_button"), "Download Fit Measures (.csv)", icon = icon("download")) # Changed ID
        )
      )
    ),
    fluidRow(
      # Column for Factor Loadings
      column(
        width = 11,
        box(
          title = "Step 4: Standardized Factor Loadings",
          collapsible = TRUE,
          collapsed = TRUE, # Start collapsed as it can be a long table
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          em("Standardized factor loadings indicate the strength of the relationship between each item and its specified factor, ranging from -1 to 1. Also shown are standard errors, z-values, p-values, and confidence intervals for the loadings."),
          br(),br(),
          tableOutput(ns("cfa_factor_loadings_table")) %>% withSpinner(type = 8, color = "#728FCE"), # Changed ID
          br(),
          downloadButton(ns("download_factor_loadings_button"), "Download Factor Loadings (.csv)", icon = icon("download")) # Changed ID
        )
      )
    ),
    fluidRow(
      # Column for Modification Indices
      column(
        width = 11,
        box(
          title = "Step 5: Modification Indices (Top 20)",
          collapsible = TRUE,
          collapsed = TRUE, # Start collapsed
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          em("Modification indices (MI) suggest ways to improve model fit by adding paths (e.g., cross-loadings, correlated errors). Large MIs indicate parameters that, if freed, would significantly decrease the model chi-square. EPC (Expected Parameter Change) indicates the expected value of the parameter if it were freed."),
          br(),br(),
          tableOutput(ns("cfa_modification_indices_table")) %>% withSpinner(type = 8, color = "#728FCE") # Changed ID
          # No download for MIs for now, can be added if needed.
        )
      )
    )
  )
}
