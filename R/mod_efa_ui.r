# EFA UI Modules
 
# ---------------------------------------------------------------------------
# UI Module for Factor Retention Methods
# ---------------------------------------------------------------------------
efa_ui_fac_ret <- function(id) {
  ns <- NS(id) # Namespace function
  tagList(
    fluidRow(
      column(
        width = 11, # Or 12 for full width
        box(
          title = "Factor Retention Guidance",
          collapsible = TRUE,
          status = "info", # "primary", "success", "info", "warning", "danger"
          solidHeader = TRUE,
          width = NULL, # Auto width within the column
          strong(
            "In exploratory factor analysis, deciding on the number of factors is a critical step.", br(),
            "It is recommended to examine findings from multiple methods.", br(),
            "This section provides several techniques to help determine the optimal number of factors.", br(),
            "Please select your preferred methods using the options below."
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 11,
        box(
          title = "Choose Factor Retention Methods",
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          strong("Select the method(s) to guide your decision on the number of factors:"),
          selectInput(
            ns("dimension_methods"), # Namespaced input ID
            label = "Select Method:",
            choices = c(
              "Optimal Parallel Analysis (MRFA)" = "pa_mrfa",
              "Traditional Parallel Analysis" = "pa_traditional",
              "Hull Method (EFA)" = "hull_method",
              "Minimum Average Partial (MAP)" = "map_method_tra",
              "Revised Minimum Average Partial (MAP)" = "map_method_rev",
              "Exploratory Graph Analysis (TMFG)" = "EGA_tmfg",
              "Exploratory Graph Analysis (Glasso)" = "EGA_glasso",
              "Empirical Kaiser Criterion (EKC)" = "EK_C",
              "Comparison Data (CD)" = "comp_data_method"
            ),
            selected = "hull_method" # Default selected method
          ),
          actionButton(ns("run_factor_ret"), "Run Factor Retention Analysis", icon = icon("cogs"))
        )
      )
    ),
    fluidRow(
      column(
        width = 11,
        box(
          title = "Scree Plot",
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          plotOutput(ns("scree_plot")) %>% withSpinner(type = 8, color = "#728FCE"),
          br(),
          strong("Note:"), "A scree plot is a useful visual aid, but relying solely on it is not optimal. Please consider results from other selected methods."
        )
      )
    ),
    fluidRow(
      column(
        width = 11,
        box(
          title = "Results of Selected Method(s)",
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          tableOutput(ns("dim_ret_results")) %>% withSpinner(type = 8, color = "#728FCE")
        )
      )
    )
  )
}

# ---------------------------------------------------------------------------
# UI Module for Setting up Exploratory Factor Analysis (EFA)
# ---------------------------------------------------------------------------
efa_ui_analysis <- function(id) {
  ns <- NS(id) # Namespace function
  tagList(
    fluidRow(
      column(
        width = 11,
        box(
          title = "Exploratory Factor Analysis (EFA) Setup",
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          strong(
            "This section allows you to perform an Exploratory Factor Analysis.", br(),
            "Before running the analysis, you need to make several decisions, such as the number of factors to extract, the correlation matrix type, the factoring method, and the rotation method.", br(),
            "Use the 'Factor Retention' section to help decide on the number of factors.", br(),
            "If your data consists of items with 4 or fewer response categories, or if items are ordinal, using a polychoric correlation matrix is generally recommended."
          )
        )
      )
    ),
    fluidRow(
      # Grouping related inputs for better organization
      column(width = 4, # Adjust width as needed
        box(
          title = "Correlation Matrix",
          status = "primary", solidHeader = TRUE, width = NULL,
          radioButtons(
            ns("cor_kind"),
            label = "Select Correlation Type:",
            choices = c("Pearson" = "pea", "Polychoric" = "poly"),
            selected = "poly", # Default to polychoric as per guidance
            inline = TRUE
          )
        ),
        box(
          title = "Number of Factors",
          status = "primary", solidHeader = TRUE, width = NULL,
          numericInput(
            ns("number_factor"),
            label = "Enter Number of Factors to Extract:",
            value = 1, # Default value
            min = 1,   # Minimum number of factors
            step = 1,  # Increment step
            width = "100%" # Use full width of the box
          ),
          em("Refer to the 'Factor Retention' section for guidance.")
        )
      ),
      column(width = 4, # Adjust width
        box(
          title = "Factoring Method",
          status = "primary", solidHeader = TRUE, width = NULL,
          radioButtons( # Changed from selectInput for better visibility of few options
            ns("fact_method"),
            label = "Select Factoring Method:",
            choices = c(
              "Principal Axis (PA)" = "pa",
              "Minimum Residual (MinRes)" = "minres",
              "Unweighted Least Squares (ULS)" = "uls",
              "Generalized Least Squares (GLS)" = "gls", # Added GLS as a common option
              "Maximum Likelihood (ML)" = "ml",     # Added ML as a common option
              "Alpha Factoring" = "alpha", # Alpha is less common for item FA, more for scale construction
              "Principal Component Analysis (PCA)" = "pca" # PCA is technically not FA but often compared
            ),
            selected = "minres" # MinRes is often a good default
          )
        )
      ),
      column(width = 3, # Adjust width
        box(
          title = "Rotation Method",
          status = "primary", solidHeader = TRUE, width = NULL,
          selectInput(
            ns("rotating_method"),
            label = "Select Rotation Method:",
            choices = c(
              # Orthogonal Rotations
              "Varimax (Orthogonal)" = "varimax",
              "Quartimax (Orthogonal)" = "quartimax",
              "Equamax (Orthogonal)" = "equamax",
              "BentlerT (Orthogonal)" = "bentlerT",
              # Oblique Rotations
              "Oblimin (Oblique)" = "oblimin",
              "Promax (Oblique)" = "promax",
              "Simplimax (Oblique)" = "simplimax",
              "BentlerQ (Oblique)" = "bentlerQ",
              "GeominQ (Oblique)" = "geominQ",
              "BiQuartimin (Oblique)" = "biquartimin", # Added common oblique
              "Cluster (Oblique)" = "cluster", # Added common oblique
              # No rotation
              "None" = "none"
            ),
            selected = "oblimin" # Oblimin is a common default for oblique
          ),
          p(strong("Orthogonal methods:"), " Assume factors are uncorrelated."),
          p(strong("Oblique methods:"), " Allow factors to be correlated.")
        )
      )
    ),
    fluidRow(
      column(
        width = 11, # Or use a smaller width and center it
        box(
          title = "Run Analysis",
          status = "success", # "success" for action button
          solidHeader = TRUE,
          width = NULL, # Or specify a width like "300px" and wrap in a centered column
          div(style = "text-align: center;", # Center the button
              actionButton(ns("run_efa"), "Run EFA Analysis", icon = icon("play-circle"), class = "btn-lg")
          )
        )
      )
    )
  )
}

# ---------------------------------------------------------------------------
# UI Module for Reporting EFA Results
# ---------------------------------------------------------------------------
efa_ui_report <- function(id) {
  ns <- NS(id) # Namespace function
  tagList(
    fluidRow(
      column(
        width = 11,
        box(
          title = "Data Suitability for EFA: KMO & Bartlett's Test",
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          h4("Kaiser-Meyer-Olkin (KMO) Measure of Sampling Adequacy:"),
          em(
            "The KMO statistic varies between 0 and 1. A value close to 1 indicates that patterns of correlations are relatively compact and so factor analysis should yield distinct and reliable factors. Values less than 0.50 are generally considered unacceptable, suggesting that the sample size might be inadequate or the variables are too heterogeneous. Field (2009) suggests the following interpretations: >0.9 Marvelous, >0.8 Meritorious, >0.7 Middling, >0.6 Mediocre, >0.5 Miserable, <0.5 Unacceptable."
          ),
          htmlOutput(ns("kmo_result")) %>% withSpinner(type = 8, color = "#728FCE"), # For HTML formatted text
          br(),
          h4("Bartlett's Test of Sphericity:"),
          em(
            "This test checks whether the correlation matrix is significantly different from an identity matrix (i.e., variables are unrelated). If the test is statistically significant (p < 0.05), it suggests that the correlations between items are sufficiently large for EFA."
          ),
          tableOutput(ns("bartlett")) %>% withSpinner(type = 8, color = "#728FCE"),
          br(),
          h4("Individual MSA Values (from KMO):"),
          em(
            "MSA values for individual items below 0.50 indicate that the item does not share sufficient variance with other items in the factor solution and might be a candidate for removal (Lorenzo-Seva & Ferrando, 2021)."
          ),
          tableOutput(ns("robust_msa")) %>% withSpinner(type = 8, color = "#728FCE")
        )
      )
    ),
    fluidRow(
      column(
        width = 11,
        box(
          title = "Inter-Item Correlation Heatmap",
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          plotOutput(ns("heat_map"), width = "100%", height = "600px") %>% withSpinner(type = 8, color = "#728FCE"), # Adjusted for responsiveness
          strong("Note:"), "The heatmap displays the correlation matrix selected in the 'EFA Setup' section."
        )
      )
    ),
    fluidRow(
      column(
        width = 11,
        box(
          title = "EFA Results: Factor Loadings (Structure Matrix)",
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          em("Factor loadings indicate the strength and direction of the relationship between each item and the underlying factor. Higher absolute values suggest a stronger association. Communalities (h2) represent the proportion of variance in each item accounted for by the factors."),
          br(),br(),
          tableOutput(ns("efa_result_str")) %>% withSpinner(type = 8, color = "#728FCE"),
          br(),
          downloadButton(ns("download_efa_loadings"), label = "Download Factor Loadings (.csv)", icon = icon("download"))
        )
      )
    ),
    fluidRow(
      column(
        width = 11,
        box(
          title = "Inter-Factor Correlations (Phi Matrix for Oblique Rotations)",
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          em("If an oblique rotation method was chosen, this table shows the correlations between the extracted factors. For orthogonal rotations, factors are assumed to be uncorrelated (correlation matrix would be an identity matrix)."),
          br(),br(),
          tableOutput(ns("efa_result_interf_cor")) %>% withSpinner(type = 8, color = "#728FCE")
        )
      )
    ),
    fluidRow(
      column(
        width = 11,
        box(
          title = "Explained Variance by Factors",
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          em("This table shows how much variance is accounted for by each factor. Key metrics include 'SS loadings' (sum of squared loadings), 'Proportion Var' (proportion of total variance in all variables accounted for by each factor), and 'Cumulative Var' (cumulative proportion of variance explained by factors). 'Proportion Explained' and 'Cumulative Proportion' refer to the proportion of the *explained* variance if multiple factors are extracted."),
          br(),br(),
          tableOutput(ns("efa_result_expl_var")) %>% withSpinner(type = 8, color = "#728FCE")
        )
      )
    )
  )
}
