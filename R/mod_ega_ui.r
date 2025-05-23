# Exploratory Graph Analysis (EGA) UI Module
 
ega_ui <- function(id) {
  ns <- NS(id) # Namespace function

  tagList(
    fluidRow(
      column(
        width = 11, # Or 12 for full width
        box(
          title = "Introduction to Exploratory Graph Analysis (EGA)",
          collapsible = TRUE,
          status = "info", # Using "info" for introductory boxes
          solidHeader = TRUE,
          width = NULL,
          strong(
            "Exploratory Graph Analysis (EGA) is a network psychometrics method used to estimate the number of dimensions (factors) in psychological data.",br(),
            "It works by estimating a network of items (nodes) and their relationships (edges), then applying a community detection algorithm to identify clusters of items, which are interpreted as dimensions."
          ),
          br(),
          em(
            "In this module, you can:",br(),
            "1. Select the network estimation method (e.g., GLASSO, TMFG).",br(),
            "2. Run the EGA to identify communities (dimensions).",br(),
            "3. View and download the resulting network structure and plot."
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 4, # Column for EGA setup
        box(
          title = "Step 1: EGA Setup",
          collapsible = TRUE,
          status = "primary", # "primary" for setup/action boxes
          solidHeader = TRUE,
          width = NULL,
          selectInput(
            ns("ega_estimation_method_select"), # Changed ID
            label = "Select Network Estimation Method:",
            choices = c(
              "Graphical LASSO (GLASSO)" = "glasso",
              "Triangulated Maximally Filtered Graph (TMFG)" = "TMFG"
            ),
            selected = "TMFG" # Default selection
          ),
          # Add other EGAnet specific parameters if needed, e.g., 'corr' type
          radioButtons(
            ns("ega_correlation_type_radio"),
            label = "Correlation type for EGA:",
            choices = c("Auto"= "cor_auto", "Pearson" = "pearson", "Spearman" = "spearman"), # EGAnet's 'cor_auto' handles this well
            selected = "cor_auto",
            inline = TRUE
          ),
          actionButton(ns("run_ega_button"), "Run EGA Analysis", icon = icon("project-diagram")) # Changed ID
        )
      ),
      column(
        width = 7, # Column for results
        box(
          title = "Step 2: EGA Network Plot",
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          em("The network plot visualizes items as nodes and their partial correlations as edges. Colors indicate identified communities (dimensions)."),
          br(),br(),
          plotOutput(ns("ega_network_plot_output"), width = "100%", height = "550px") %>% withSpinner(type = 8, color = "#728FCE"), # Changed ID
          br(),
          downloadButton(ns("download_ega_plot_button"), "Download Network Plot (.svg)", icon = icon("download")) # Changed ID
        )
      )
    ),
    fluidRow(
      column(
        width = 11,
        box(
          title = "Step 3: EGA Network Structure (Adjacency Matrix)",
          collapsible = TRUE,
          collapsed = TRUE, # Start collapsed as it can be large
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          em("The adjacency matrix represents the (weighted) connections between items in the estimated network. Non-zero values indicate an edge between items."),
          br(),br(),
          tableOutput(ns("ega_network_table_output")) %>% withSpinner(type = 8, color = "#728FCE"), # Changed ID
          br(),
          downloadButton(ns("download_ega_network_button"), "Download Network Matrix (.csv)", icon = icon("download")) # Changed ID
        ),
        box(
          title = "Step 4: EGA Dimensionality Results",
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          em("This section provides information on the number of dimensions identified by EGA and item-to-dimension assignments."),
          br(),br(),
          verbatimTextOutput(ns("ega_dimensionality_summary_output")) %>% withSpinner(type = 8, color = "#728FCE"), # For text summary
          br(),
          tableOutput(ns("ega_item_community_table_output")) %>% withSpinner(type =8, color = "#728FCE") # For item assignments
        )
      )
    )
  )
}
