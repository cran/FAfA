#' Data Selection UI Module
#'
#' Provides the user interface elements for data uploading, setting data options
#' (like header presence), displaying a data preview, and showing summary
#' statistics in infoBoxes.
#'
#' @param id A character string, the module id.
#'
#' @return A UI definition.
#' @import shiny
#' @importFrom shinydashboard box infoBoxOutput
#' @importFrom shinycssloaders withSpinner 
#' @noRd
data_selection_ui <- function(id) {
  ns <- NS(id) # Namespace function to ensure ID uniqueness

  tagList(
    fluidRow(
      # Left column for file upload and data preview
      column(
        width = 5, 
        box(
          title = "Data Upload Instructions",
          collapsible = TRUE,
          status = "primary", 
          solidHeader = TRUE,
          width = NULL, 
          strong(
            "Please upload your data file. Supported formats include .csv, .txt, .dat, .sav (SPSS), .xlsx, and .xls (Excel).", br(),
            "Ensure your data is properly formatted."
          ),
          br(),
          em(
            "Regarding missing values: Please ensure they are represented as NA in your dataset. ",
            "The application will attempt to identify and convert common missing value indicators to NA. ",
            "Subsequent analyses will typically use listwise deletion for rows with NA values."
          )
        ),
        box(
          title = "Select Your Data File & Options", # Box title updated
          collapsible = TRUE,
          status = "success", 
          solidHeader = TRUE,
          width = NULL,
          fileInput(
            ns("file1"),
            label = "Choose Data File:",
            accept = c(
              ".csv", ".txt", ".dat", # Text-based
              ".sav",                 # SPSS
              ".xlsx", ".xls"         # Excel
            ),
            placeholder = "Upload .csv, .txt, .dat, .sav, .xlsx, or .xls file"
          ),
          # New input for asking about header row
          checkboxInput(
            ns("has_header_checkbox"), 
            label = "My data file has a header row (variable names in the first row)", 
            value = TRUE # Default to TRUE, assuming headers are common
          ),
          em("Uncheck this if your .csv, .dat, or .txt file does NOT contain a header row. Excel and SPSS files typically handle headers automatically."),
          br(),br(),
          actionButton(ns("analyze_data"), "Analyze Data", icon = icon("play-circle"))
        ),
        box(
          title = "Data Preview (First 10 Rows)",
          collapsible = TRUE,
          status = "info", 
          solidHeader = TRUE,
          width = NULL,
          tableOutput(ns("mydatatable")) %>% withSpinner(type = 8, color = "#728FCE")
        )
      ),
      # Right column for displaying summary statistics using infoBoxes
      column(
        width = 7,
        box(
          title = "Summary Statistics",
          collapsible = TRUE,
          status = "info", 
          solidHeader = TRUE,
          width = NULL,
          fluidRow(
            infoBoxOutput(ns("n_var"), width = 6),
            infoBoxOutput(ns("n"), width = 6)
          ),
          fluidRow(
            infoBoxOutput(ns("min_value"), width = 6),
            infoBoxOutput(ns("max_value"), width = 6)
          ),
          fluidRow(
            infoBoxOutput(ns("num_cat"), width = 6)
          )
        )
      )
    )
  )
}
