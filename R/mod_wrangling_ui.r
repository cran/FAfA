# Wrangling UI Modules
 
# mod_wrangling_ui.R dosyasının içeriği (varsa library() çağrıları silinmiş olarak)

# ---------------------------------------------------------------------------
# UI Module for Excluding Variables
# ---------------------------------------------------------------------------
#' UI Module for Excluding Variables
#'
#' Provides UI elements for users to specify and exclude variables from a dataset.
#'
#' @param id A character string, the module id.
#' @return A UI definition.
#' @import shiny
#' @importFrom shinydashboard box
#' @importFrom shinycssloaders withSpinner
#' @noRd
wrangling_ui_ex_var <- function(id) {
  ns <- NS(id) # Namespace function

  tagList(
    fluidRow(
      column(width = 6, # Adjust width as needed
        box(
          title = "Exclude Variables from Dataset",
          collapsible = TRUE,
          status = "primary", # Changed status for a more descriptive action
          solidHeader = TRUE,
          width = NULL, # Box will take the full width of the column
          strong("To exclude specific variables (columns) from your dataset, please enter their column numbers, separated by commas (e.g., 1,3,5). Please note that excluding variables here will immediately update your dataset, and subsequent analyses will be performed on the dataset without the excluded variables."),
          br(),br(),
          textInput(
            ns("excluded_variables_input"), # Changed ID slightly for clarity
            label = "Enter column numbers to exclude:",
            placeholder = "e.g., 1,3,5 or 2:4,6" # Added example for range
          ),
          actionButton(ns("exclude_button"), "Exclude Selected Variables", icon = icon("columns")), # Changed ID & icon
          br(), br(),
          em("After excluding variables, you can save the modified dataset."),
          br(),
          downloadButton(ns("download_excluded_data_button"), "Save Dataset (Variables Excluded)", icon = icon("download")) # Changed ID
        )
      )
    )
  )
}

# ---------------------------------------------------------------------------
# UI Module for Splitting Data
# ---------------------------------------------------------------------------
#' UI Module for Splitting Dataset
#'
#' Provides UI elements for users to split their dataset into two subsets.
#'
#' @param id A character string, the module id.
#' @return A UI definition.
#' @import shiny
#' @importFrom shinydashboard box
#' @noRd
wrangling_ui_split <- function(id) {
  ns <- NS(id) # Namespace function

  tagList(
    fluidRow(
      column(width = 6, # Adjust width
        box(
          title = "Split Dataset for Cross-Validation",
          collapsible = TRUE,
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          strong(
            "This function allows you to randomly split your current dataset into two subsets (e.g., for Exploratory Factor Analysis (EFA) and Confirmatory Factor Analysis (CFA) or for cross-validation purposes)."
          ),
          br(),
          sliderInput(ns("split_percentage_slider"), "Percentage for First Subset (e.g., EFA set):", min = 10, max = 90, value = 50, step = 5, post = "%"),
          actionButton(ns("split_data_button"), "Split Data", icon = icon("object-ungroup")), # Changed ID & icon
          br(), br(),
          em("Save the generated subsets:"),
          br(),
          downloadButton(ns("download_first_subset_button"), "Save First Subset", icon = icon("download")), # Changed ID
          downloadButton(ns("download_second_subset_button"), "Save Second Subset", icon = icon("download")) # Changed ID
        )
      )
    )
  )
}

# ---------------------------------------------------------------------------
# UI Module for Handling Outliers
# ---------------------------------------------------------------------------
#' UI Module for Handling Multivariate Outliers
#'
#' Provides UI elements for detecting and managing multivariate outliers.
#'
#' @param id A character string, the module id.
#' @return A UI definition.
#' @import shiny
#' @importFrom shinydashboard box
#' @importFrom shinycssloaders withSpinner
#' @noRd
wrangling_ui_outliers <- function(id) {
  ns <- NS(id) # Namespace function

  tagList(
    fluidRow(
      column(width = 7, # Adjust width
        box(
          title = "Manage Multivariate Outliers",
          collapsible = TRUE,
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          strong(
            "Detect multivariate outliers using Mahalanobis Distance (MD).",br(),
            "Observations with a Mahalanobis p-value less than a specified threshold (e.g., 0.001) are typically considered outliers."
          ),
          br(),br(),
          numericInput(ns("mah_p_value_threshold_input"), "P-value threshold for outlier detection (e.g., 0.001):", value = 0.001, min = 0.00001, max = 0.05, step = 0.0001),
          actionButton(ns("check_outliers_button"), "Identify Outliers", icon = icon("search")), # Changed ID
          br(), br(),
          h4("Detected Outliers:"),
          textOutput(ns("outlier_count_text")), # For displaying count
          tableOutput(ns("outliers_table")) %>% withSpinner(type = 8, color = "#728FCE"), # Changed ID
          br(),
          actionButton(ns("remove_outliers_button"), "Remove Identified Outliers", icon = icon("user-minus"), class = "btn-danger"), # Added class for styling
          br(), br(),
          em("Save the dataset after removing outliers:"),
          br(),
          downloadButton(ns("download_data_no_outliers_button"), "Save Data (Outliers Removed)", icon = icon("download")) # Changed ID
        )
      )
    )
  )
}
